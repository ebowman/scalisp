import jline.ConsoleReader
import scala.util.parsing.combinator.JavaTokenParsers

case class Env(vars: Map[Var, Expression])

abstract class Expression(label: String) {
  def eval(implicit env: Env): Expression
  override def toString = label
}

abstract class EvaluatedExpression extends Expression("") {
  def repr: String

  def eval(implicit env: Env): Expression = this

  override def toString = repr
}

case class Num(v: Long) extends EvaluatedExpression {
  val repr = v.toString
}

case class Bool(b: Boolean) extends EvaluatedExpression {
  val repr = b.toString
}

case class Str(repr: String) extends EvaluatedExpression

case class Var(repr: String) extends Expression("Var(%s)".format(repr)) {
  override def eval(implicit env: Env): Expression = env.vars(this).eval
}

abstract class Function(repr: String, val args: List[String] = List.empty) extends Expression(repr)

case class DefFunction(name: String, override val args: List[String], desc: String, expression: Expression) extends Function("(%s %s)".format(name, args.mkString(" "))) {

  // since Lisp is pure (no side effects), a function can memoize its results.
  // pretty slow without this.
  val memo = collection.mutable.Map[Map[Var, Expression], Expression]()

  override def eval(implicit env: Env): Expression = {
    val varsInThisFunction = env.vars.collect {
      case (v: Var, e: Expression) if args.contains(v.repr) => v -> e.eval
    }
    memo.getOrElseUpdate(varsInThisFunction, {
      expression.eval(env.copy(vars = env.vars ++ varsInThisFunction))
    })
    memo(varsInThisFunction)
  }
}

trait LispParser extends JavaTokenParsers {
  val dictionary = collection.mutable.Map[Var, Expression]()

  def globalEnv = Env(dictionary.toMap)

  def parse(program: String): String = {
    val result = parseAll(sExpression, program.trim)
    if (result.successful) {
      result.get.eval(globalEnv).toString
    } else {
      sys.error(result.toString)
    }
  }

  implicit def expr2long(expr: Expression)(implicit env: Env): Long = expr.eval(env).toString.toLong

  implicit def expr2bool(expr: Expression)(implicit env: Env): Boolean = expr.eval(env).toString.toBoolean

  implicit def expr2str(expr: Expression)(implicit env: Env): String = expr.eval(env).toString

  implicit def bool2Bool(bool: Boolean): Bool = Bool(bool)

  implicit def long2Num(num: Long): Num = Num(num)

  def number: Parser[Num] = wholeNumber ^^ (n => n.toLong)

  def string: Parser[Str] = stringLiteral ^^ (s => Str(s.drop(1).take(s.length - 2)))

  def variable: Parser[Var] = """[a-zA-Z][a-zA-Z0-9_-]*""".r ^^ (n => Var(n))

  def function: Parser[Expression] =
    "defun" ~> (variable ~ ("(" ~> rep(variable) <~ ")") ~ string ~ nExpression) ^^ {
      case name ~ vars ~ desc ~ body =>
        dictionary += name -> DefFunction(name.repr, vars.map(_.repr), desc.repr, body)
        Str("defined %s".format(name))
    } |
    "<" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Function("(< %s %s)".format(left, right)) {
        override def eval(implicit env: Env): Expression = left < right
      }
    } |
    "=" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Function("(= %s %s)".format(left, right)) {
        override def eval(implicit env: Env): Expression = (left: String) == (right: String)
      }
    } |
    ">" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Function("(> %s %s)".format(left, right)) {
        override def eval(implicit env: Env): Expression = left > right
      }
    } |
    ">=" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Function("(>= %s %s)".format(left, right)) {
        override def eval(implicit env: Env): Expression = left >= right
      }
    } |
    "<=" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Function("(<= %s %s)".format(left, right)) {
        override def eval(implicit env: Env): Expression = left <= right
      }
    } |
    "if" ~> nExpression ~ nExpression ~ nExpression ^^ {
      case test ~ then ~ els => new Function("(if %s then %s else %s)".format(test, then, els)) {
        override def eval(implicit env: Env): Expression = if (test) then.eval else els.eval
      }
    } |
    "+" ~> nExpression ~ rep1(nExpression) ^^ {
      case head ~ tail => new Function("(+ %s %s)".format(head, tail.mkString(" "))) {
        override def eval(implicit env: Env): Expression = ((head: Long) +: tail.map(n => n: Long)).sum
      }
    } |
    "-" ~> nExpression ~ rep1(nExpression) ^^ {
      case head ~ tail => new Function("(- %s %s)".format(head, tail.mkString(" "))) {
        override def eval(implicit env: Env): Expression = ((head: Long) +: tail.map(n => (n: Long) * -1)).sum
      }
    } |
    "*" ~> nExpression ~ rep1(nExpression) ^^ {
      case head ~ tail => new Function("(* %s %s)".format(head, tail.mkString(" "))) {
        override def eval(implicit env: Env): Expression = ((head: Long) +: tail.map(n => (n: Long))).product
      }
    } |
    "/" ~> nExpression ~ nExpression ^^ {
      case num ~ denom => new Function("( %s %s)".format(num, denom)) {
        override def eval(implicit env: Env): Expression = num / denom
      }
    } |
    (("lamba" ~ "(") ~> rep1(variable) <~ ")") ~ nExpression ^^ {
      case parseArgs ~ nExpression =>
        new Function("(lambda (%s) %s)".format(parseArgs.map(_.repr).mkString(" "), nExpression), parseArgs.map(_.repr)) {
          def eval(implicit env: Env): Expression = {
            if (nExpression.isInstanceOf[Function]) {
              val func = nExpression.asInstanceOf[Function]
              val newEnv = envFromVars(func.asInstanceOf[Function].args, parseArgs)
              // if all the args are non-vars, or vars in the environment, eval; otherwise, don't
              if (parseArgs.forall((arg: Expression) => !arg.isInstanceOf[Var] ||
                newEnv.vars.contains(arg.asInstanceOf[Var]))) {
                func.eval(newEnv)
              } else {
                func
              }
            } else {
              nExpression.eval
            }
          }
        }
    } |
    variable ~ rep(nExpression) ^^ {
      case fname ~ args => new Expression("(%s %s)".format(fname, args.mkString(" "))) {
        override def eval(implicit env: Env): Expression = {
          var func = env.vars(fname)
          if (func.isInstanceOf[Function]) {
            val newEnv = envFromVars(func.asInstanceOf[Function].args, args)
            // if all the args are in the environment, eval; otherwise, don't
            if (args.forall((arg: Expression) => !arg.isInstanceOf[Var] ||
                newEnv.vars.contains(arg.asInstanceOf[Var]))) {
              func.eval(newEnv)
            } else {
              func
            }
          } else {
            func.eval
          }
        }
      }
    }

  def nExpression: Parser[Expression] = "(" ~> function <~ ")" | variable | string | number

  def sExpression: Parser[Expression] = "(" ~> (function | variable | string | number) <~ ")"

  def envFromVars(argVars: Seq[String], argVals: Seq[Expression])(implicit env: Env): Env = {
    env.copy(vars = env.vars ++ argVars.map(Var(_)).zip(argVals.map {
      case v: Var => v
      case a: Expression => a.eval
    }))
  }
}

object Driver extends App with LispParser {
  val reader = new ConsoleReader()
  reader.setBellEnabled(false)
  var line = reader.readLine("> ")
  while (line != null) {
    try {
      val start = System.currentTimeMillis
      val result = parse(line)
      val elapsed = System.currentTimeMillis - start
      println("(%d ms) %s".format(elapsed, result))
    } catch {
      case e => println(e.getMessage)
    }
    line = reader.readLine("> ")
  }
  println("bye")
}
