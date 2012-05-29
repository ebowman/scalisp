import util.parsing.combinator.JavaTokenParsers

object Lisp extends LispParser {
  val dictionary = collection.mutable.Map[Var, Expression]()

  implicit def env = Env(dictionary.toMap)

  def parse(program: String): String = {
    val result = parseAll(sExpression, program.trim)
    if (result.successful) {
      result.get.eval.toString
    } else {
      sys.error(result.toString)
    }
  }
}

case class Env(vars: Map[Var, Expression])

trait Expression {
  def eval(implicit env: Env): Expression
}

trait EvaluatedExpression extends Expression {
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

case class Str(s: String) extends EvaluatedExpression {
  val repr = s.drop(1).take(s.size - 2)
}

case class Var(repr: String) extends Expression {
  override def eval(implicit env: Env): Expression = {
    env.vars(this).eval
  }
}

case class Function(name: String, args: List[String], desc: String, expression: Expression) extends Expression {

  // since Lisp is pure (no side effects), a function can memoize its results
  val memo = collection.mutable.Map[Env, Expression]()

  override def eval(implicit env: Env): Expression = {
    memo.getOrElseUpdate(env, {
      // everything that is a variable this function depends on, needs to be
      // evaluated before this function can evaluate. Other variables do not
      // need to evaluate. pretty slow without this!
      val vars = env.vars.map {
        case (v: Var, e: Expression) if args.contains(v.repr) => v -> e.eval
        case (v: Var, e: Expression) => v -> e
      }
      expression.eval(env.copy(vars = vars))
    })
    memo(env)
  }
}

trait LispParser extends JavaTokenParsers {
  implicit def expr2long(expr: Expression)(implicit env: Env): Long = expr.eval(env).toString.toLong

  implicit def expr2bool(expr: Expression)(implicit env: Env): Boolean = expr.eval(env).toString.toBoolean

  def dictionary: collection.mutable.Map[Var, Expression]

  def number: Parser[Num] = wholeNumber ^^ (n => Num(n.toLong))

  def string: Parser[Str] = stringLiteral ^^ (s => Str(s))

  def variable: Parser[Var] = """[a-zA-Z][a-zA-Z0-9_-]*""".r ^^ (n => Var(n))

  def function: Parser[Expression] =
    "defun" ~> (variable ~ ("(" ~> rep(variable) <~ ")") ~ string ~ nExpression) ^^ {
      case name ~ vars ~ desc ~ body =>
        dictionary += name -> Function(name.repr, vars.map(_.repr), desc.repr, body)
        Str("\"defined %s\"".format(name))
    } |
    "<" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Expression {
        override def eval(implicit env: Env): Expression = Bool(left < right)
      }
    } |
    "=" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Expression {
        override def eval(implicit env: Env): Expression = Bool(left.eval.toString == right.eval.toString)
      }
    } |
    ">" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Expression {
        override def eval(implicit env: Env): Expression = Bool(left > right)
      }
    } |
    ">=" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Expression {
        override def eval(implicit env: Env): Expression = Bool(left >= right)
      }
    } |
    "<=" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Expression {
        override def eval(implicit env: Env): Expression = Bool(left <= right)
      }
    } |
    "if" ~> nExpression ~ nExpression ~ nExpression ^^ {
      case test ~ then ~ els => new Expression {
        override def eval(implicit env: Env): Expression = if (test) then.eval else els.eval
      }
    } |
    "+" ~> nExpression ~ rep1(nExpression) ^^ {
      case head ~ tail => new Expression {
        override def eval(implicit env: Env): Expression = Num(((head: Long) +: tail.map(n => n: Long)).sum)
      }
    } |
    "-" ~> nExpression ~ rep1(nExpression) ^^ {
      case head ~ tail => new Expression {
        override def eval(implicit env: Env): Expression = Num(((head: Long) +: tail.map(n => (n: Long) * -1)).sum)
      }
    } |
    "*" ~> nExpression ~ rep1(nExpression) ^^ {
      case head ~ tail => new Expression {
        override def eval(implicit env: Env): Expression = Num(((head: Long) +: tail.map(n => (n: Long))).product)
      }
    } |
    "/" ~> nExpression ~ nExpression ^^ {
      case head ~ tail => new Expression {
        override def eval(implicit env: Env): Expression = Num(head / tail)
      }
    } |
    variable ~ rep(nExpression) ^^ {
      case fname ~ args => new Expression {
        override def eval(implicit env: Env): Expression = {
          val func = dictionary(fname).asInstanceOf[Function]
          val newEnv = env.copy(vars = env.vars ++ func.args.map(Var(_)).zip(args.map(_.eval)))
          func.eval(newEnv)
        }
      }
    }

  def nExpression: Parser[Expression] = ("(" ~> function <~ ")" | variable | string | number)

  def sExpression: Parser[Expression] = "(" ~> (function | variable | string | number) <~ ")"
}

object Driver extends App {
  import Lisp.parse
  println(parse(
    """
      | (defun fib (n) "recursive"
      |   (if
      |     (< n 2)
      |     n
      |     (+ (fib (- n 1)) (fib (- n 2))))
      | )
      |
      | """.stripMargin))
  println(parse("(fib 40)"))

  println(parse("""(defun fac (n) "factorial" (if (< n 2) n (* n (fac (- n 1)))))"""))
  println(parse("(fac 6)"))
}
