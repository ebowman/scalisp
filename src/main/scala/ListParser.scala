import util.parsing.combinator.JavaTokenParsers

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

case class Str(repr: String) extends EvaluatedExpression

case class Var(repr: String) extends Expression {
  override def eval(implicit env: Env): Expression = env.vars(this).eval
}

case class Function(name: String, args: List[String], desc: String, expression: Expression) extends Expression {

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

  implicit def env = Env(dictionary.toMap)

  def parse(program: String): String = {
    val result = parseAll(sExpression, program.trim)
    if (result.successful) {
      result.get.eval.toString
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
        dictionary += name -> Function(name.repr, vars.map(_.repr), desc.repr, body)
        Str("defined %s".format(name))
    } |
    "<" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Expression {
        override def eval(implicit env: Env): Expression = left < right
      }
    } |
    "=" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Expression {
        override def eval(implicit env: Env): Expression = (left: String) == (right: String)
      }
    } |
    ">" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Expression {
        override def eval(implicit env: Env): Expression = left > right
      }
    } |
    ">=" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Expression {
        override def eval(implicit env: Env): Expression = left >= right
      }
    } |
    "<=" ~> nExpression ~ nExpression ^^ {
      case left ~ right => new Expression {
        override def eval(implicit env: Env): Expression = left <= right
      }
    } |
    "if" ~> nExpression ~ nExpression ~ nExpression ^^ {
      case test ~ then ~ els => new Expression {
        override def eval(implicit env: Env): Expression = if (test) then.eval else els.eval
      }
    } |
    "+" ~> nExpression ~ rep1(nExpression) ^^ {
      case head ~ tail => new Expression {
        override def eval(implicit env: Env): Expression = ((head: Long) +: tail.map(n => n: Long)).sum
      }
    } |
    "-" ~> nExpression ~ rep1(nExpression) ^^ {
      case head ~ tail => new Expression {
        override def eval(implicit env: Env): Expression = ((head: Long) +: tail.map(n => (n: Long) * -1)).sum
      }
    } |
    "*" ~> nExpression ~ rep1(nExpression) ^^ {
      case head ~ tail => new Expression {
        override def eval(implicit env: Env): Expression = ((head: Long) +: tail.map(n => (n: Long))).product
      }
    } |
    "/" ~> nExpression ~ nExpression ^^ {
      case head ~ tail => new Expression {
        override def eval(implicit env: Env): Expression = head / tail
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

object Driver extends App with LispParser {
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
