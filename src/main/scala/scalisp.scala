import util.parsing.combinator.JavaTokenParsers

object Lisp extends LispParser {
  val dictionary = collection.mutable.Map[Var, Expression]()

  def parse(program: String): String = {
    parseAll(parenExpression, program).toString.trim
  }
}

case class Env(vars: Map[Var, Expression])

trait Expression {
  def eval(implicit env: Env): Expression
}

trait ConcreteExpression extends Expression {
  def repr: String

  def eval(implicit env: Env): Expression = this

  override def toString = repr
}

case class Num(v: Long) extends ConcreteExpression {
  val repr = v.toString
}

case class Bool(b: Boolean) extends ConcreteExpression {
  val repr = b.toString
}

case class Str(s: String) extends ConcreteExpression {
  val repr = s.drop(1).take(s.size - 2)
}

case class Var(repr: String) extends Expression {
  override def eval(implicit env: Env): Expression = {
    env.vars(this).eval
  }
}

case class Function(name: String, args: List[String], desc: String, expression: Expression) extends Expression {

  override def eval(implicit env: Env): Expression = {
    val vars = env.vars.map {
      case (v: Var, e: Expression) if args.contains(v.repr) => v -> e.eval
      case (v: Var, e: Expression) => v -> e
    }
    expression.eval(env.copy(vars = vars))
  }
}

trait LispParser extends JavaTokenParsers {
  override val whiteSpace = "[ \t\r\n]+".r
  implicit def expr2long(expr: Expression)(implicit env: Env): Long = expr.eval(env).toString.toLong
  implicit def expr2bool(expr: Expression)(implicit env: Env): Boolean = expr.eval(env).toString.toBoolean

  def dictionary: collection.mutable.Map[Var, Expression]

  def number: Parser[Num] = wholeNumber ^^ (n => Num(n.toLong))

  def string: Parser[Str] = stringLiteral ^^ (s => Str(s))

  def variable: Parser[Var] = """[a-zA-Z][a-zA-Z0-9_-]*""".r ^^ (n => Var(n))

  def expr: Parser[Expression] =
    "defun" ~> (variable ~ ("(" ~> rep(variable) <~ ")") ~ string ~ expression) ^^ {
      case name ~ vars ~ desc ~ body =>
        dictionary += name -> Function(name.repr, vars.map(_.repr), desc.repr, body)
        dictionary(name)
    } |
      "<" ~> expression ~ expression ^^ {
        case left ~ right => new Expression {
          override def eval(implicit env: Env): Expression = Bool(left < right)
        }
      } |
      "if" ~> (expression ~ expression ~ expression) ^^ {
        case test ~ then ~ els => new Expression {
          override def eval(implicit env: Env): Expression =  if (test) then.eval else els.eval
        }
      } |
      "+" ~> expression ~ rep1(expression) ^^ {
        case head ~ tail => new Expression {
          override def eval(implicit env: Env): Expression = Num(((head: Long) +: tail.map(n => n: Long)).sum)
        }
      } |
      "-" ~> expression ~ rep1(expression) ^^ {
        case head ~ tail => new Expression {
          override def eval(implicit env: Env): Expression = Num(((head: Long) +: tail.map(n => (n: Long) * -1)).sum)
        }
      } |
      variable ~ rep(expression) ^^ {
        case fname ~ args => new Expression {
          override def eval(implicit env: Env): Expression = {
            val func = dictionary(fname).asInstanceOf[Function]
            val newEnv = env.copy(vars = env.vars ++ func.args.map(Var(_)).zip(args.map(_.eval)))
            func.eval(newEnv)
          }
        }
      }

  def expression: Parser[Expression] = ("(" ~> expr <~ ")" | variable | string | number)

  def parenExpression: Parser[Expression] = "(" ~> (variable | string | number ) <~ ")" | "(" ~> expr <~ ")"
}

object Driver extends App with LispParser {
  val dictionary = collection.mutable.Map[Var, Expression]()
  implicit def env = Env(dictionary.toMap)

  println(parseAll(parenExpression, """(defun fib (n) "recursive" (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"""))
  for (i <- 1 to 40) {
    println("%d -> %s".format(i, parseAll(parenExpression, "(fib %d)".format(i)).get.eval))
  }

  println(parseAll(parenExpression, """(fib 10)""").get.eval)
  val start = System.currentTimeMillis()
  println(parseAll(parenExpression, """(fib 40)""").get.eval)
  val finish = System.currentTimeMillis()
  println("elapsed = %d".format(finish - start))
}
