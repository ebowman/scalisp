import util.parsing.combinator.JavaTokenParsers

object Lisp extends LispParser {
  val dictionary = collection.mutable.Map[Var, Expression]()

  def parse(program: String): String = {
    parseAll(expression, program).toString.trim
  }
}

case class Env(vars: Map[Var, Expression])

trait Expression {
  def eval(env: Env): Expression

  def toDouble(env: Env): Double = eval(env).toDouble(env)

  def toBoolean(env: Env): Boolean = eval(env).toBoolean(env)

  def toString(env: Env): String = eval(env).toString(env)
}

trait ConcreteExpression extends Expression {
  def eval(env: Env): Expression = this

  override def toDouble(env: Env): Double =
    sys.error("Cannot convert %s to double".format(this))

  override def toBoolean(env: Env): Boolean =
    sys.error("Cannot convert %s to boolean".format(this))

  override def toString(env: Env): String
}

trait DoubleType extends Expression {
  override def toString(env: Env): String = toDouble(env).toString
}

trait BooleanType extends Expression {
  override def toString(env: Env): String = toBoolean(env).toString
}

case class Num(repr: String) extends ConcreteExpression with DoubleType {
  override def toDouble(env: Env) = repr.toDouble
}

case class Bool(repr: String) extends ConcreteExpression with BooleanType {
  override def toBoolean(env: Env): Boolean = "repr" match {
    case "true" => true
    case "false" => false
    case _ => sys.error("Cannot convert %s to boolean".format(repr))
  }
}

case class Str(repr: String) extends ConcreteExpression {
  override def toString(env: Env): String = repr.drop(1).take(repr.size - 2)
}

case class Var(repr: String) extends Expression {
  override def eval(env: Env): Expression = env.vars(this).eval(env)

  override def toDouble(env: Env): Double = env.vars(this).toDouble(env)

  override def toBoolean(env: Env): Boolean = env.vars(this).toBoolean(env)

  override def toString(env: Env): String = env.vars(this).toString(env)
}

case class Function(name: String, args: List[String], desc: String, expression: Expression) extends Expression {
  override def eval(env: Env): Expression =
    expression.eval(env)

  override def toDouble(env: Env): Double =
    eval(env).toDouble(env)

  override def toBoolean(env: Env): Boolean =
    eval(env).toBoolean(env)

  override def toString(env: Env): String =
    eval(env).toString(env)
}

trait LispParser extends JavaTokenParsers {
  override val whiteSpace = "[ \t\r\n]+".r

  def dictionary: collection.mutable.Map[Var, Expression]

  def number: Parser[Num] = (wholeNumber | decimalNumber | floatingPointNumber) ^^ {
    case n => Num(n)
  }

  def string: Parser[Str] = stringLiteral ^^ {
    case n => Str(n)
  }

  def variable: Parser[Var] = """[a-zA-Z][a-zA-Z0-9_-]*""".r ^^ (n => Var(n))

  def boolean: Parser[Bool] = """true|false""".r ^^ (b => Bool(b))

  def expr: Parser[Expression] =
    "defun" ~> (variable ~ ("(" ~> rep(variable) <~ ")") ~ string ~ expression) ^^ {
      case name ~ vars ~ desc ~ body =>
        val func = Function(name.repr, vars.map(_.repr), desc.repr, body)
        dictionary += name -> func
        func
    } |
      "<" ~> expression ~ expression ^^ {
        case left ~ right => new ConcreteExpression {
          override def toBoolean(env: Env): Boolean = {
            left.toDouble(env) < right.toDouble(env)
          }

          override def toString(env: Env) = toBoolean(env).toString
        }
      } |
      "if" ~> (expression ~ expression ~ expression) ^^ {
        case test ~ then ~ els => new Expression {
          override def eval(env: Env): Expression = {
            if (test.toBoolean(env)) {
              then
            } else {
              els
            }
          }
        }
      } |
      "+" ~> expression ~ rep1(expression) ^^ {
        case head ~ tail => new Expression with DoubleType {
          override def eval(env: Env): Expression =
            Num((head.toDouble(env) +: tail.map(_.toDouble(env))).sum.toString)
        }
      } |
      "-" ~> expression ~ rep1(expression) ^^ {
        case head ~ tail => new Expression with DoubleType {
          override def eval(env: Env): Expression = Num((head.toDouble(env) +: tail.map(_.toDouble(env) * -1d)).sum.toString)
        }
      } |
      variable ~ rep(expression) ^^ {
        case v ~ args => new Expression {
          override def eval(env: Env): Expression = {
            println("env = %s".format(env))
            val func = dictionary(v).asInstanceOf[Function]
            func.eval(env.copy(vars = env.vars ++ func.args.map(Var(_)).zip(args)))
          }
        }
      }

  def expression: Parser[Expression] = ("(" ~> expr <~ ")" | boolean | variable | string | number)

  def parenExpression: Parser[Expression] = "(" ~> expression <~ ")" | "(" ~> expr <~ ")"
}

object Driver extends App with LispParser {
  val dictionary = collection.mutable.Map[Var, Expression]()
    println(parseAll(parenExpression, "(< 1 2)").get.toBoolean(Env(dictionary.toMap)))
    println(parseAll(parenExpression, "(+ 1 2)").get.toDouble(Env(dictionary.toMap)))
    println(parseAll(parenExpression, "(- 1 2)").get.toDouble(Env(dictionary.toMap)))
    println(parseAll(parenExpression, "(\"hello\")").get.toString(Env(dictionary.toMap)))
  println(parseAll(parenExpression, """(defun fib (n) "recursive" (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"""))
  println(parseAll(parenExpression, """(fib 40)""").get.toDouble(Env(dictionary.toMap)))
}
