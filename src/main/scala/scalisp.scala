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
}

trait ConcreteExpression extends Expression {
  def repr: String
  def eval(env: Env): Expression = this
  override def toString = repr
}

case class Num(v: Double) extends ConcreteExpression {
  val repr = v.toString
}

case class Bool(b: Boolean) extends ConcreteExpression {
  val repr = b.toString
}

case class Str(s: String) extends ConcreteExpression {
  val repr = s.drop(1).take(s.size - 2)
}

case class Var(repr: String) extends Expression {
  override def eval(env: Env): Expression = {
//    println("Evaluting %s with %s".format(this, env))
    val resolved = env.vars(this)
    resolved.eval(env)
  }
}

case class Function(name: String, args: List[String], desc: String, expression: Expression) extends Expression {
  override def eval(env: Env): Expression = {

    val vars = env.vars.map {
      case (v: Var, e: Expression) if args.contains(v.repr) =>
        v -> e.eval(env)
      case (v: Var, e: Expression) =>
        v -> e
    }

    expression.eval(env.copy(vars = vars))
  }

  override def toString = "%s (%s) \"%s\" => %s".format(name, args, desc, expression)
}

trait LispParser extends JavaTokenParsers {
  override val whiteSpace = "[ \t\r\n]+".r

  def dictionary: collection.mutable.Map[Var, Expression]

  def number: Parser[Num] = (wholeNumber | decimalNumber | floatingPointNumber) ^^ {
    case n => Num(n.toDouble)
  }

  def string: Parser[Str] = stringLiteral ^^ {
    case n => Str(n)
  }

  def variable: Parser[Var] = """[a-zA-Z][a-zA-Z0-9_-]*""".r ^^ (n => Var(n))

  def boolean: Parser[Bool] = """true|false""".r ^^ (b => Bool(b.toBoolean))

  def expr: Parser[Expression] =
    "defun" ~> (variable ~ ("(" ~> rep(variable) <~ ")") ~ string ~ expression) ^^ {
      case name ~ vars ~ desc ~ body =>
        val func = Function(name.repr, vars.map(_.repr), desc.repr, body)
        dictionary += name -> func
        func
    } |
      "<" ~> expression ~ expression ^^ {
        case left ~ right => new Expression {
          override def eval(env: Env): Expression = {
//            println("evaluating %s < %s with %s".format(left, right, env))
            val leftEval = left.eval(env)
            val rightEval = right.eval(env)
            val result = Bool(leftEval.toString.toDouble < rightEval.toString.toDouble)
//            println("result (<) = %s".format(result))
            result
          }
          override def toString = "< %s %s".format(left, right)
        }
      } |
      "if" ~> (expression ~ expression ~ expression) ^^ {
        case test ~ then ~ els => new Expression {
          override def eval(env: Env): Expression = {
//            println("evaluating if (%s) %s else %s with %s".format(test, then, els, env))
            val result = if (test.eval(env).toString.toBoolean) {
              then.eval(env)
            } else {
              els.eval(env)
            }
//            println("result (if) = %s".format(result))
            result
          }
          override def toString = "if (%s) %s else %s".format(test, then, els)
        }
      } |
      "+" ~> expression ~ rep1(expression) ^^ {
        case head ~ tail => new Expression {
          override def eval(env: Env): Expression = {
//            println("Evaluating %s + %s with %s".format(head, tail, env))
            val result = Num((head.eval(env).toString.toDouble +: tail.map(_.eval(env).toString.toDouble)).sum)
//            println("result (+) = %s".format(result))
            result
          }
          override def toString = "+ %s :: %s".format(head, tail)
        }
      } |
      "-" ~> expression ~ rep1(expression) ^^ {
        case head ~ tail => new Expression {
          override def eval(env: Env): Expression = {
//            println("Evaluating - on %s -> %s with %s".format(head, tail, env))
            val result = Num((head.eval(env).toString.toDouble +: tail.map(_.eval(env).toString.toDouble * -1d)).sum)
//            println("result (-) = %s".format(result))
            result
          }
          override def toString = "- %s :: %s".format(head, tail)
        }
      } |
      variable ~ rep(expression) ^^ {
        case fname ~ args => new Expression {
          override def eval(env: Env): Expression = {
//            println("Evaluating function %s(%s) with %s".format(fname, args, env))
            val func = dictionary(fname).asInstanceOf[Function]
            val newEnv = env.copy(vars = env.vars ++ func.args.map(Var(_)).zip(args.map(_.eval(env))))
//            println("newEnv = %s".format(newEnv))
            func.eval(newEnv)
          }
          override def toString = "fn: %s (%s)".format(fname, args)
        }
      }

  def expression: Parser[Expression] = ("(" ~> expr <~ ")" | boolean | variable | string | number)

  def parenExpression: Parser[Expression] = "(" ~> expression <~ ")" | "(" ~> expr <~ ")"
}

object Driver extends App with LispParser {
  val dictionary = collection.mutable.Map[Var, Expression]()
  //    println(parseAll(parenExpression, "(< 1 2)").get.toBoolean(Env(dictionary.toMap)))
  //    println(parseAll(parenExpression, "(+ 1 2)").get.toDouble(Env(dictionary.toMap)))
  //    println(parseAll(parenExpression, "(- 1 2)").get.toDouble(Env(dictionary.toMap)))
  //    println(parseAll(parenExpression, "(\"hello\")").get.toString(Env(dictionary.toMap)))
//  println(parseAll(parenExpression, """(defun r1 (n) "silly" (if (< n 1) n (r1 (- n 1))))"""))
//  println("answer: " + parseAll(parenExpression, """(r1 1)""").get.eval(Env(dictionary.toMap)))
  println(parseAll(parenExpression, """(defun fib (n) "recursive" (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"""))
  println(parseAll(parenExpression, """(fib 40)""").get.eval(Env(dictionary.toMap)))
}
