import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 5/30/12 8:47 AM
 */

class LispParserTest extends FlatSpec with ShouldMatchers with Checkers {

  val parser = new LispParser {}
  import parser.parse

  "A Lisp parser" should "add correctly" in {
    check {
      (x: List[Long]) =>
        x.size < 2 || parse("(+ %s)".format(x.mkString(" "))).toLong == x.sum
    }
  }
  it should "subtract correctly" in {
    check {
      (x: List[Long]) =>
        x.size < 2 || parse("(- %s)".format(x.mkString(" "))).toLong == x.head - x.tail.sum
    }
  }
  it should "multiply correctly" in {
    check {
      (x: List[Long]) =>
        x.size < 2 || parse("(* %s)".format(x.mkString(" "))).toLong == x.product
    }
  }
  it should "divide correctly" in {
    check {
      (x: Long, y: Long) =>
        y == 0 || parse("(/ %d %d)".format(x, y)).toLong == x / y
    }
  }
  it should "< correctly" in {
    check {
      (x: Long, y: Long) =>
        parse("(< %d %d)".format(x, y)).toBoolean == (x < y)
    }
  }
  it should "> correctly" in {
    check {
      (x: Long, y: Long) =>
        parse("(> %d %d)".format(x, y)).toBoolean == (x > y)
    }
  }
  it should ">= correctly" in {
    check {
      (x: Long, y: Long) =>
        parse("(>= %d %d)".format(x, y)).toBoolean == (x >= y)
    }
  }
  it should "<= correctly" in {
    check {
      (x: Long, y: Long) =>
        parse("(<= %d %d)".format(x, y)).toBoolean == (x <= y)
    }
  }
  it should "= correctly" in {
    check {
      (x: Long, y: Long) =>
        parse("(= %d %d)".format(x, y)).toBoolean == (x == y)
    }
  }
  it should "if correctly" in {
    check {
      (x: Long, y: Long) =>
        parse("(if (< %d %d) %d %d)".format(x, y, x, y)).toLong == math.min(x, y)
    }
  }
  it should "factorial correctly" in {
    parse("""(defun fac (n) "factorial" (if (< n 2) n (* n (fac (- n 1)))))""")
    parse("(fac 6)").toLong should equal(720l)
  }
  it should "fibonnaci correctly" in {

    parse(
      """
        | (defun fib (n) "recursive"
        |   (if
        |     (< n 2)
        |     n
        |     (+ (fib (- n 1)) (fib (- n 2))))
        | )
        |
        | """.stripMargin)
    parse("(fib 40)").toLong should equal(102334155L)
  }

  it should "support passing a function" in {
    parse("""(defun add (x y) "" (+ x y))""")
//    parse("(add 2 3)") should equal ("5")
    parse("""(defun high (x y f) "" (f x y))""")
    parse("(high 2 3 add)") should equal("5")
  }
}
