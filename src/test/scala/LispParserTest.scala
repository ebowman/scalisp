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
  "A Lisp parser" should "add correctly" in {
    check {
      (x: List[Long]) =>
        x.size < 2 || parser.parse("(+ %s)".format(x.mkString(" "))).toLong == x.sum
    }
  }
  it should "subtract correctly" in {
    check {
      (x: List[Long]) =>
        x.size < 2 || parser.parse("(- %s)".format(x.mkString(" "))).toLong == x.head - x.tail.sum
    }
  }
  it should "multiply correctly" in {
    check {
      (x: List[Long]) =>
        x.size < 2 || parser.parse("(* %s)".format(x.mkString(" "))).toLong == x.product
    }
  }
  it should "divide correctly" in {
    check {
      (x: Long, y: Long) =>
        y == 0 || parser.parse("(/ %d %d)".format(x, y)).toLong == x / y
    }
  }
  it should "< correctly" in {
    check {
      (x: Long, y: Long) =>
        parser.parse("(< %d %d)".format(x, y)).toBoolean == (x < y)
    }
  }
  it should "> correctly" in {
    check {
      (x: Long, y: Long) =>
        parser.parse("(> %d %d)".format(x, y)).toBoolean == (x > y)
    }
  }
  it should ">= correctly" in {
    check {
      (x: Long, y: Long) =>
        parser.parse("(>= %d %d)".format(x, y)).toBoolean == (x >= y)
    }
  }
  it should "<= correctly" in {
    check {
      (x: Long, y: Long) =>
        parser.parse("(<= %d %d)".format(x, y)).toBoolean == (x <= y)
    }
  }
  it should "= correctly" in {
    check {
      (x: Long, y: Long) =>
        parser.parse("(= %d %d)".format(x, y)).toBoolean == (x == y)
    }
  }
  it should "if correctly" in {
    check {
      (x: Long, y: Long) =>
        parser.parse("(if (< %d %d) %d %d)".format(x, y, x, y)).toLong == math.min(x, y)
    }
  }
  it should "factorial correctly" in {
    parser.parse("""(defun fac (n) "factorial" (if (< n 2) n (* n (fac (- n 1)))))""")
    parser.parse("(fac 6)").toLong should equal(720l)
  }
  it should "fibonnaci correctly" in {

    parser.parse(
      """
        | (defun fib (n) "recursive"
        |   (if
        |     (< n 2)
        |     n
        |     (+ (fib (- n 1)) (fib (- n 2))))
        | )
        |
        | """.stripMargin)
    parser.parse("(fib 40)").toLong should equal(102334155L)
  }
}
