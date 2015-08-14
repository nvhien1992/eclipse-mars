package scala.math
import java.io;

class Rational(initial_numer: Int = 1, initial_denom: Int = 1) {
  require(initial_denom != 0, "denom must be non-zero")

  private val gcd = {
    def gcd_recur(x: Int, y: Int): Int = {
      if (y == 0) x else gcd_recur(y, x % y)
    }
    abs(gcd_recur(initial_numer, initial_denom))
  }

  val numer: Int = (if (initial_denom < 0) { if (initial_numer < 0) initial_numer else -initial_numer } else initial_numer) / gcd
  val denom: Int = (abs(initial_denom)) / gcd

  def unary_- = new Rational(-numer, denom)

  def +(other: Int): Rational = new Rational(numer + other * denom, denom)
  def *(other: Rational): Rational = new Rational(numer * other.numer, denom * other.denom)
  def *(other: Int): Rational = new Rational(numer * other, denom)

  override def toString = numer + "/" + denom
}

object Test extends Rational {
  def main(args: Array[String]): Unit = {
    val r1 = new Rational(-2, 4)
    val r2 = new Rational(2, 8)
    val r3 = new Rational(1, 5)
    println(r1.toString())
    println(r2.toString())
    println(r3.toString())

    println((r1 * r2).toString())
    println((r1 * 3).toString())
    println((-r1).toString())
    println((r3 + 4).toString())

    val x = new Rational(3)
    println(x.toString())
    val y = new Rational
    println(y.toString())
  }
}
