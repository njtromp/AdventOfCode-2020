package nl.njtromp.adventofcode

// Thanks to: https://gist.github.com/IvanProdaiko94/663776739b9b577f949bfb4818936585

//  Ex: Complete the Rational class with an add(r:Rational):Rational
//  function.

//  Ex: Redefine the toString method of the Rational class.

//  Ex: Redefine equals method of the Rational class.

//  Ex: Add sub, div, mul methods

//  Ex: Make sure that rational numbers that are created are optimized e.g. 6/12 is optimized into 1/2.
//      Hint: use gcd function we've seen in lectures.

//  Ex: Define toDouble method to return real approximate value of the number.

//  Ex: Operators: <, >, ==, /, *, +, -. Glance through https://docs.scala-lang.org/tour/operators.html.
//      Remember you can operate with functions as values to define an 'alias' operators for already defined methods.
//      To get a function value from a method, you can use '_' syntax, e.g. `isNull _`

//  Ex (Optional): Define companion object Rational with apply as alternative constructor for rationals.

//  Ex: Write a few tests using ScalaTest framework covering basic use cases.

case class Rational(n: Int, d: Int){
  val num: Int = n
  val den: Int = d
  def isNull: Boolean = num == 0

  def toDouble: Double = if den == 0 then num else num / den

  private def to2Pi(a: Double): Double =
    if a < 0 then 2 * Math.PI + a else a
  def angle: Double = to2Pi(Math.atan2(den, num))
  override def toString: String = if (isNull) "0" else s"($num/$den)"

  def add(r:Rational):Rational =
    Rational.calculate(this, r, (x:Int, y:Int) => x + y)

  def sub(r:Rational): Rational =
    Rational.calculate(this, r, (x:Int, y:Int) => x - y)

  def mul(r:Rational): Rational =
    Rational.findGCD(Rational(num * r.num, den * r.den))

  def div(r:Rational): Rational =
    mul(Rational(r.den, r.num))

  def +(r:Rational): Rational = add(r)
  def -(r:Rational): Rational = sub(r)
  def *(r:Rational): Rational = mul(r)
  def /(r:Rational): Rational = div(r)
  def ==(r:Rational): Boolean = sub(r).isNull
  def >(r:Rational): Boolean = (this - r).num > 0
  def <(r:Rational): Boolean = (this - r).num < 0
  def ===(r: Rational): Boolean = (den == 0 && r.den == 0 && num.sign == r.num.sign)
    || (sub(r).isNull
    && den.sign == r.den.sign
    && num.sign == r.num.sign)
  def !==(r: Rational): Boolean = !(this === r)
}

object Rational {
  def fromString(x: String): Rational = {
    val l: List[String] = x.split("/").toList
    l match {
      case n::m::Nil => Rational(n.toInt, m.toInt)
      case _ => throw new Error("rational number string should be in form `Int/Int`")
    }
  }

  private def findGCD(x:Rational): Rational = {
    val gcd = Rational.gcd(x.num, x.den)
    Rational(x.num / gcd, x.den / gcd)
  }

  private def gcd(a: Int,b: Int): Int = {
    math.abs(if(b == 0) a else gcd(b, a%b))
  }

  private def calculate(x: Rational, y: Rational, fn:(Int, Int) => Int): Rational = {
    if (x.den == y.den) {
      return Rational(fn(x.num, y.num), x.den)
    }
    val newDenominator = x.den * y.den
    val m = x.num * y.den
    val n = y.num * x.den
    val num = fn(m, n)
    Rational.findGCD(Rational(num, newDenominator))
  }
}