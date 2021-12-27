package nl.njtromp.adventofcode_2021

object Day24Nomad extends App {
  def monads: List[(Long, Long) => Long] = List(monad0, monad3, monad4, monad5, monad6, monad7, monad8, monad9, monad10, monad11, monad12, monad13)
  type NomadInfo = Map[(Long, Long), Long]

  val digits = 1L to 9L

  val curry = Long.MaxValue - 25
//  digits.foreach(d => println(s"($curry, $d) ${monad13(curry, d)}"))
  digits.foreach(d => digits.foreach(w => {
    var x = if (d == w) 1 else 0
    x = if (x == 0) 1 else 0
    println(s"$d, $w => $x")
  }))

  // Monads
  def monad0(curry: Long, input: Long): Long = {
    val w = input
    var x = 0L
    var z = curry
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (1 < 0) return 0;
    z = z / 1
    x = x + 12
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    var y = 0L
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 6
    y = y * x
    z = z + y
    z
  }

  def monad1(curry: Long, input: Long): Long = {
    val w = input
    var x = 0L
    var z = curry
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (1 < 0) return 0;
    z = z / 1
    x = x + 11
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    var y = 0L
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 12
    y = y * x
    z = z + y
    z
  }

  def monad2(curry: Long, input: Long): Long = {
    val w = input
    var x = 0L
    var z = curry
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (1 < 0) return 0;
    z = z / 1
    x = x + 10
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    var y = 0L
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 5
    y = y * x
    z = z + y
    z
  }

  def monad3(curry: Long, input: Long): Long = {
    val w = input
    var x = 0L
    var z = curry
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (1 < 0) return 0;
    z = z / 1
    x = x + 10
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    var y = 0L
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 10
    y = y * x
    z = z + y
    z
  }

  def monad4(curry: Long, input: Long): Long = {
    val w = input
    var x = 0L
    var z = curry
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (26 < 0) return 0;
    z = z / 26
    x = x + -16
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    var y = 0L
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 7
    y = y * x
    z = z + y
    z
  }

  def monad5(curry: Long, input: Long): Long = {
    var z = curry
    var w = 0L
    var x = 0L
    var y = 0L
    w = input
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (1 < 0) return 0;
    z = z / 1
    x = x + 14
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 0
    y = y * x
    z = z + y
    z
  }

  def monad6(curry: Long, input: Long): Long = {
    var z = curry
    var w = 0L
    var x = 0L
    var y = 0L
    w = input
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (1 < 0) return 0;
    z = z / 1
    x = x + 12
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 4
    y = y * x
    z = z + y
    z
  }

  def monad7(curry: Long, input: Long): Long = {
    var z = curry
    var w = 0L
    var x = 0L
    var y = 0L
    w = input
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (26 < 0) return 0;
    z = z / 26
    x = x + -4
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 12
    y = y * x
    z = z + y
    z
  }

  def monad8(curry: Long, input: Long): Long = {
    var z = curry
    var w = 0L
    var x = 0L
    var y = 0L
    w = input
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (1 < 0) return 0;
    z = z / 1
    x = x + 15
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 14
    y = y * x
    z = z + y
    z
  }

  def monad9(curry: Long, input: Long): Long = {
    var z = curry
    var w = 0L
    var x = 0L
    var y = 0L
    w = input
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (26 < 0) return 0;
    z = z / 26
    x = x + -7
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 13
    y = y * x
    z = z + y
    z
  }

  def monad10(curry: Long, input: Long): Long = {
    var z = curry
    var w = 0L
    var x = 0L
    var y = 0L
    w = input
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (26 < 0) return 0;
    z = z / 26
    x = x + -8
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 10
    y = y * x
    z = z + y
    z
  }

  def monad11(curry: Long, input: Long): Long = {
    var z = curry
    var w = 0L
    var x = 0L
    var y = 0L
    w = input
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (26 < 0) return 0;
    z = z / 26
    x = x + -4
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 11
    y = y * x
    z = z + y
    z
  }

  def monad12(curry: Long, input: Long): Long = {
    var z = curry
    var w = 0L
    var x = 0L
    var y = 0L
    w = input
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (26 < 0) return 0;
    z = z / 26
    x = x + -15
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 9
    y = y * x
    z = z + y
    z
  }

  def monad13(curry: Long, input: Long): Long = {
    val w = input
    var x = 0L
    var z = curry
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (26 < 0) return 0;
    z = z / 26
    x = x + -8
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    var y = 0L
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 9
    y = y * x
    z = z + y
    z
  }
}
