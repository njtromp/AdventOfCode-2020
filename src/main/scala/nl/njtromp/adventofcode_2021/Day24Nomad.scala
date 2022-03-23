package nl.njtromp.adventofcode_2021

import scala.collection.mutable

object Day24Nomad extends App {
  type Monad = (Long, Long) => Long
  def monads: List[Monad] = List(monad0, monad1, monad2, monad3, monad4, monad5, monad6, monad7, monad8, monad9, monad10, monad11, monad12, monad13)

  val digits = (1L to 9L).toList
  val unsuccessfulCarries: List[mutable.Set[Long]] = monads.map(_ => mutable.Set.empty[Long])

  def findModelNumber(carry: Long, digits: List[Long], unsuccessfullCarries: List[mutable.Set[Long]], monads: List[Monad], modelNumber: List[Long]): Unit = {
    monads match {
      case Nil =>
        if (carry == 0) {
          println(modelNumber.reverse)
        }
      case monad :: monads =>
        if (!unsuccessfullCarries.head.contains(carry)) {
          unsuccessfullCarries.head += carry
          digits.foreach(d => findModelNumber(monad(carry, d), digits, unsuccessfullCarries.tail, monads, d :: modelNumber))
        }
    }
  }
  println("First number found is for part 1")
  findModelNumber(0, digits.reverse, unsuccessfulCarries, monads, Nil)
  println("First number found is for part 2")
  unsuccessfulCarries.foreach(_.clear())
  findModelNumber(0, digits, unsuccessfulCarries, monads, Nil)

  // Monads
  def monad0(carry: Long, input: Long): Long = {
    val w = input
    var x = 0L
    var z = 0L
    var y = 0L
    x = x + z
    if (x < 0 || 26 <= 0) throw new IllegalArgumentException
    x = x % 26
    if (1 < 0) throw new IllegalArgumentException
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
    y = y + 6
    y = y * x
    z = z + y
    z
  }

  def monad1(carry: Long, input: Long): Long = {
    val w = input
    var z = carry
    var x = 0L
    x = x + z
    if (x < 0 || 26 <= 0) throw new IllegalArgumentException
    x = x % 26
    if (1 < 0) throw new IllegalArgumentException
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

  def monad2(carry: Long, input: Long): Long = {
    val w = input
    var z = carry
    var x = 0L
    x = x + z
    if (x < 0 || 26 <= 0) throw new IllegalArgumentException
    x = x % 26
    if (1 < 0) throw new IllegalArgumentException
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

  def monad3(carry: Long, input: Long): Long = {
    val w = input
    var z = carry
    var x = 0L
    x = x + z
    if (x < 0 || 26 <= 0) throw new IllegalArgumentException
    x = x % 26
    if (1 < 0) throw new IllegalArgumentException
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

  def monad4(carry: Long, input: Long): Long = {
    val w = input
    var z = carry
    var x = 0L
    x = x + z
    if (x < 0 || 26 <= 0) throw new IllegalArgumentException
    x = x % 26
    if (26 < 0) throw new IllegalArgumentException
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

  def monad5(carry: Long, input: Long): Long = {
    val w = input
    var z = carry
    var x = 0L
    x = x + z
    if (x < 0 || 26 <= 0) throw new IllegalArgumentException
    x = x % 26
    if (1 < 0) throw new IllegalArgumentException
    z = z / 1
    x = x + 14
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    var y = 0L
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

  def monad6(carry: Long, input: Long): Long = {
    val w = input
    var z = carry
    var x = 0L
    x = x + z
    if (x < 0 || 26 <= 0) throw new IllegalArgumentException
    x = x % 26
    if (1 < 0) throw new IllegalArgumentException
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
    y = y + 4
    y = y * x
    z = z + y
    z
  }

  def monad7(carry: Long, input: Long): Long = {
    val w = input
    var z = carry
    var x = 0L
    x = x + z
    if (x < 0 || 26 <= 0) throw new IllegalArgumentException
    x = x % 26
    if (26 < 0) throw new IllegalArgumentException
    z = z / 26
    x = x + -4
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    var y = 0l
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

  def monad8(carry: Long, input: Long): Long = {
    val w = input
    var z = carry
    var x = 0L
    x = x + z
    if (x < 0 || 26 <= 0) throw new IllegalArgumentException
    x = x % 26
    if (1 < 0) throw new IllegalArgumentException
    z = z / 1
    x = x + 15
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    var y = 0L
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

  def monad9(carry: Long, input: Long): Long = {
    val w = input
    var z = carry
    var x = 0L
    x = x + z
    if (x < 0 || 26 <= 0) throw new IllegalArgumentException
    x = x % 26
    if (26 < 0) throw new IllegalArgumentException
    z = z / 26
    x = x + -7
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    var y = 0L
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

  def monad10(carry: Long, input: Long): Long = {
    val w = input
    var z = carry
    var x = 0L
    x = x + z
    if (x < 0 || 26 <= 0) throw new IllegalArgumentException
    x = x % 26
    if (26 < 0) throw new IllegalArgumentException
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
    y = y + 10
    y = y * x
    z = z + y
    z
  }

  def monad11(carry: Long, input: Long): Long = {
    val w = input
    var z = carry
    var x = 0L
    x = x + z
    if (x < 0 || 26 <= 0) throw new IllegalArgumentException
    x = x % 26
    if (26 < 0) throw new IllegalArgumentException
    z = z / 26
    x = x + -4
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    var y = 0L
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

  def monad12(carry: Long, input: Long): Long = {
    val w = input
    var z = carry
    var x = 0L
    x = x + z
    if (x < 0 || 26 <= 0) throw new IllegalArgumentException
    x = x % 26
    if (26 < 0) throw new IllegalArgumentException
    z = z / 26
    x = x + -15
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

  def monad13(carry: Long, input: Long): Long = {
    val w = input
    var z = carry
    var x = 0L
    x = x + z
    if (x < 0 || 26 <= 0) throw new IllegalArgumentException
    x = x % 26
    if (26 < 0) throw new IllegalArgumentException
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
