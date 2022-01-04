package nl.njtromp.adventofcode_2021

object Day24Nomad extends App {
  def monads: List[(Long, Long) => Long] = List(monad0, monad1, monad2, monad3, monad4, monad5, monad6, monad7, monad8, monad9, monad10, monad11, monad12, monad13)
  type NomadInfo = Map[(Long, Long), Long]

  val digits = 1L to 9L

  def findModelNumber(monad: Int, carry: Long, modelNumber: Long): Long = {
    if (monad == monads.length) {
      if (carry == 0)
        modelNumber
      else
        throw new IllegalArgumentException
    } else {
      var nr = 0L
      digits.foreach(d => {
        try {
          if (nr == 0L) {
            val newCarry = monads(monad)(carry, d)
            nr = findModelNumber(monad + 1, newCarry, modelNumber * 10L + d)
          }
        } catch {
          case _: IllegalArgumentException => ;// Just to get notified about
        }
      })
      nr
    }
  }

  println(findModelNumber(0, 0, 0))

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
//    if (x == 1) throw new IllegalArgumentException("Exploding carry!")
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
//    if (x == 1) throw new IllegalArgumentException("Exploding carry!")
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
//    if (x == 1) throw new IllegalArgumentException("Exploding carry!")
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
//    if (x == 1) throw new IllegalArgumentException("Exploding carry!")
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
//    if (x == 1) throw new IllegalArgumentException("Exploding carry!")
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
//    if (x == 1) throw new IllegalArgumentException("Exploding carry!")
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
//    if (x == 1) throw new IllegalArgumentException("Exploding carry!")
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
//    if (x == 1) throw new IllegalArgumentException("Exploding carry!")
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
//    if (x == 1) throw new IllegalArgumentException("Exploding carry!")
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
//    if (x == 1) throw new IllegalArgumentException("Exploding carry!")
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
//    if (x == 1) throw new IllegalArgumentException("Exploding carry!")
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
//    if (x == 1) throw new IllegalArgumentException("Exploding carry!")
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
//    if (x == 1) throw new IllegalArgumentException("Exploding carry!")
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
//    if (x == 1) throw new IllegalArgumentException("Exploding carry!")
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
