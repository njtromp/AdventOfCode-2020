package nl.njtromp.adventofcode_2020

import scala.io.Source
import scala.util.{Success, Try}

object Day13 extends App {
  val lines: List[String] = Source.fromInputStream(Day13.getClass.getResourceAsStream("/input-puzzle13.txt")).getLines().toList

  def solvePart1(lines: List[String]):Int = {
    val time = lines.head.toInt
    val bus = lines(1).split(",").filter(id => id != "x").map(id => (id.toInt, id.toInt - time % id.toInt)).sortBy(i => i._2).toList.head
    bus._1 * bus._2
  }

  def solvePart2(lines: List[String]): Long = {
    val busses = lines(1).split(",").zipWithIndex.filter(b => b._1 != "x").map(b => (b._1.toLong, (b._1.toLong - b._2.toLong) % b._1.toLong)).toList
    chineseRemainder(busses.map(b => b._1), busses.map(b => b._2)).get
  }

  println(s"Answer part 1: ${solvePart1(lines)}")
  println(s"Answer part 2: ${solvePart2(lines)}")

  // Thanks to Jari for the pointer ;-)
  // https://rosettacode.org/wiki/Chinese_remainder_theorem#Scala
  def chineseRemainder(n: List[Long], a: List[Long]): Option[Long] = {
    require(n.size == a.size)
    val prod = n.product

    def iter(n: List[Long], a: List[Long], sm: Long): Long = {
      def mulInv(a: Long, b: Long): Long = {
        def loop(a: Long, b: Long, x0: Long, x1: Long): Long = {
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
        }

        if (b == 1) 1
        else {
          val x1 = loop(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
        }
      }

      if (n.nonEmpty) {
        val p = prod / n.head

        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      } else sm
    }

    Try {
      iter(n, a, 0) % prod
    } match {
      case Success(v) => Some(v)
      case _          => None
    }
  }
}
