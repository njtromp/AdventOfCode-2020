package nl.njtromp.adventofcode_2020

import scala.io.Source
import scala.util.{Success, Try}

object Day14 extends App {
  val lines: List[String] = Source.fromInputStream(Day14.getClass.getResourceAsStream("/input-puzzle14.txt")).getLines().toList

  def padLeft(s: String): String = {
    var p: String = s
    while (p.length < 36) {
      p = "0" + p
    }
    p
  }

  def solvePart1(lines: List[String]):Long = {
    val Mask = "mask = ([X01]{36})".r
    val Mem = "mem\\[(\\d+)\\] = (\\d+)".r

    var mem: Map[Long, Long] = Map.empty
    var mask: String = ""
    for (line <- lines) {
      line match {
        case Mask(m) => mask = m
        case Mem(address, value) =>
          val v: String = padLeft(value.toLong.toBinaryString)
          val vm: List[(Char, Char)] = v.zip(mask).toList
          val nv: Long = java.lang.Long.parseLong(vm.map(a => if (a._2 == 'X') a._1 else a._2).mkString, 2)
          mem += (address.toLong -> nv)
      }
    }
    var total: Long = 0
    for ((_, v) <- mem) {
      total += v
    }
    total
  }

  def solvePart2(lines: List[String]): Int = {
    -1
  }

  println(s"Answer part 1: ${solvePart1(lines)}")
  println(s"Answer part 2: ${solvePart2(lines)}")

}
