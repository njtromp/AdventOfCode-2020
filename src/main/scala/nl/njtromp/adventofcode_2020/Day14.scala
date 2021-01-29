package nl.njtromp.adventofcode_2020

class Day14 extends Puzzle {
  private val Mask = "mask = ([X01]{36})".r
  private val Mem = "mem\\[(\\d+)\\] = (\\d+)".r

  def solvePart1(lines: List[String]):Long = {
    var mem: Map[Long, Long] = Map.empty
    var mask: String = ""
    for (line <- lines) {
      line match {
        case Mask(m) => mask = m
        case Mem(address, value) =>
          val vm: List[(Char, Char)] = padLeft(value.toLong.toBinaryString).zip(mask).toList
          mem += (address.toLong -> java.lang.Long.parseLong(vm.map(a => if (a._2 == 'X') a._1 else a._2).mkString, 2))
      }
    }
    mem.values.sum
  }

  def solvePart2(lines: List[String]): Long = {
    var mem: Map[Long, Long] = Map.empty
    var mask: String = ""
    for (line <- lines) {
      line match {
        case Mask(m) => mask = m
        case Mem(address, value) =>
          val nam = padLeft(address.toLong.toBinaryString).zip(mask).toList.map(a => if (a._2 == '0') a._1 else if (a._2 == '1') '1' else 'X').mkString
          val baseAddress = java.lang.Long.parseLong(nam.replaceAll("X","0"), 2)
          for (bits <- 0 until 1 << nam.replaceAll("[01]", "").length) {
            var floatingAddress: Long = baseAddress
            var bitSelector = 0L
            nam.zipWithIndex.filter(_._1 == 'X').map(_._2).foreach(i => {
              floatingAddress = floatingAddress | (((bits.toLong >>> bitSelector) & 1L) << (35 - i))
              bitSelector += 1
            })
            mem += (floatingAddress -> value.toLong)
          }
      }
    }
    mem.values.sum
  }

  def padLeft(s: String): String = {
    var p: String = s
    while (p.length < 36) {
      p = "0" + p
    }
    p
  }

}

object Day14 extends App {
  new Day14().solvePuzzles("/2020/input-puzzle14.txt")
}
