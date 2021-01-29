package nl.njtromp.adventofcode_2020

class Day25 extends Puzzle {

  override def solvePart1(lines: List[String]): Long = {
    val publicKeys = lines.map(_.toLong).reverse
    val loopSizes = publicKeys.map(findLoopSize)
    val encryptionKeys = publicKeys.reverse.zip(loopSizes).map(x => encrypt(x._1, x._2))
    encryptionKeys.reverse.head
  }

  override def solvePart2(lines: List[String]): Long = {
    -1
  }

  private var MODULO_PRIME = 20201227L

  def findLoopSize(publicKey: Long): Long = {
    var loopSize = 0L
    var key = 1L
    while (key != publicKey) {
      key = (key * 7L) % MODULO_PRIME
      loopSize += 1
    }
    loopSize
  }

  def encrypt(subjectNumber: Long, loopSize: Long): Long = {
    var key = 1L
    for (loop <- 1 to loopSize.toInt) {
      key = (key * subjectNumber) % MODULO_PRIME
    }
    key
  }
}

object Day25 extends App {
  new Day25().solvePuzzles("/2020/input-puzzle25.txt")
}
