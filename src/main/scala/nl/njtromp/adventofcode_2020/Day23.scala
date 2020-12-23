package nl.njtromp.adventofcode_2020

class Day23(var moves: Int) extends Puzzle {
  class Cup(value: Long) {
    var next: Cup = null;
    def last: Cup = if (next == null) this else next.last
    def find(nr: Long): Cup = if (value == nr) this else next.find(nr)
    def skip(nr: Int): Cup = if (nr == 0) this else next.skip(nr - 1)
    def cut(tail: Cup): Unit = if (next == tail) next = null else next.cut(tail)
    def exists(nr: Long): Boolean = {
      def exists(nr: Long, head: Cup, cur: Cup): Boolean = {
        if (nr == cur.label)
          true
        else if (cur == head) false else exists(nr, head, cur.next)
      }
      exists(nr, this, this.next)
    }
    def label: Long = value.toChar
    override def toString: String = value.toString
  }

  override def solvePart1(lines: List[String]): Long = {
    var current: Cup = createCups(lines.head.toList)
    current.last.next = current
    while (moves > 0) {
      val removed = current.next
      val gap = removed.skip(3)
      removed.cut(gap)
      current.next = gap

      var value = current.label
      do {
        value -= 1
        if (value == 0) value = 9
      }
      while (!current.exists(value));

      removed.last.next = current.find(value).next
      current.find(value).next = removed

      current = current.next
      moves -= 1
    }
    val one = current.find(1)
    val result: StringBuilder = new StringBuilder()
    current = one.next
    while (current != one) {
      result.append(current.label)
      current = current.next
    }
    result.toString().toLong
  }

  override def solvePart2(lines: List[String]): Long = {
    -1
  }

  def createCups(labels: List[Char]): Cup = {
    labels match {
      case Nil => null
      case label :: tail => {
        val cup = new Cup(label.toString.toLong)
        cup.next = createCups(tail)
        cup
      }
    }
  }

}

object Day23 extends App {
  new Day23(100).solvePuzzles("/input-puzzle23.txt")
}
