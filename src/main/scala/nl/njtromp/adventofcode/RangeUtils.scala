package nl.njtromp.adventofcode

class RangeUtils {
  implicit class RangeUtils(a: Range) {
    def holds(b: Range): Boolean = a.start <= b.start && a.last >= b.last

    def overlap(b: Range): Range = {
      val start = Math.max(a.start, b.start)
      val last = Math.min(a.last, b.last)
      if (start <= last) start to last else Range(0, 0)
    }

    def isOverlapping(b: Range): Boolean = a.contains(b.start) || a.contains(b.last)
    def isAdjacent(b: Range): Boolean = !isOverlapping(b) && (a.last + 1 == b.start || b.last == a.start)
    def combine(b: Range): List[Range] =
      if (isOverlapping(b) || isAdjacent(b))
        List(Math.min(a.start, b.start) to Math.max(a.last, b.last))
      else
        List(a, b)
  }

}
