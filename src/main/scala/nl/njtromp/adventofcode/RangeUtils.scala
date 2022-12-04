package nl.njtromp.adventofcode

class RangeUtils {
  implicit class RangeUtils(a: Range) {
    def holds(b: Range): Boolean = a.start <= b.start && a.last >= b.last

    def overlap(b: Range): Range = {
      val start = Math.max(a.start, b.start)
      val last = Math.min(a.last, b.last)
      if (start <= last) start to last else Range(0, 0)
    }
  }

}
