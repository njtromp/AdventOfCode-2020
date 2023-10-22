package nl.njtromp.adventofcode

class LongRange(val start: Long, val last: Long) {
  def size: Long = last - start + 1

  def contains(n: Long): Boolean = start <= n && n <= last

  def contains(r: LongRange): Boolean = start <= r.start && last >= r.last

  def isOverlapping(b: LongRange): Boolean = contains(b.start) || contains(b.last) || b.contains(start) || b.contains(last)

  def isAdjacent(b: LongRange): Boolean = !isOverlapping(b) && (last + 1 == b.start || b.last == start)

  def combine(b: LongRange): List[LongRange] =
    if (isOverlapping(b) || isAdjacent(b))
      List(new LongRange(Math.min(start, b.start), Math.max(last, b.last)))
    else
      List(this, b)

  override def toString: String = s"LongRange($start, $last)"
}

object LongRange {
  implicit def apply(start: Long, last: Long): LongRange = new LongRange(start, last)
}
