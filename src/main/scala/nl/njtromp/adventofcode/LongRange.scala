package nl.njtromp.adventofcode

case class LongRange(first: Long, last: Long) {
  def size: Long = last - first + 1
  def mid: Long = (first + last) / 2L

  def contains(n: Long): Boolean = first <= n && n <= last

  def isOverlapping(b: LongRange): Boolean = contains(b.first) || contains(b.last) || b.contains(first) || b.contains(last)

  def isAdjacent(b: LongRange): Boolean = !isOverlapping(b) && (last + 1 == b.first || b.last == first)

  def combine(b: LongRange): List[LongRange] =
    if (isOverlapping(b) || isAdjacent(b))
      List(new LongRange(Math.min(first, b.first), Math.max(last, b.last)))
    else
      List(this, b)

  def intersect(r: LongRange): LongRange = {
    LongRange(Math.max(first, r.first), Math.min(last, r.last))
  }

  def values(): Seq[Long] = first to last
}

object LongRange {
  implicit def apply(start: Long, last: Long): LongRange = new LongRange(start, last)
}
