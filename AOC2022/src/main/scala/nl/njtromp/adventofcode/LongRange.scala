package nl.njtromp.adventofcode

case class LongRange(first: Long, last: Long) {
  def size: Long = last - first + 1
  def contains(n: Long): Boolean = first <= n && n <= last

  /** Determines of this range completely contains the other range. */
  def contains(other: LongRange): Boolean = first <= other.first && last >= other.last

  /** Determines of this range has at list one element in common with the other range. */
  def isOverlapping(other: LongRange): Boolean = contains(other.first) || contains(other.last) || other.contains(first) || other.contains(last)

  /** Determines of this range and the other range  */
  def isAdjacent(other: LongRange): Boolean = !isOverlapping(other) && (last + 1 == other.first || other.last + 1 == first)

  def combine(b: LongRange): List[LongRange] =
    if (isOverlapping(b) || isAdjacent(b))
      List(new LongRange(Math.min(first, b.first), Math.max(last, b.last)))
    else
      List(this, b)

  def intersect(r: LongRange): LongRange =
    LongRange(Math.max(first, r.first), Math.min(last, r.last))

  def values(): Seq[Long] = first to last

}

object LongRange {
  implicit def apply(start: Long, last: Long): LongRange = new LongRange(start, last)
}
