package nl.njtromp.adventofcode

case class LongRange(start: Long, last: Long) {
  def size: Long = last - start + 1
  def contains(n: Long): Boolean = start <= n && n <= last

  /** Determines of this range completely contains the other range. */
  def contains(other: LongRange): Boolean = start <= other.start && last >= other.last

  /** Determines of this range has at list one element in common with the other range. */
  def isOverlapping(other: LongRange): Boolean = contains(other.start) || contains(other.last) || other.contains(start) || other.contains(last)

  /** Determines of this range and the other range  */
  def isAdjacent(other: LongRange): Boolean = !isOverlapping(other) && (last + 1 == other.start || other.last + 1 == start)

  def combine(b: LongRange): List[LongRange] =
    if (isOverlapping(b) || isAdjacent(b))
      List(new LongRange(Math.min(start, b.start), Math.max(last, b.last)))
    else
      List(this, b)
}

object LongRange {
  implicit def apply(start: Long, last: Long): LongRange = new LongRange(start, last)
}
