package nl.njtromp.adventofcode

class RockGenerator extends Iterator[Array[Long]] {
  private val MIN_POSITION = -1000L
  private val MAX_POSITION = 1000L
  private val MIN_VElOCITY = -100L
  private val MAX_VELOCITY = 100L

  private val values = Array(MIN_POSITION, MIN_POSITION, MIN_POSITION, MIN_VElOCITY, MIN_VElOCITY, MIN_VElOCITY)

  override def hasNext: Boolean = values.take(3).forall(_ <= MAX_POSITION) && values.drop(3).forall(_ <= MAX_VELOCITY)

  override def next(): Array[Long] =
    val rock = values.clone()
    (5 to 0 by -1).foldLeft(1L)((c, i) =>
      if c == 1L then
        values(i) += 1
        if values(i) > (if i >= 2 then MAX_VELOCITY else MAX_POSITION) then
          values(i) = if i >= 2 then MIN_VElOCITY else MIN_POSITION
          1L
        else
          0L
      else
        0L
    )
    rock

}
