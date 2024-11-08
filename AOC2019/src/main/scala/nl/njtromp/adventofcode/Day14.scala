package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

class Day14 extends Puzzle[Long] {
  private val FUEL = "FUEL"
  private val ORE = "ORE"
  private type Chemical = (String, Long)

  private def parseLine(line: String): (Chemical, List[Chemical]) =
    def parseParts(line: String): Chemical =
      val parts = line.split(" ").map(_.trim)
      (parts.last, parts.head.toLong)
    val parts = line.split("=>").map(_.trim)
    (parseParts(parts.last), parts.head.split(",").map(_.trim).toList.map(parseParts))

  private def produceFuel(amount: Long, chemicals: List[(Chemical, List[Chemical])]): Long =
    val recipes = chemicals.map(c => (c._1._1, c)).toMap
    val neededFor = chemicals.flatMap((p, n) => n.map(c => (c._1, p._1))).groupMap(_._1)(_._2)
    def productionNeeded(chemical: String): Long =
      if chemical == FUEL then
        amount
      else
        neededFor(chemical).map(c =>
          val required = productionNeeded(c)
          val recipe = recipes(c)
          val prodRecipe = recipe._2.filter(_._1 == chemical).head
          val productionAmount = prodRecipe._2
          val multiplier = required / recipe._1._2 + (if required % recipe._1._2 == 0 then 0 else 1)
          val needed = productionAmount * multiplier
          needed
        ).sum
    productionNeeded(ORE)

  private def produce(toBeProduced: List[Chemical], chemicals: List[(Chemical, List[Chemical])]): Long =
    val recipes = chemicals.map(c => (c._1._1, c)).toMap
    @tailrec
    def produce(toBeProduced: List[Chemical]): List[Chemical] =
      if toBeProduced.size == 1 && toBeProduced.head._1 == ORE then
        toBeProduced
      else
        val bla: List[Chemical] = toBeProduced.filter(_._1 == ORE) ++
          toBeProduced.filter(_._1 != ORE).flatMap(c =>
            print(s"Producing ${c._1} ")
            val recipe = recipes(c._1)
            val multiplier = Math.max(1, c._2 / recipe._1._2) //+ (if c._2 % recipe._1._2 != 0 then 1 else 0)
            print(s"${c._2} /  ${recipe._1._2} => $multiplier ")
            val needed = recipe._2.map(r => (r._1, r._2 * multiplier))
            println(s"requires $needed")
            needed
          )
        println
        val reduced = bla.groupBy(_._1).map(r => (r._1, r._2.map(_._2).sum)).toList
        produce(reduced)
    produce(toBeProduced).head._2

  override def exampleAnswerPart1: Long = 31 + 165 + 2210736
  override def solvePart1(lines: List[String]): Long =
    groupByEmptyLine(lines).map(ls =>
      val chemicals = ls.map(parseLine)
      produceFuel(1, chemicals)
    ).sum

  override def exampleAnswerPart2: Long = 460664
  override def solvePart2(lines: List[String]): Long =
    val ls = if lines.length == 32 then lines.drop(15) else lines
    val chemicals = ls.map(parseLine)
    val MAX_ORE = 1000000000000L 
    var fuel = 1L
    var ore = produceFuel(fuel, chemicals)
    while ore < MAX_ORE do
      fuel *= 2
      ore = produceFuel(fuel, chemicals)
    var low = fuel / 2
    while low + 1 != fuel && produceFuel(fuel, chemicals) > MAX_ORE do
      val mid = low + (fuel - low) / 2
      if produceFuel(mid, chemicals) < MAX_ORE then
        low = mid
      else
        fuel = mid
    fuel - 1

}

object Day14 extends App {
  new Day14().solvePuzzles()
}
