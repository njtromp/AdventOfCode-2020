package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

class Day19 extends Puzzle2 {
  private val BLUEPRINT = """Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.""".r
  // Indexes into stock and robot arrays
  private val DUMMY = 0
  private val ORE = 1
  private val CLAY = 2
  private val OBSIDIAN = 3
  private val GEODE = 4

  private case class Robot(material: String, produces : Int, costs: Array[Int]) {
    def canBeProduced(stock : Array[Int]) : Boolean = stock.zip(costs).forall(m =>  m._1 >= m._2)
    def produceRobot(stock: Array[Int]): Array[Int] = (DUMMY to GEODE).map(m => stock(m) - costs(m)).toArray
  }

  private class Blueprint(val id: Int, val robots: Array[Robot]) {
    val maxRobots = (DUMMY to GEODE).map(m => robots.map(r => r.costs(m)).max).toArray
    maxRobots(DUMMY) = Int.MaxValue
    maxRobots(GEODE) = Int.MaxValue
  }

  private def readBlueprints(lines: List[String]): List[Blueprint] = {
    val dummy = Robot("Dummy", DUMMY, Array(0, 0, 0, 0, 0))
    lines.map {
      case BLUEPRINT(id, oreRobotOreCosts, clayRobotOreCosts, obsidianRobotOreCosts, obsidianRobotClayCosts, geodeRobotOreCosts, geodeRobotObsidianCosts) =>
        new Blueprint(
          id.toInt,
          Array(
            dummy,
            Robot("Ore", ORE, Array(0, oreRobotOreCosts.toInt, 0, 0, 0)),
            Robot("Clay", CLAY, Array(0, clayRobotOreCosts.toInt, 0, 0, 0)),
            Robot("Obsidian", OBSIDIAN, Array(0, obsidianRobotOreCosts.toInt, obsidianRobotClayCosts.toInt, 0, 0)),
            Robot("Geodes", GEODE, Array(0, geodeRobotOreCosts.toInt, 0, geodeRobotObsidianCosts.toInt, 0))
          )
        )
    }
  }

  private def runMine(minutes: Int, blueprint: Blueprint): Long = {
    val usedRobots = Array.fill(minutes + 1)(0)
    val needGeodeRobot = minutes - Math.sqrt(blueprint.robots(GEODE).costs(OBSIDIAN))
    val needObsidianRobot = needGeodeRobot - Math.sqrt(blueprint.robots(OBSIDIAN).costs(CLAY))
    var constructionOrder = Array(0)
    var bestStock = Array(0)
    var minedGeodes = 0L
    var noGeodePossible = 0L
    var noObsidianPossible = 0L
    var cantImprove = 0L
    var overproduction = 0L
    def runMine(minute: Int, robots: Array[Int], stock: Array[Int]): Unit = {
      if (minute > minutes) {
        if (minedGeodes < stock(GEODE)) {
          minedGeodes = stock(GEODE)
          constructionOrder = usedRobots.clone()
          bestStock = stock.clone()
        }
      } else {
        // Bail out if there is not enough time to mine
        if (minute > needGeodeRobot && robots(OBSIDIAN) == 0) {
          noGeodePossible += 1L
          return
        }
        if (minute > needObsidianRobot && robots(CLAY) == 0) {
          noObsidianPossible += 1L
          return
        }
        val minutesLeft = minutes - minute
        def maxProductionByNewRobots: Long = // 1 + 3 + 5 ... + n = (n + 1) * (n + 1) / 4
          (minutesLeft + 2) * (minutesLeft + 2) / 4
        // If maximum estimated production is below 80% of the current best, bail out
        if ((minedGeodes * 8) / 10 > stock(GEODE) + minutesLeft * robots(GEODE) + maxProductionByNewRobots) {
          cantImprove += 1L
          return
        }
        val canBeProduced = blueprint.robots.filter(_.canBeProduced(stock))
        // No need to produce more material then can be used for constructing the most demanding robot (using that material)
        val willBeProduced = canBeProduced.filter(r => robots(r.produces) < blueprint.maxRobots(r.produces))
        overproduction += canBeProduced.length - willBeProduced.length
        willBeProduced.foreach(r => {
          usedRobots(minute) = r.produces
          // Produce new robot
          val newStock = r.produceRobot(stock)
          // Update stock
          (ORE to GEODE).foreach(m => newStock(m) += robots(m))
          // New robot is ready
          val newRobots = robots.clone()
          newRobots(r.produces) += 1
          // Continue
          runMine(minute + 1, newRobots, newStock)
        })
      }
    }
    time(runMine(1, Array(0, 1, 0, 0, 0), Array(0, 0, 0, 0, 0)))
    println(s"Pruning caused by no Geode possible $noGeodePossible")
    println(s"Pruning caused by no Obsidian possible $noObsidianPossible")
    println(s"Pruning caused by not possible to improve $cantImprove")
    println(s"Pruning caused by overproduction $overproduction")
    println(s"Blueprint ${blueprint.id} constructed the following robots ${constructionOrder.toList.drop(1)} to mine ${bestStock.toList.drop(1)}.")
    minedGeodes * blueprint.id
  }

  override def exampleAnswerPart1: Long = 33
  override def solvePart1(lines: List[String]): Long = {
    val blueprints = readBlueprints(lines)
    val qualityLevels = Array.fill[Long](blueprints.length)(0)
    val workers = Array.fill[Thread](blueprints.length)(null)
    val minutes = 24
    blueprints.zipWithIndex.foreach(a => {
      workers(a._2) = new Thread(() => {qualityLevels(a._2) = runMine(minutes, a._1)})
      workers(a._2).start()
    })
    workers.foreach(_.join())
    println(s"Quality levels ${qualityLevels.toList}")
    qualityLevels.sum // 1659 in 57 minutes :-(, in less then 6 minutes after applying overproduction optimization.
  }

  override def exampleAnswerPart2: Long = 56 * 62
  override def solvePart2(lines: List[String]): Long = {
    val blueprints = readBlueprints(lines).take(3)
    val qualityLevels = Array.fill[Long](blueprints.length)(0)
    val workers = Array.fill[Thread](blueprints.length)(null)
    val minutes = 32
    blueprints.zipWithIndex.foreach(a => {
      workers(a._2) = new Thread(() => {
        qualityLevels(a._2) = runMine(minutes, a._1)
      })
      workers(a._2).start()
    })
    workers.foreach(_.join())
    val openedGeodes = qualityLevels.toList.zipWithIndex.map(a => a._1 / (a._2 + 1))
    println(s"Geodes opened $openedGeodes")
    openedGeodes.product // 6804 (28*27*9) just a guess based on the failures below :-)
    /*
     After applying the overproduction optimization it takes about 1,5 hours to do part 2
     28 and 27 are the number of Geodes found for blueprints 3 and 2 respectively
     and from there on just using 'binary search' starting from 26.
     19656 (28*27*26) too high
     15120 (28*27*20) too high
     7560 (28*27*10) too high
     3780 (28*27*5) not good
     */
  }

}

object Day19 extends App{
  new Day19().solvePuzzles("/2022/day19.txt")
}
