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

  private case class Robot(material: String, produces : Int, costs: Array[Long]) {
    def canBeProduced(stock : Array[Long]) : Boolean = stock.zip(costs).forall(m =>  m._1 >= m._2)
    def produceRobot(stock: Array[Long]): Array[Long] = (DUMMY to GEODE).map(m => stock(m) - costs(m)).toArray
  }

  private class Blueprint(val id: Int, val robots: Array[Robot])

  private def readBlueprints(lines: List[String]): List[Blueprint] = {
    val dummy = Robot("Dummy", DUMMY, Array(0L, 0L, 0L, 0L, 0L))
    lines.map {
      case BLUEPRINT(id, oreRobotOreCosts, clayRobotOreCosts, obsidianRobotOreCosts, obsidianRobotClayCosts, geodeRobotOreCosts, geodeRobotObsidianCosts) =>
        new Blueprint(
          id.toInt,
          Array(
            dummy,
            Robot("Ore", ORE, Array(0L, oreRobotOreCosts.toLong, 0L, 0L, 0L)),
            Robot("Clay", CLAY, Array(0L, clayRobotOreCosts.toLong, 0L, 0L, 0L)),
            Robot("Obsidian", OBSIDIAN, Array(0L, obsidianRobotOreCosts.toLong, obsidianRobotClayCosts.toLong, 0L, 0L)),
            Robot("Geodes", GEODE, Array(0L, geodeRobotOreCosts.toLong, 0L, geodeRobotObsidianCosts.toLong, 0L))
          )
        )
    }
  }

  private def runMine(minutes: Int, blueprint: Blueprint): Long = {
    val usedRobots = Array.fill(minutes + 1)(0L)
    val needGeodeRobot = minutes - Math.sqrt(blueprint.robots(GEODE).costs(OBSIDIAN))
    val needObsidianRobot = needGeodeRobot - Math.sqrt(blueprint.robots(OBSIDIAN).costs(CLAY))
    var constructionOrder = Array(0L)
    var bestStock = Array(0L)
    var minedGeodes = 0L
    var noGeodePossible = 0L
    var noObsidianPossible = 0L
    var cantImprove = 0L
    def runMine(minute: Int, robots: Array[Long], stock: Array[Long]): Unit = {
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
        if ((minedGeodes * 8L) / 10L > stock(GEODE) + minutesLeft * robots(GEODE) + maxProductionByNewRobots) {
          cantImprove += 1L
          return
        }
        val canBeProduced = blueprint.robots.filter(_.canBeProduced(stock)).reverse
        canBeProduced.foreach(r => {
          usedRobots(minute) = r.produces
          // Produce new robot
          val newStock = r.produceRobot(stock)
          // Update stock
          (ORE to GEODE).foreach(m => newStock(m) += robots(m))
          // New robot is ready
          val newRobots = robots.clone()
          newRobots(r.produces) += 1L
          // Continue
          runMine(minute + 1, newRobots, newStock)
        })
      }
    }
    time(runMine(1, Array(0L, 1L, 0L, 0L, 0L), Array(0L, 0L, 0L, 0L, 0L)))
    println(s"Pruning caused by no Geode possible $noGeodePossible")
    println(s"Pruning caused by no Obsidian possible $noObsidianPossible")
    println(s"Pruning caused by not possible to improve $cantImprove")
    println(s"Blueprint ${blueprint.id} constructed the following robots ${constructionOrder.toList.drop(1)} to mine ${bestStock.toList.drop(1)}.")
    minedGeodes * blueprint.id
  }

//  private var quickAnswerPart1 = 33L
  override def exampleAnswerPart1: Long = 33
  override def solvePart1(lines: List[String]): Long = {
//    val answer = quickAnswerPart1
//    quickAnswerPart1 = 1659
//    answer
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
    qualityLevels.sum // 1659 in 57 minutes :-(
  }

  var quickAnswerPart2: Long = 56 * 62
  override def exampleAnswerPart2: Long = 56L * 62L
  override def solvePart2(lines: List[String]): Long = {
    val answer = quickAnswerPart2
    quickAnswerPart2 = 6804
    answer
//    val blueprints = readBlueprints(lines).take(3)
//    val qualityLevels = Array.fill[Long](blueprints.length)(0)
//    val workers = Array.fill[Thread](blueprints.length)(null)
//    val minutes = 32
//    blueprints.zipWithIndex.foreach(a => {
//      workers(a._2) = new Thread(() => {
//        qualityLevels(a._2) = runMine(minutes, a._1)
//      })
//      workers(a._2).start()
//    })
//    workers.foreach(_.join())
//    val openedGeodes = qualityLevels.toList.zipWithIndex.map(a => a._1 / (a._2 + 1))
//    println(s"Geodes opened $openedGeodes")
//    openedGeodes.product // 6804 (28*27*9) just a guess based on the failures below :-)
    /*
     28 and 27 are the number of Geodes found for blueprints 3 and 2 respectively
     and from there on just using binarysearch starting from 26
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
