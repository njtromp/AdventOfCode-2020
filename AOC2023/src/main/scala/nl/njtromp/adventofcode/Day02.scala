package nl.njtromp.adventofcode

class Day02 extends Puzzle[Long] {
  private val GameInfo = "Game (\\d+)".r
  private type Cubes = (Int, Int, Int)
  private case class Game(id: Int, cubes: List[Cubes]) {
    def getMaximums: Cubes = cubes.foldLeft((0, 0, 0))((a, c) => (Math.max(a._1, c._1), Math.max(a._2, c._2), Math.max(a._3, c._3)))
  }

  private def createGames(id: Int, takes: Array[String]): Game =
    def getCount(color: String, colors: Array[String]): Int =
      val ColorCount = s"(\\d+) $color".r
      val counts = colors.map(_.trim match {
        case ColorCount(count) => count.toInt
        case _ => 0
      })
      counts.sum
    def colorCount(take: String): Cubes =
      val balls = take.split(",")
      (getCount("red", balls), getCount("green", balls), getCount("blue", balls))
    Game(id, takes.map(colorCount).toList)

  override def exampleAnswerPart1: Long = 8
  override def solvePart1(lines: List[String]): Long =
    val games = lines.map(l => {
      l.split(":").head match {
        case GameInfo(id) => createGames(id.toInt, l.split(":").last.split(";"))
      }
    })
    val possiblegames = games.filter(g => g.getMaximums._1 <= 12 && g.getMaximums._2 <= 13 && g.getMaximums._3 <= 14)
    possiblegames.map(_.id).sum


  override def exampleAnswerPart2: Long = 2286
  override def solvePart2(lines: List[String]): Long =
    val games = lines.map(l => {
      l.split(":").head match {
        case GameInfo(id) => createGames(id.toInt, l.split(":").last.split(";"))
      }
    })
    games.map(g => g.getMaximums._1 * g.getMaximums._2 * g.getMaximums._3).sum

}

object Day02 extends App {
  new Day02().solvePuzzles("/day02.txt")
}
