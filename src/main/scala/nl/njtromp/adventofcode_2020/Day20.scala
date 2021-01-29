package nl.njtromp.adventofcode_2020

import java.util.Objects
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

class Day20 extends Puzzle {
  case class Tile(id: Long, orientation: String, top: String, right: String, bottom: String, left: String) {
    override def equals(obj: Any): Boolean = {
      obj match {
        case Tile(oId, _, oTop, oRight, oBottom, oLeft) =>
          id == oId && top.equals(oTop) && right.equals(oRight) && bottom.equals(oBottom) && left.equals(oLeft)
        case _ => false
      }
    }

    override def hashCode(): Int = {
      id.toInt + 13 * Objects.hash(top, right, bottom, left)
    }

    override def toString: String = {
      s"${id}\n\t${top}\n\t\t${right}\n${left}\n\t${bottom}"
    }
  }

  private var tileData: List[(Long, List[String])] = null
  private var image: Array[Array[Tile]] = null

  override def solvePart1(lines: List[String]): Long = {
    tileData = readTiles(lines)
    val tiles: Set[Tile] = rotateAndFlip(tileData)
    val tops: Map[String, Set[Tile]] = tiles.groupBy(_.top)
    val lefts: Map[String, Set[Tile]] = tiles.groupBy(_.left)
    val dim = Math.sqrt(tileData.size).toInt
    image = Array.ofDim(dim, dim)

    def createImage(tileNr: Int, usedTiles: Set[Long]): Boolean = {
      if (tileNr >= dim * dim) {
//        for (row <- image) {
//          for (tile <- row) {
//            print(s"${tile.id} (${tile.orientation}) ")
//          }
//          println
//        }
        true
      } else {
        val x = tileNr % dim
        val y = tileNr / dim
        // Must match tile from row above
        val matchingTop: Set[Tile] = (if (y > 0) tops(image(y - 1)(x).bottom) else tiles).filter(t => !usedTiles.contains(t.id))
        val matchingLeft: Set[Tile] = if (x > 0) lefts(image(y)(x - 1).right) else tiles.filter(t => !usedTiles.contains(t.id))
        val possibleTiles = matchingTop & matchingLeft
        for (tile <- possibleTiles) {
          image(y)(x) = tile
          if (createImage(tileNr + 1, usedTiles + tile.id)) {
            return true
          }
          image(y)(x) = null
        }
        false
      }
    }

    if (createImage(0, Set.empty)) image(0)(0).id * image(0)(dim - 1).id * image(dim - 1)(0).id * image(dim -1)(dim - 1).id else -1
  }

  override def solvePart2(lines: List[String]): Long = {
    solvePart1(lines)

    def antiClockwise(matrix: Array[String]): Array[String] = {
      val N = matrix.length
      def rotateAntiClockwise(mat: Array[Array[Char]]): Unit = {
        // Consider all squares one by one
        for (x <- 0 until N / 2) { // Consider elements in group
          // of 4 in current square
          for (y <- x until N - x - 1) { // Store current cell in
            // temp variable
            val temp = mat(x)(y)
            // Move values from right to top
            mat(x)(y) = mat(y)(N - 1 - x)
            // Move values from bottom to right
            mat(y)(N - 1 - x) = mat(N - 1 - x)(N - 1 - y)
            // Move values from left to bottom
            mat(N - 1 - x)(N - 1 - y) = mat(N - 1 - y)(x)
            // Assign temp to left
            mat(N - 1 - y)(x) = temp
          }
        }
      }
      val m = matrix.map(_.toArray)
      rotateAntiClockwise(m)
      m.map(_.mkString)
    }

    def clockwise(matrix: Array[String]): Array[String] = {
      val N = matrix.length
      // Traverse each cycle
      def rotateClockwise(a: Array[Array[Char]]): Unit = {
        var i = 0
        while ( {
          i < N / 2
        }) {
          var j = i
          while ( {
            j < N - i - 1
          }) { // Swap elements of each cycle
            // in clockwise direction
            val temp = a(i)(j)
            a(i)(j) = a(N - 1 - j)(i)
            a(N - 1 - j)(i) = a(N - 1 - i)(N - 1 - j)
            a(N - 1 - i)(N - 1 - j) = a(j)(N - 1 - i)
            a(j)(N - 1 - i) = temp

            j += 1
          }

          i += 1
        }
      }
      val m = matrix.map(_.toArray)
      rotateClockwise(m)
      m.map(_.mkString)
    }

    def flipRotate(orientation: String, image: Array[String]): Array[String] = {
      val temp = if (orientation.startsWith("F")) image.map(_.reverse) else image
      orientation.charAt(1) match {
        case '0' => temp
        case '1' => antiClockwise(temp)
        case '2' => temp.reverse.map(_.reverse)
        case '3' => clockwise(temp)
      }
    }

    val tilesById: Map[Long, List[String]] = tileData.toMap

    def extractImage(tile: Tile): Array[String] = {
      // Remove upper and lower border and the left and right border
      flipRotate(tile.orientation, tilesById(tile.id).tail.reverse.tail.reverse.map(_.substring(1, 9)).toArray)
    }

    val rawImage: Array[Array[Array[String]]] = image.map(_.map(extractImage))

    val finalImage: Array[String] = Array.ofDim(rawImage.length * 8)
    var i = 0
    for (y <- rawImage.indices) {
      for (l <- rawImage(y)(0).indices) {
        finalImage(i) = rawImage(y).map(_(l)).mkString
        i += 1
      }
    }
//    finalImage.foreach(println(_))
    val monster: Array[String] = Array(
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
    )
    val nrOfMonsters = if (finalImage.length == 3) countMonsters(antiClockwise(antiClockwise(finalImage)), monster) else countMonsters(finalImage, monster)
    // Rotating and flipping should have been build into the code, but just rotating the map 180 degrees solved part 2!
    if (nrOfMonsters > 0) finalImage.map(_.count(_ == '#')).sum - nrOfMonsters * monster.map(_.count(_ == '#')).sum else -1
  }

  def countMonsters(finalImage: Array[String], monster: Array[String]): Int = {
    val patterns: Array[String] = monster.map(m => m.replaceAll(" ", "."))
    var monsters = 0
    for (l <- 0 until finalImage.length - monster.length) {

      var matchStarts: mutable.Set[Int] = mutable.Set.empty ++ finalImage(l).indices.toSet
      for (p <- patterns.indices) {
        val starts: mutable.Set[Int] = mutable.Set.empty
        for (i <- 0 to finalImage(l + p).length - monster(p).length) {
          if (finalImage(l + p).substring(i, i + monster(p).length).matches(patterns(p))) {
            starts += i
          }
        }
        matchStarts = matchStarts & starts
      }
      monsters += matchStarts.size
    }
    monsters
  }

  def readTiles(lines: List[String]): List[(Long, List[String])] = {
    val ID = "Tile (\\d+):".r
    var tiles: ListBuffer[(Long, List[String])] = ListBuffer.empty
    var id: Long = 0
    var tile: ListBuffer[String] = ListBuffer.empty
    for (line <- lines) {
      line match {
        case ID(nr) => id = nr.toLong
        case "" =>
          tiles += ((id, tile.toList))
          tile = ListBuffer.empty
        case _ => tile += line
      }
    }
    tiles.toList
  }

  def rotateAndFlip(tiles: List[(Long, List[String])]): Set[Tile] = {
    tiles.flatMap({case (id, tile) => rotateAndFlip(id, tile)}).toSet
  }

  def rotateAndFlip(id: Long, tile: List[String]): Set[Tile] = {
    val top = tile.head
    val right = tile.map(_.reverse.head).mkString
    val bottom = tile.reverse.head
    val left = tile.map(_.head).mkString
    rotate(id, "R",(top, right, bottom, left)) ++ rotate(id, "F", (top.reverse, left, bottom.reverse, right))
  }

  def rotate(id: Long, orientation: String, tile: (String, String, String, String)): Set[Tile] = {
    Set(Tile(id, orientation + "0", tile._1, tile._2, tile._3, tile._4),
      Tile(id, orientation + "1", tile._2, tile._3.reverse, tile._4, tile._1.reverse),
      Tile(id, orientation + "2", tile._3.reverse, tile._4.reverse, tile._1.reverse, tile._2.reverse),
      Tile(id, orientation + "3", tile._4.reverse, tile._1, tile._2.reverse, tile._3))
  }

}

object Day20 extends App {
  new Day20().solvePuzzles("/2020/input-puzzle20.txt")
}
