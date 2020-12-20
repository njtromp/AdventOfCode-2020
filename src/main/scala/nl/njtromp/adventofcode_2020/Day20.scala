package nl.njtromp.adventofcode_2020

import java.util.Objects
import scala.collection.mutable.ListBuffer

class Day20 extends Puzzle {
  case class Tile(id: Long, top: String, right: String, bottom: String, left: String) {
    override def equals(obj: Any): Boolean = {
      obj match {
        case Tile(oId, oTop, oRight, oBottom, oLeft) =>
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

  override def solvePart1(lines: List[String]): Long = {
    val tileData = readTiles(lines)
    val tiles: Set[Tile] = rotateAndFlip(tileData)
    val tops: Map[String, Set[Tile]] = tiles.groupBy(_.top)
    val lefts: Map[String, Set[Tile]] = tiles.groupBy(_.left)
    val dim = Math.sqrt(tileData.size).toInt
    val image: Array[Array[Tile]] = Array.ofDim(dim, dim)

    def createImage(tileNr: Int, usedTiles: Set[Long]): Boolean = {
      if (tileNr >= dim * dim) {
        for (row <- image) {
          for (tile <- row) {
            print(s"${tile.id} ")
          }
          println
        }
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
    -1
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
    rotate(id, (top, right, bottom, left)) ++ rotate(id, (top.reverse, left, bottom.reverse, right))
  }

  def rotate(id: Long, tile: (String, String, String, String)): Set[Tile] = {
    Set(Tile(id, tile._1, tile._2, tile._3, tile._4),
      Tile(id, tile._2, tile._3.reverse, tile._4, tile._1.reverse),
      Tile(id, tile._3.reverse, tile._4.reverse, tile._1.reverse, tile._2.reverse),
      Tile(id, tile._4.reverse, tile._1, tile._2.reverse, tile._3))
  }

}

object Day20 extends App {
  new Day20().solvePuzzles("/input-puzzle20.txt")
}
