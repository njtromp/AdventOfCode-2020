package nl.njtromp

import scala.annotation.tailrec

class Quento {
  val tiles = Set(
    (0, 0), (0, 1), (0, 2),
    (1, 0), (1, 1), (1, 2),
    (2, 0), (2, 1), (2, 2),
  )
  val startTiles = Set(
    (0, 0), (0, 2),
    (1, 1),
    (2, 0), (2, 2),
  )
  val moves = Set((0, -1), (0, 1), (-1, 0), (1, 0))
  val puzzle = Map(
    (0, 0) -> "7", (0, 1) -> "+", (0, 2) -> "8",
    (1, 0) -> "-", (1, 1) -> "2", (1, 2) -> "-",
    (2, 0) -> "6", (2, 1) -> "+", (2, 2) -> "4",
  )

  private def paths(numberOfDigits: Int): List[List[(Int, Int)]] = {
    startTiles.flatten(tile => paths(List(tile), tiles - tile, 2 * numberOfDigits - 2)).toList
  }

  private def paths(partialPath: List[(Int, Int)], tilesLeft: Set[(Int, Int)], numberOfTiles: Int): List[List[(Int, Int)]] = numberOfTiles match {
    case 0 => List(partialPath)
    case _ => moves.map(move => (partialPath.head._1 + move._1, partialPath.head._2 + move._2))
        .intersect(tilesLeft)
        .flatten(tile => paths(tile :: partialPath, tilesLeft - tile, numberOfTiles - 1)).toList
  }

  private def map(path: List[(Int, Int)]): List[String] = {
    path.map(tile => puzzle(tile))
  }

  private def evaluate(instructions: List[String]): Int = {
    evaluate(instructions.head.toInt, instructions.tail)
  }

  @tailrec
  private def evaluate(value: Int, instructions: List[String]): Int = instructions match {
    case Nil => value
    case op :: digit :: remainder => op match {
      case "-" => evaluate(value - digit.toInt, remainder)
      case "+" => evaluate(value + digit.toInt, remainder)
    }
  }
}

object Quento extends App {
  val quento = new Quento
  for (n <- 2 to 5) {
    println("="*20)
    println(n)
    println("-"*20)
    val paths = quento.paths(n)
    val uniqueLegalPaths = paths.map(path => (quento.evaluate(quento.map(path)), quento.map(path))).filter(p => p._1 >= 0).groupBy(_._1)
    uniqueLegalPaths.keys.toList.sorted.foreach(result => println(s"${result} => ${uniqueLegalPaths(result).map(_._2)}"))
  }
}
