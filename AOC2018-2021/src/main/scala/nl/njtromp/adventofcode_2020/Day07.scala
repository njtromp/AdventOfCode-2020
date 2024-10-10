package nl.njtromp.adventofcode_2020

import scala.collection.mutable
import scala.io.Source

object Day07 extends App {
  var bags: mutable.Map[String, List[(String, Int)]] = mutable.Map.empty
  val outerBagInfo = "(.+) bags contain (.+)\\.".r
  val innerBagInfo = "(\\d+) (.+) bag(s?)".r
  for (line <- Source.fromInputStream(Day01.getClass.getResourceAsStream("/2020/input-puzzle07.txt")).getLines) {
    line match {
      case outerBagInfo(outerColor, innerBagsInfo) => {
        var innerBags: List[(String, Int)] = bags.getOrElse(outerColor, List.empty)
        for (innerBag <- innerBagsInfo.split(", ")) {
          innerBag match {
            case innerBagInfo(number, innerColor, _) => innerBags = (innerColor, number.toInt) +: innerBags
            case "no other bags" => // Nothing
            case _ => println(s"No match for inner bag [${innerBag}]")
          }
        }
        bags += (outerColor -> innerBags)
      }
      case _ => println(s"No match :'-( [${line}]");
    }
  }

  var answerPart1 = 0
  for (bagColor <- bags.keySet) {
    if (!"shiny gold".equals(bagColor)) {
      answerPart1 += (if (leadsToShinyGold(bagColor)) 1 else 0)
    }
  }
  println(s"Answer part 1: $answerPart1")

  val answerPart2 = countBags(bags("shiny gold"))
  println(s"Answer part 2: $answerPart2")

  def leadsToShinyGold(bagColor: String): Boolean = {
    bagColor match {
      case "shiny gold" => true
      case _ => bags(bagColor).exists(b => leadsToShinyGold(b._1))
    }
  }

  def countBags(innerBags: List[(String, Int)]): Int = {
    innerBags.map(innerBag => (innerBag._2 + innerBag._2 * countBags(bags(innerBag._1)))).sum
  }

}
