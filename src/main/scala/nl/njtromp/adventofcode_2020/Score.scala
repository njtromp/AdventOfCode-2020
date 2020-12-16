package nl.njtromp.adventofcode_2020

import play.api.libs.json.{JsObject, Json}

object Score extends App {
  val leaderBoard = Json.parse(getClass.getResourceAsStream("/leaderboard.json"))
  val fields = (leaderBoard \ "members").as[JsObject].values
  val contenders: List[(String, List[(Int, Int)])] = fields.map(field => {
    val name = if (field.as[JsObject].values.count(_.toString() == "null") == 1) s"coward-${(field \ "id").as[String]}" else (field \ "name").as[String]
    val completions = (field \ "completion_day_level").as[JsObject]
    (name, completions.keys.map(p => {
      val stars = completions.value(p).as[JsObject]
      val star1 = if (stars.keys.contains("1")) (stars \ "1" \ "get_star_ts").as[String].toInt else 0
      val star2 = if (stars.keys.contains("2")) (stars \ "2" \ "get_star_ts").as[String].toInt else 0
      (star1, star2)
    }).toList
    )
  }).toList

  contenders.sortWith(sortBySpeedOfSecondPart).foreach(c => println(s"${stars(c)} stars ${timeNeededForSecondPart(c) / Math.max(1, stars(c)) / 60} (minutes) avg. for part two -> ${c._1}"))

  def stars(c: (String, List[(Int, Int)])) : Int = {
    c._2.flatMap(s => List(s._1, s._2)).count(_ > 0)
  }

  private def timeNeededForSecondPart(c: (String, List[(Int, Int)])): Int = {
    c._2.map(s => s._2 - s._1).filter(_ > 0).sum
  }

  private def sortBySpeedOfSecondPart(c1: (String, List[(Int, Int)]), c2: (String, List[(Int, Int)])): Boolean = {
    // First the number of days participated
    var comp = c1._2.size - c2._2.size
    if (comp == 0) {
      // Then the number of stars for the second part
      comp = c1._2.count(_._2 > 0) - c2._2.count(_._2 > 0)
    }
    if (comp == 0) {
      // Then the number of stars for the first part
      comp = c1._2.count(_._1 > 0) - c2._2.count(_._1 > 0)
    }
    if (comp == 0) {
      // Then the total time needed for solving part 2
      comp = timeNeededForSecondPart(c2) - timeNeededForSecondPart(c1)
    }
    comp > 0
  }
}
