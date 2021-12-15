package nl.njtromp.adventofcode_2021

import play.api.libs.json.{JsObject, Json}

object Score extends App {
  val leaderBoard = Json.parse(getClass.getResourceAsStream("/2021/leaderboard.json"))
  val fields = (leaderBoard \ "members").as[JsObject].values
  val contenders: List[(String, List[(Int, Int)])] = fields.map(field => {
    val name = if (field.as[JsObject].values.count(_.toString() == "null") == 1) s"coward-${(field \ "id").as[String]}" else (field \ "name").as[String]
    val completions = (field \ "completion_day_level").as[JsObject]
    (name, completions.keys.map(p => {
      val stars = completions.value(p).as[JsObject]
      val star1 = if (stars.keys.contains("1")) (stars \ "1" \ "get_star_ts").as[Int] else 0
      val star2 = if (stars.keys.contains("2")) (stars \ "2" \ "get_star_ts").as[Int] else 0
      (star1, star2)
    }).toList
    )
  }).toList

  val days = contenders.map(_._2.size).max

  val leaders = contenders.filter(_._2.size == days)
  scorePerDay(leaders).foreach(s => println(s"${s._2} ${s._1}"))

  def scorePerDay(leaders: List[(String, List[(Int, Int)])]): List[(String, Int)] = {
    def dayScore(timings: List[(String, Int, Int)]): List[(String, Int)] = {
      timings.sortBy(t => t._2).zipWithIndex.sortBy(t => t._1._3).zipWithIndex.map(s => (s._1._1._1, s._2 + s._1._2))
    }
    def determineScores(timings: List[List[(String, Int, Int)]]): List[(String, Int)] = {
      if (timings.isEmpty)
        List.empty
      else
        dayScore(timings.head) ++ determineScores(timings.tail)
    }
    val dayScores = leaders.map(d => d._2.map(s => (d._1, s._1, s._2)))
    determineScores(dayScores).groupBy(_._1).map(s => (s._1, s._2.map(_._2).sum)).toList.sortBy(_._2).map(a => (a._1, 2 * leaders.length * days - a._2))
  }

}
