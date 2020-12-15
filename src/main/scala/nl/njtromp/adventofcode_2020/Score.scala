package nl.njtromp.adventofcode_2020

import play.api.libs.json.{JsDefined, JsObject, Json}

object Score extends App {
  val leaderBoard = Json.parse(getClass.getResourceAsStream("/leaderboard.json"))
  val fields = (leaderBoard \ "members").as[JsObject].values
  val contenders: List[(String, List[(Int, Int)])] = fields.map(field => {
    val name = if (field.as[JsObject].values.count(_.toString() == "null") == 1) s"anonymous-coward: ${(field \ "id").as[String]}" else (field \ "name").as[String]
    val completions = (field \ "completion_day_level").as[JsObject]
    (name, completions.keys.map(p => {
      val stars = completions.value(p).as[JsObject]
      val star1 = if (stars.keys.contains("1")) (stars \ "1" \ "get_star_ts").as[String].toInt else 0
      val star2 = if (stars.keys.contains("2")) (stars \ "2" \ "get_star_ts").as[String].toInt else 0
      (star1, star2)
    }).toList
    )
  }).toList
  contenders.sortBy(c => -c._2.size).foreach(println(_))
}
