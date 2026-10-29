package nl.njtromp.adventofcode.leaderboard

import io.circe.{Decoder, HCursor}
import io.circe.jawn.decode
import sttp.client4.Response
import sttp.client4.quick.*
import sttp.model.StatusCode
import sttp.model.StatusCode.Ok

import java.io.{File, FileWriter}
import java.time.{LocalDateTime, ZoneOffset}
import scala.io.Source

object LeaderBoard {
  def unixTime(timeStamp: Int): LocalDateTime =
    LocalDateTime.ofEpochSecond(timeStamp, 0, ZoneOffset.ofHours(1))

  implicit val decodeStar: Decoder[Star] = (c: HCursor) => for {
    starTimeStamp <- c.downField("get_star_ts").as[Int]
    starIndex <- c.downField("star_index").as[Int]
  } yield {
    Star(unixTime(starTimeStamp), starIndex)
  }

  implicit val decodeDay: Decoder[Day] = (c: HCursor) => for {
    star1 <- c.downField("1").as[Star]
    star2 <- c.downField("2").as[Option[Star]]
  } yield {
    Day(star1, star2)
  }

  implicit val decodeMember: Decoder[Member] = (c: HCursor) => for {
    id <- c.downField("id").as[Int]
    lastStar <- c.downField("last_star_ts").as[Int]
    score <- c.downField("local_score").as[Int]
    name <- c.downField("name").as[Option[String]]
    stars <- c.downField("stars").as[Int]
    days <- c.downField("completion_day_level").as[Map[String, Day]]
  } yield {
    val sortedDays = days.toList.sortBy[Int](_._1.toInt).map(_._2)
    Member(id, name.getOrElse("COWARD"), score, stars, unixTime(lastStar), sortedDays)
  }

  implicit val decoreEvent: Decoder[Event] = (c: HCursor) => for {
    startTime <- c.downField("day1_ts").as[Int]
    event <- c.downField("event").as[Int]
    numberOfDays <- c.downField("num_days").as[Int]
    ownerId <- c.downField("owner_id").as[Int]
    members <- c.downField("members").as[Map[String, Member]]
  } yield {
    Event(unixTime(startTime), event, numberOfDays, ownerId, members.filter(_._2.score > 0).values.toList)
  }

  def showLeaderBoard(year: Int, cookie: String): Unit =
    val scoring: Response[String] = quickRequest.get(uri"https://adventofcode.com/$year/leaderboard/private/view/1030938.json")
      .cookie("session", cookie).send()
    if scoring.code == Ok then {
      val writer = new FileWriter(new File(getClass.getResource("/leaderboard.json").getFile))
      writer.write(scoring.body)
      writer.close()
    }

    val json = Source.fromInputStream(getClass.getResourceAsStream("/leaderboard.json")).getLines().mkString
    decode[Event](json) match {
      case Left(error) => Console.err.println(error)
      case Right(result) =>
        result.members
          .sortBy[Int](_.score)
          .reverse
          .foreach(m => println(f"${m.score}%7s : (${m.stars}) ${m.name}"))
    }
}

@main def showLeaderBoard(args: String*): Unit =
  val source = Source.fromResource(".session.cookie")
  val cookie = try source.mkString.trim finally source.close()
  LeaderBoard.showLeaderBoard(2025, cookie)