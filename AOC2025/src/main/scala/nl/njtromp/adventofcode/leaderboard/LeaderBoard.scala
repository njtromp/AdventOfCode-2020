package nl.njtromp.adventofcode.leaderboard

import io.circe.{Decoder, HCursor}
import io.circe.generic.semiauto.deriveDecoder
import io.circe.jawn.decode

import java.time.{LocalDateTime, ZoneOffset}
import scala.io.Source

class LeaderBoard {
}

object LeaderBoard extends App {
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
    Member(id, name.getOrElse("COWARD"), score, stars, unixTime(lastStar), days.values.toList)
  }

  implicit val decoreEvent: Decoder[Event] = (c: HCursor) => for {
    startTime <- c.downField("day1_ts").as[Int]
    event <- c.downField("event").as[Int]
    numberOfDays <- c.downField("num_days").as[Int]
    ownerId <- c.downField("owner_id").as[Int]
    members <- c.downField("members").as[Map[String, Member]]
  } yield {
    Event(unixTime(startTime), event, numberOfDays, ownerId, members.values.toList)
  }

  val json = Source.fromInputStream(getClass.getResourceAsStream("/results.json")).getLines().mkString

  decode[Event](json) match {
    case Left(error) => Console.err.println(error)
    case Right(result) =>
      result.members
        .filter(_.score > 0)
        .sortBy[Int](_.score)
        .reverse
        .foreach(m => println(f"${m.score}%7s : ${m.name}"))
  }
}
