package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.Puzzle2

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import scala.collection.mutable
import scala.util.matching.Regex

class Day04 extends Puzzle2 {
  private val NEW_GUARD: Regex = "\\[(.+)] Guard #(\\d+) begins shift".r
  private val FALLS_ASLEEP: Regex = "\\[(.+)] falls asleep".r
  private val WAKES_UP: Regex = "\\[(.+)] wakes up".r

  sealed abstract class Event {
    def dateTime: LocalDateTime
  }
  case class Start(id: Int, start: LocalDateTime) extends Event {
    override def dateTime: LocalDateTime = start
  }
  case class Asleep(asleep: LocalDateTime) extends Event {
    override def dateTime: LocalDateTime = asleep
  }
  case class Awake(awake: LocalDateTime) extends Event {
    override def dateTime: LocalDateTime = awake
  }

  case class Guard(id: Int) {
    val sleeping: mutable.Map[LocalDate, Array[Int]] = mutable.Map[LocalDate, Array[Int]]()
    var startedSleeping: LocalDateTime = null
    def wakeUp(time: LocalDateTime): Unit = {
      for (minute <- startedSleeping.getMinute until time.getMinute) {
        sleeping(startedSleeping.toLocalDate)(minute) = 1
      }
      startedSleeping = null
    }
    def sleep(time: LocalDateTime): Unit = {
      if (!sleeping.contains(time.toLocalDate))
        sleeping(time.toLocalDate) = new Array[Int](60)
      startedSleeping = time
    }
    def totalSleepingMinutes: Int = sleeping.values.map(_.count(_ == 1)).sum
    def sleepiestMinute: Int = (0 until 60).map(m => (m, shiftsSleeping(m))).maxBy(_._2)._1
    def shiftsSleeping(minute: Int): Int = sleeping.values.count(_(minute) == 1)
  }

  private def parseEvents(lines: List[String]): List[Event] = {
    val dateTimeFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
    lines.sorted.map({
      case NEW_GUARD(dateTime, id) => Start(id.toInt, LocalDateTime.parse(dateTime, dateTimeFormat))
      case FALLS_ASLEEP(dateTime) => Asleep(LocalDateTime.parse(dateTime, dateTimeFormat))
      case WAKES_UP(dateTime) => Awake(LocalDateTime.parse(dateTime, dateTimeFormat))
    })
  }

  private def createGuards(events: List[Event]): Map[Int, Guard] = {
    events.filter({
      case _: Start => true
      case _ => false
    })
      .map({case s: Start => (s.id, Guard(s.id))})
      .toMap
  }

  private def registerShifts(events: List[Event], guards: Map[Int, Guard]): Unit = {
    events.foldLeft(Option.empty[Guard])((currentGuard, event) => event match {
      case start: Start =>
        currentGuard match {
          case Some(g) =>
            if (g.startedSleeping != null) {
              if (start.dateTime == g.startedSleeping.toLocalDate)
                g.wakeUp(start.dateTime)
              else
                g.wakeUp(start.dateTime.withMinute(59))
            }
            Some(guards(start.id))
          case None =>
            Some(guards(start.id))
        }
      case wakeup: Awake =>
        currentGuard match {
          case Some(g) => g.wakeUp(wakeup.dateTime)
        }
        currentGuard
      case fallAsleep: Asleep =>
        currentGuard match {
          case Some(g) => g.sleep(fallAsleep.dateTime)
        }
        currentGuard
    })
  }

  override def exampleAnswerPart1: Long = 240
  override def solvePart1(lines: List[String]): Long = {
    val events = parseEvents(lines)
    val guards = createGuards(events)
    registerShifts(events, guards)
    val sleepyGuard = guards.values.toList.maxBy(_.totalSleepingMinutes)
    sleepyGuard.id * sleepyGuard.sleepiestMinute
  }

  override def exampleAnswerPart2: Long = 4455
  override def solvePart2(lines: List[String]): Long = {
    val events = parseEvents(lines)
    val guards = createGuards(events)
    registerShifts(events, guards)
    val sleepyGuard = guards.values.maxBy(g => g.shiftsSleeping(g.sleepiestMinute))
    sleepyGuard.id * sleepyGuard.sleepiestMinute
  }
}

object Day04 extends App {
  new Day04().solvePuzzles("/2018/day04.txt")
}
