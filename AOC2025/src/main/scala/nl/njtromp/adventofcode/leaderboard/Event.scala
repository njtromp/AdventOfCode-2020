package nl.njtromp.adventofcode.leaderboard

import java.time.LocalDateTime

case class Event(startTime: LocalDateTime, event: Int, numberOfDays: Int, ownerId: Int, members: List[Member]) {
}
