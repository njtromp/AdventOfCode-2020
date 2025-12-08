package nl.njtromp.adventofcode.leaderboard

import java.time.LocalDateTime

case class Member(id: Int, name: String, score: Int, stars: Int, lastStar: LocalDateTime, days: List[Day])
