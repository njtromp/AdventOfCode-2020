package nl.njtromp.adventofcode

import scala.collection.mutable

class Day18 extends Puzzle[Long] with RouteFinding {
  private val OPEN = '.'

  private val openedDoors = mutable.Stack.empty[Char]

  private def openDoor(map: SimpleMap[Char])(keyPos: Pos): (Char, Option[Pos]) =
    val keyId = map(keyPos)
    openedDoors.push(keyId)
    map(keyPos) = OPEN
    val doorPos = map.find(d => d.isUpper && d.toLower == keyId)
    if doorPos.nonEmpty then
      map(doorPos.head) = OPEN
      (keyId, Some(doorPos.head))
    else
      (keyId, None)

  private def closeDoor(map: SimpleMap[Char])(keyPos: Pos, door: (Char, Option[Pos])): Unit =
    openedDoors.pop()
    map(keyPos) = door._1
    door._2.foreach(map(_) = door._1.toUpper)

  private def neighbours(map: SimpleMap[Char])(pos: Pos): List[Pos] =
    map.neighborPositions(pos, square).filter(p => map(p).isLower || map(p) == OPEN)

  private def openAllDoors(map: SimpleMap[Char]): List[(Pos, Char)] =
    map.find(_.isUpper).map(p =>
      val n = map(p)
      map(p) = OPEN
      (p, n)
    )

  private def closeAllDoors(openedDoors: List[(Pos, Char)], map: SimpleMap[Char]): Unit =
    openedDoors.foreach((p, d) => map(p) = d)

  private def determineDistancesBetweenKeys(keys: Set[Pos], map: SimpleMap[Char]): Map[(Pos, Pos), Int] =
    keys.flatMap(k1 =>
      keys.filterNot(_ == k1)
        .map(k2 => ((k1, k2), bfs(k1, k2, neighbours(map)).length))
    ).toMap

  private def reachableKeys(start: Pos, keys: Set[Pos], map: SimpleMap[Char]): Set[Pos] =
    keys.filter(p => bfs(start, p, neighbours(map)).nonEmpty)

//  private def neighbours(current: Pos, allKeys: Set[Pos], map: SimpleMap[Char]): Set[Pos] =
//    allKeys.filterNot(_ == current).filter(n => bfs(current, n, neighbours(map)).nonEmpty)

  private def determineHiddenKeys(start: Pos, allKeys: Set[Pos], map: SimpleMap[Char]): Map[Pos, Set[Pos]] =
    val closedDoors = map.find(_.isUpper).toSet
    val doorIds = closedDoors.map(map(_).toLower)
    val keysForClosedDoors = mutable.Queue.from(allKeys.filter(p => doorIds.contains(map(p))))
    val hiddenKeys = mutable.Map.empty[Pos, Set[Pos]]
    var hiddenKeysCount = hiddenKeys.values.map(_.size).sum
    while keysForClosedDoors.nonEmpty do
      val alreadyReachable = reachableKeys(start, allKeys, map)
      val key = keysForClosedDoors.dequeue()
      val doorInfo = openDoor(map)(key)
      val newlyReachable = reachableKeys(start, allKeys, map) -- alreadyReachable
      if newlyReachable.nonEmpty then
        hiddenKeys(key) = newlyReachable
        map(key) = doorInfo._1
      else
        closeDoor(map)(key, doorInfo)
        // Let's try later
        keysForClosedDoors.enqueue(key)
      val bla = hiddenKeys.values.map(_.size).sum
      if bla == hiddenKeysCount then
        println(map.asString())
        println(keysForClosedDoors)
      else
        hiddenKeysCount = bla
    hiddenKeys.toMap

  private def optimizeRoute(start: Pos, map: SimpleMap[Char]): Long =
    val allKeys = map.find(_.isLower).toSet
    val reachableFromStart = reachableKeys(start, allKeys, map)
    val openedDoors = openAllDoors(map)
    val distances = reachableFromStart.map(n => ((start, n), bfs(start, n, neighbours(map)).size)).toMap ++
      determineDistancesBetweenKeys(allKeys, map)
    closeAllDoors(openedDoors, map)

    val distanceToStart = mutable.Map.empty[Pos, Int].withDefaultValue(Int.MaxValue)

    val source = mutable.Map.empty[Pos, Pos]
    val toBeVisited = mutable.PriorityQueue.empty[Pos](Ordering.by(p => -distanceToStart(p)))
    val visited = mutable.Set.empty[Pos]
    distanceToStart += start -> 0
    toBeVisited.enqueue(start)
    val usedKeys = mutable.Queue.empty[Pos]
    while (toBeVisited.nonEmpty)
      val current = toBeVisited.dequeue()
//      if (current == finish)
//        return reconstructPath(start, finish, source)
      // The queue might hold positions with an 'old' (lower) priority so if we encounter one of them, we must skip it.
      if (!visited.contains(current))
        usedKeys.enqueue(current)
        visited += current
        openDoor(map)(current)
        val length = distanceToStart(current)
        val neighbors = allKeys.filterNot(_ == current).filter(n => bfs(current, n, neighbours(map)).nonEmpty)
        neighbors.foreach(n => {
          if (!visited.contains(n))
            toBeVisited.enqueue(n)
          // Relax
          val newDistance = length + distances(current, n)
          if (newDistance < distanceToStart(n))
            distanceToStart += n -> newDistance
            source += n -> current
        })
    usedKeys.zip(usedKeys.tail).map(distances).sum


  //    val distanceToStart = mutable.Map.empty[Pos, Int].withDefaultValue(Int.MaxValue)
//    val investigate = mutable.PriorityQueue.empty[Pos](Ordering.by(p => -distanceToStart(p)))
//    val source = mutable.Map.empty[Pos, Pos]
//    val visited = mutable.Set.empty[Pos]
//    var lastKey = start
//    investigate.enqueue(start)
//    distanceToStart(start) = 0
//    while investigate.nonEmpty do
//      val current = investigate.dequeue()
//      if !visited.contains(current) then
//        lastKey = current
//        visited += current
//        if map(current).isLower then
//          openDoor(map)(current)
//        neighbours(map)(current).foreach(n =>
//          if !visited.contains(n) then
//            investigate += n
//            if distanceToStart(current) + 1 < distanceToStart(n) then
//              distanceToStart(n) = distanceToStart(current) + 1
//              source(n) = current
//        )
//    reconstructPath(start, lastKey, source)

  override def exampleAnswerPart1: Long = 11//136
  override def solvePart1(lines: List[String]): Long =
    println("5738 is too high")
    val map = SimpleMap[Char](lines, _.toCharArray)
    val start = map.find('@').head
    map(start) = OPEN

//    val allKeys = map.find(_.isLower).toSet
//    val openedDoors = openAllDoors(map)
//    val distances = determineDistancesBetweenKeys(allKeys, map)
//    closeAllDoors(openedDoors, map)
//    val hiddenKeys = determineHiddenKeys(start, allKeys, map)
//    closeAllDoors(openedDoors, map)
//    hiddenKeys.foreach((k, ds) =>
//      print(s"${map(k)} -> ")
//      println(ds.map(map(_)).mkString(", "))
//    )
//    println(map.asString())

    val result = optimizeRoute(start, map)
    result


  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day18 extends App {
  new Day18().solvePuzzles()
}
