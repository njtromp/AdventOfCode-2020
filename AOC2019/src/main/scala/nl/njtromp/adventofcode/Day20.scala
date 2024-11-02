package nl.njtromp.adventofcode

class Day20 extends Puzzle[Long] with RouteFinding {

  override def exampleAnswerPart1: Long = 23
  override def solvePart1(lines: List[String]): Long =
    val ls = if lines.length == 56 then lines.take(19) else lines
    val map = SimpleMap[Char](ls, _.toCharArray)
    val dots = map.find('.').toSet
    val chars = map.find(_.isLetter).toSet
    def order(p1: Pos, p2: Pos): (Pos, Pos) =
      if p1._1 < p2._1 || p1._2 < p2._2 then
        (p1, p2)
      else
        (p2, p1)
    def id(d: Pos, c1: Pos): String =
      val c2 = (c1._1 + (c1._1 - d._1), c1._2 + (c1._2 - d._2))
      val (p1, p2) = order(c1, c2)
      s"${map(p1)}${map(p2)}"
    val portals = chars
      .map(c => (c, map.neighborPositions(c, square).filter(dots.contains)))
      .filter(_._2.nonEmpty)
      .map(cd => (cd._2.head, id(cd._2.head, cd._1)))
    def followPortal(p: Pos): List[Pos] =
      val portal = portals.filter(_._1 == p)
      if portal.isEmpty then
        List.empty[Pos]
      else
        portals
          .filter(_._2 == portal.head._2)
          .map(_._1)
          .toList
    def neighbours(p: Pos): List[Pos] =
      (followPortal(p) ++ map.neighborPositions(p, square).filter(dots.contains)).filterNot(_ == p)
    val start = portals.filter(_._2 == "AA").head._1
    val finish = portals.filter(_._2 == "ZZ").head._1
    val path = bfs(start, finish, neighbours)
    path.length - 1

  override def exampleAnswerPart2: Long = 396
  override def solvePart2(lines: List[String]): Long =
    println("Please run with at least 64MB stackspace, eg: -Xss64m")
    val ls = if lines.length == 56 then lines.drop(19) else lines
    val map = SimpleMap[Char](ls, _.toCharArray)
    val dots = map.find('.').toSet
    val chars = map.find(_.isLetter).toSet
    def order(p1: Pos, p2: Pos): (Pos, Pos) =
      if p1._1 < p2._1 || p1._2 < p2._2 then
        (p1, p2)
      else
        (p2, p1)
    def id(d: Pos, c1: Pos): String =
      val c2 = (c1._1 + (c1._1 - d._1), c1._2 + (c1._2 - d._2))
      val (p1, p2) = order(c1, c2)
      s"${map(p1)}${map(p2)}"
    val portals = chars
      .map(c => (c, map.neighborPositions(c, square).filter(dots.contains)))
      .filter(_._2.nonEmpty)
      .map(cd => (cd._2.head, id(cd._2.head, cd._1)))
    val outerPortals = portals.filter(
      p => p._1._2 == 2 ||
        p._1._2 == map.width - 3 ||
        p._1._1 == 2 ||
        p._1._1 == map.height - 3
    )
    val innerPortals = portals.diff(outerPortals)
    def followPortal(p: (Pos, Int)): List[(Pos, Int)] =
      val innerPortal = innerPortals.filter(_._1 == p._1)
      val outerPortal = outerPortals.filter(_._1 == p._1)
      (innerPortal.nonEmpty, outerPortal.nonEmpty) match
        case (true, false) => // At an inner portal, level is increasing
          outerPortals.filter(_._2 == innerPortal.head._2)
            .map(o => (o._1, p._2 + 1))
            .toList
        case (false, true) => // At an outer portal, level is decreasing
          if p._2 == 0 then // At level 0 there are no outer portals
            List.empty[(Pos, Int)]
          else
            innerPortals.filter(_._2 == outerPortal.head._2)
              .map(i => (i._1, p._2 - 1))
              .toList
        case (false, false) => // Somewhere in between
          List.empty[(Pos, Int)] // No portal
        case (true, true) =>
          ??? // Can never happen, but it prevents the compiler from complaining ;-)
    def neighbours(p: (Pos, Int)): List[(Pos, Int)] =
      followPortal(p) ++
        map.neighborPositions(p._1, square)
          .filter(dots.contains)
          .map((_, p._2)) // Add level
    val start = portals.filter(_._2 == "AA").head._1
    val finish = portals.filter(_._2 == "ZZ").head._1
    val path = bfs((start, 0), (finish, 0), neighbours)
    path.length - 1

}

object Day20 extends App {
  new Day20().solvePuzzles(false)
}
