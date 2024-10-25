package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

class Day15 extends Puzzle[Long] with RouteFinding {
  private type Pos = (Long, Long)
  private val STOP = 99
  private val ADD = 1
  private val MUL = 2

  private val JUMP_IF_TRUE = 5
  private val JUMP_IF_FALSE = 6
  private val LESS_THEN = 7
  private val EQUALS = 8
  private val ADJUST_BP = 9

  private val INPUT = 3
  private val OUTPUT = 4

  private val POSITION = 0
  private val IMMEDIATE = 1
  private val RELATIVE = 2

  private val NORTH = 0 // 1
  private val EAST = 1  // 4
  private val SOUTH = 2 // 2
  private val WEST = 3  // 3
  private val MOVES = Array(1, 4, 2, 3)

  private val WALL = 0
  private val MOVED = 1
  private val OXYGEN_SYSTEM = 2
  private val OXYGEN = 2
  private val FREE = 3

  private def printMap(robot: Pos, map: mutable.Map[Pos, Int]): Unit =
    println("="*20)
    val minX = map.keySet.minBy(_._1)._1 - 1
    val maxX = map.keySet.maxBy(_._1)._1 + 1
    val minY = map.keySet.minBy(_._2)._2 - 1
    val maxY = map.keySet.maxBy(_._2)._2 + 1
    (minY to maxY).foreach(y =>
      (minX to maxX).foreach(x =>
        if (x, y) == (0, 0) then
          print('o')
        else if (x, y) == robot then
          print('X')
        else if map.keySet.contains(x, y) then
          map(x, y) match
            case WALL => print('#')
            case OXYGEN_SYSTEM => print('O')
            case _ => print('.')
        else
          print(' ')
      )
      println
    )
    println

  private def doMove(pos: Pos, move: Int): Pos =
    move match
      case NORTH => (pos._1, pos._2 - 1)
      case SOUTH => (pos._1, pos._2 + 1)
      case EAST => (pos._1 + 1, pos._2)
      case WEST => (pos._1 - 1, pos._2)

  private def reverse(move: Int): Int =
    move match
      case NORTH => SOUTH
      case EAST => WEST
      case SOUTH => NORTH
      case WEST => EAST

  private def execute(program: Array[Long]): mutable.Map[Pos, Int] =
    val extendedMemory = mutable.Map.empty[Long, Long]
    var ip: Int = 0
    var bp: Int = 0
    var pos: Pos = (0L, 0L)
    val map = mutable.Map.empty[Pos, Int]
    val path = mutable.Stack.empty[Int]
    var move = 0
    var backTracking = false

    def nextMove(): Unit =
      // Try next direction
      while map.keySet.contains(doMove(pos, move)) do
        move += 1
        if move > WEST then
          // Tried all directions :-(
          startBackTracking()
          return
    def startBackTracking(): Unit =
      backTracking = true
      // We need to go back in the opposite direction that we came here
      move = reverse(path.pop())
      if path.isEmpty then
        program(ip + 2) = STOP

    def read(mode: Int, value: Long): Long =
      def readMappedMemory(address: Long): Long =
        if address < program.length then
          program(address.toInt)
        else
          extendedMemory(address)
      mode % 10 match
        case POSITION => readMappedMemory(value)
        case IMMEDIATE => value
        case RELATIVE => readMappedMemory(bp + value)
    def write(mode: Int, address: Long, value: Long): Unit =
      def writeMappedMemory(address: Long): Unit =
        if address < program.length then
          program(address.toInt) = value
        else
          extendedMemory(address) =  value
      mode % 10 match
        case IMMEDIATE => 
          val bla = read(mode, address)
          writeMappedMemory(bla)
        case RELATIVE => writeMappedMemory(bp + address)
        case POSITION => writeMappedMemory(address)

    def biFunction(ip: Int, mode: Int, f: (Long, Long) => Long): Int =
      write(mode / 100, program(ip + 3), f(read(mode, program(ip + 1)), read(mode / 10, program(ip + 2))))
      ip + 4
    def input(ip: Int, mode: Int, input: Long): Int =
      write(mode, program(ip + 1), input)
      ip + 2
    def output(ip: Int, mode: Int): Int =
      val response = read(mode, program(ip + 1))
      if backTracking then
        backTracking = false
        pos = doMove(pos, move)
        // Restore original direction
        move = reverse(move)
      else
        if response == WALL then
          val wallPos = doMove(pos, move)
          map(wallPos) = WALL
        else
          path.push(move)
          pos = doMove(pos, move)
          map(pos) = if response == MOVED then FREE else OXYGEN_SYSTEM
          move = 0
//      printMap(pos, map)
      nextMove()
      ip + 2
    def jumpIfTrue(ip: Int, mode: Int): Int =
      if read(mode, program(ip + 1)) != 0 then
        read(mode / 10, program(ip + 2)).toInt
      else
        ip + 3
    def jumpIfFalse(ip: Int, mode: Int): Int =
      if read(mode, program(ip + 1)) == 0 then
        read(mode / 10, program(ip + 2)).toInt
      else
        ip + 3
    def lessThen(ip: Int, mode: Int): Int =
      write(mode / 100, program(ip + 3), if read(mode, program(ip + 1)) < read(mode / 10, program(ip + 2)) then 1 else 0)
      ip + 4
    def equals(ip: Int, mode: Int): Int =
      write(mode / 100, program(ip + 3), if read(mode, program(ip + 1)) == read(mode / 10, program(ip + 2)) then 1 else 0)
      ip + 4
    def adjustBp(ip: Int, mode: Int): Int =
      bp += read(mode, program(ip + 1)).toInt
      ip + 2

    map(pos) = FREE
    var opcode: Long = program(ip)
    while opcode != STOP do
      opcode % 100 match
        case ADD =>
          ip = biFunction(ip, opcode.toInt / 100, (a, b) => a + b)
        case MUL =>
          ip = biFunction(ip, opcode.toInt / 100, (a, b) => a * b)
        case INPUT =>
          ip = input(ip, opcode.toInt / 100, MOVES(move))
        case OUTPUT =>
          ip = output(ip, opcode.toInt / 100)
        case JUMP_IF_TRUE =>
          ip = jumpIfTrue(ip, opcode.toInt / 100)
        case JUMP_IF_FALSE =>
          ip = jumpIfFalse(ip, opcode.toInt / 100)
        case LESS_THEN =>
          ip = lessThen(ip, opcode.toInt / 100)
        case EQUALS =>
          ip = equals(ip, opcode.toInt / 100)
        case ADJUST_BP =>
          ip = adjustBp(ip, opcode.toInt / 100)
      opcode = program(ip)
    println
//    printMap((0, 0), map)
    map

  private val moves = List(NORTH, EAST, SOUTH, WEST)
  @tailrec
  private def fillWithOxygen(time: Long, filled: Set[Pos], map: mutable.Map[(Long, Long), Int]): Long =
    if filled.isEmpty then
      time - 1
    else
      val newlyFilled = filled.flatMap(p => moves.map(doMove(p, _)))
        .filter(map(_) == FREE)
      newlyFilled.foreach(map(_) = OXYGEN)
      fillWithOxygen(time + 1, newlyFilled, map)

  override def exampleAnswerPart1: Long = 0
  override def solvePart1(lines: List[String]): Long =
    if lines.isEmpty then return 0
    val program = lines.head.split(",").map(_.toLong)
    val map = execute(program)
    val start = (0L, 0L)
    val finish = map.filter(_._2 == OXYGEN_SYSTEM).head._1
    val route = bfs(start, finish, p => moves.map(m => doMove(p, m)).filter(map(_) != WALL))
    route.size - 1

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    if lines.isEmpty then return 0
    val program = lines.head.split(",").map(_.toLong)
    val map = execute(program)
    map((0, 0)) = FREE
    val oxygenSystem: Pos = map.filter(_._2 == OXYGEN_SYSTEM).head._1
    fillWithOxygen(0L, Set(oxygenSystem), map)

}

object Day15 extends App {
  new Day15().solvePuzzles()
}
