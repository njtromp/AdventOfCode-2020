package nl.njtromp.adventofcode

import scala.collection.mutable
import scala.io.StdIn.readLine

class Day25 extends Puzzle[Long] {
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

  // Step 1: Manually walk through the ship and create a map of the ship with information on items found
  //         see src/main/resources/day-map.xlsx
  // Step 2: Manually pick up items and mark the items that keep the game going green and red otherwise
  // Step 3: Programmatically gather all items and go to the Security Check
  // Step 4: Drop items from inventory
  // Step 5: Pickup a number of items using bit-mapped itemMask variable
  // Step 6: Go north, if fails, goto step 4, otherwise program terminates

  // A lot of the code was used to find the solution and is now redundant
  private enum InputSource:
    case Instructions, Items, CommandLine
  private enum ItemState:
    case DropItems, TakeItems

  private var inputSource = InputSource.Instructions
  private var itemState = ItemState.DropItems
  private val items = List(
    "astronaut ice cream",
    "wreath",
    "coin",
    "dehydrated water",
    "asterisk",
    "monolith",
    "astrolabe",
    "mutex"
  )
  // Bit-mapped mask for the items that we will take before trying to go north in at the Security Check
  private var itemMask = (1 << (items.length - 1)) - 1 // We arrive with all the items
  private var instructionIndex = 0
  // All disabled instructions took part in the automatic gathering of the items
  // and are not needed for the final solution.
  private var instructions = List(
    "south",
    "take monolith",
    "east",
    "take asterisk",
    "west",
    "north",
//    "west",
//    "take coin",
//    "north",
//    "east",
//    "take astronaut ice cream",
//    "west",
//    "south",
//    "east",
    "north",
    "north",
//    "take mutex",
    "west",
    "take astrolabe",
    "west",
//    "take dehydrated water",
    "west",
    "take wreath",
    "east",
    "south",
    "east",
    "north",
//    "inv", // Just to ensure that we have every possible item
    "north"
  ).mkString("", "\n", "\n")
  private var result = 0L
  private def execute(program: Array[Long]): Long =
    var ip: Int = 0
    var bp: Int = 0
    val extendedMemory = mutable.Map.empty[Long, Long]
    var continuousDigits = false  // The description of Engineering contains a digit that we must skip

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
      val value = read(mode, program(ip + 1))
      if value == '\n' then
        println
      else {
        val char = value.toChar
        print(char)
        if Range('0', '9').contains(char) then
          if !continuousDigits then
            result = 0
          result = result * 10 + char.asDigit
          continuousDigits = true
        else
          continuousDigits = false
      }
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

    var commandIndex = 0
    var command = ""
    var opcode: Long = program(ip)
    while opcode != STOP do
      opcode % 100 match
        case ADD =>
          ip = biFunction(ip, opcode.toInt / 100, (a, b) => a + b)
        case MUL =>
          ip = biFunction(ip, opcode.toInt / 100, (a, b) => a * b)
        case INPUT =>
          inputSource match
            case InputSource.Instructions =>
              ip = input(ip, opcode.toInt / 100, instructions.charAt(instructionIndex))
              instructionIndex += 1
              if instructionIndex >= instructions.length then
                inputSource = InputSource.Items
            case InputSource.Items =>
              itemState match
                case ItemState.DropItems =>
                  instructions = items.zipWithIndex
                    .filter(ii => ((1 << ii._2) & itemMask) != 0)
                    .map(ii => s"drop ${ii._1}")
                    .mkString("", "\n", "\n")
                  itemMask -= 1
                  instructionIndex = 0
                  itemState = ItemState.TakeItems
                case ItemState.TakeItems =>
                    instructions = ("north" :: items.zipWithIndex
                      .filter(ii => ((1 << ii._2) & itemMask) != 0)
                      .map(ii => s"take ${ii._1}"))
                      .reverse
                      .mkString("", "\n", "\n")
                    instructionIndex = 0
                    itemState = ItemState.DropItems
              inputSource = InputSource.Instructions
            case InputSource.CommandLine =>
              if command.isEmpty then
                command = readLine + "\n"
              else
                ip = input(ip, opcode.toInt / 100, command.charAt(commandIndex))
                commandIndex += 1
                if commandIndex >= command.length then
                  command = ""
                  commandIndex = 0
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
    result

  override def exampleAnswerPart1: Long = 0
  override def solvePart1(lines: List[String]): Long =
    if lines.isEmpty then return 0
    execute(lines.head.split(",").map(_.toLong))


  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day25 extends App {
  new Day25().solvePuzzles()
}
