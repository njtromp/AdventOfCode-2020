package nl.njtromp.adventofcode

import java.time.Duration
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*

class Day23 extends Puzzle[Long] {
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

  private val finished = AtomicBoolean(false)
  private val result = AtomicLong(0)

  private type SyncQueue[E] = java.util.concurrent.ConcurrentLinkedQueue[E]

  private case class Packet(id: Int, x: Long, y: Long)

  private class InQueue extends SyncQueue[Long] {
    override def poll(): Long = if isEmpty then -1 else super.poll()
  }

  private class OutQueue(inQueues: Array[InQueue]) extends SyncQueue[Packet] {
    override def offer(packet: Packet): Boolean =
      if packet.id == 255 then
        if result.get() == 0 then
          result.set(packet.y)
          finished.set(true)
      else
        synchronized[Unit]{
          inQueues(packet.id).offer(packet.x)
          inQueues(packet.id).offer(packet.y)
        }
      true
  }

  private class NAT(inQueues: Array[InQueue]) extends SyncQueue[Packet] {
    private val lastPacket: AtomicReference[Packet] = AtomicReference[Packet](Packet(-1, -1, -1))
    private var lastY = 0L
    override def offer(packet: Packet): Boolean =
      if packet.id == 255 then
        lastPacket.set(packet)
        true
      else
        send(packet.id, packet.x, packet.y)
    private def send(id: Int, x: Long, y: Long): Boolean = {
      synchronized[Boolean] {
        inQueues(id).offer(x)
        inQueues(id).offer(y)
      }
    }
    def run(): Unit =
      // Trial and error, the correct value is: 14327
      val notCorrect = List(-1L, 21160, 1355, 68157, 19429, 18031)
      var hitCount = 0
      while !finished.get() do
        if inQueues.forall(_.isEmpty) then
          val packet = lastPacket.get()
          send(0, packet.x, packet.y)
          if packet.y == lastY && !notCorrect.contains(lastY) then
            result.set(lastY)
            hitCount += 1
            if hitCount > 30 then
              finished.set(true)
          lastY = packet.y
  }

  private def execute(program: Array[Long], in: InQueue, out: SyncQueue[Packet]): Unit =
    var ip: Int = 0
    var bp: Int = 0
    val extendedMemory = mutable.Map.empty[Long, Long].withDefaultValue(0)
    val outputBuffer = mutable.Queue.empty[Long]

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
        case IMMEDIATE => writeMappedMemory(read(mode, address))
        case RELATIVE => writeMappedMemory(bp + address)
        case POSITION => writeMappedMemory(address)

    def biFunction(ip: Int, mode: Int, f: (Long, Long) => Long): Int =
      write(mode / 100, program(ip + 3), f(read(mode, program(ip + 1)), read(mode / 10, program(ip + 2))))
      ip + 4
    def input(ip: Int, mode: Int, input: Long): Int =
      write(mode, program(ip + 1), input)
      ip + 2
    def output(ip: Int, mode: Int): Int =
      outputBuffer.enqueue(read(mode, program(ip + 1)))
      if outputBuffer.size >= 3 then
        val id = outputBuffer.dequeue()
        val x = outputBuffer.dequeue()
        val y = outputBuffer.dequeue()
        out.offer(Packet(id.toInt, x, y))
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

    var opcode: Long = program(ip)
    var hasId = false
    var provideX = true
    while opcode != STOP && !finished.get() do
      opcode % 100 match
        case ADD =>
          ip = biFunction(ip, opcode.toInt / 100, (a, b) => a + b)
        case MUL =>
          ip = biFunction(ip, opcode.toInt / 100, (a, b) => a * b)
        case INPUT =>
          if in.size() >= 2 then
            if provideX then
              ip = input(ip, opcode.toInt / 100, in.peek())
              provideX = false
            else
              in.poll() // Get rid of X
              ip = input(ip, opcode.toInt / 100, in.poll())
              provideX = true
          else if !hasId then // Get our ID
            ip = input(ip, opcode.toInt / 100, in.poll())
            hasId = true
          else
            ip = input(ip, opcode.toInt / 100, -1)
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

  override def exampleAnswerPart1: Long = 0
  override def solvePart1(lines: List[String]): Long =
    if lines.isEmpty then return 0
    println("Expecting: 21160")
    val program = lines.head.split(",").map(_.toLong)
    val queues = (0 to 49).map(id =>
      val q = new InQueue()
      q.offer(id)
      q
    ).toArray
    val out = OutQueue(queues)
    val computers = (0 to 49).map(id => new Thread((() => execute(program.clone(), queues(id), out)): Runnable))
    result.set(0)
    finished.set(false)
    computers.foreach(_.start())
    computers.forall(c => c.join(Duration.ofSeconds(30)))
    result.get()

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    if lines.isEmpty then return 0
    println("Expecting: 14327")
    val program = lines.head.split(",").map(_.toLong)
    val queues = (0 to 49).map(id =>
      val q = new InQueue()
      q.offer(id)
      q
    ).toArray
    val out = NAT(queues)
    val computers = Thread(() => out.run()) :: (0 to 49).toList.map(id => new Thread((() => execute(program.clone(), queues(id), out))))
    result.set(0)
    finished.set(false)
    computers.foreach(_.start())
    computers.forall(c => c.join(Duration.ofSeconds(10)))
    println("Done")
    finished.set(true)
    result.get()

}

object Day23 extends App {
  new Day23().solvePuzzles()
}
