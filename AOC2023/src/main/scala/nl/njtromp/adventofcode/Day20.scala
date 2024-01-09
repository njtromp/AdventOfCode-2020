package nl.njtromp.adventofcode

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Day20 extends Puzzle[Long] {
  private type Pulse = Boolean
  private val low = false
  private var buttonPresses = 0L
  private var pulseQueue: PulseQueue = _
  private abstract class Module(val name: String, val connectionNames: List[String]) {
    val inputs: ListBuffer[Module] = ListBuffer.empty[Module]
    val outputs: ListBuffer[Module]  = ListBuffer.empty[Module]
    def connectOutput(output: Module): Unit =
      outputs += output
    def connectInput(input: Module): Unit =
      inputs += input
    def input(pulse: Boolean, source: Module): Unit
    override def toString: String = s"Module($name -> ${connectionNames.mkString("(", ",",")")})"
  }
  private case class Broadcaster(override val name: String, override val connectionNames: List[String]) extends Module(name, connectionNames) {
    override def input(pulse: Boolean, source: Module): Unit =
      outputs.foreach(m =>
        pulseQueue.enqueue(pulse, this, m)
      )
    override def toString: String = s"Broadcaster($name -> ${connectionNames.mkString("(", ",", ")")})"
  }
  private case class FlipFlop(override val name: String, override val connectionNames: List[String]) extends Module(name, connectionNames) {
    private var state = false
    override def input(pulse: Boolean, source: Module): Unit =
      if !pulse then
        state = !state
        outputs.foreach(m =>
          pulseQueue.enqueue(state, this, m)
        )
    override def toString: String = s"FlipFlop($name -> ${connectionNames.mkString("(", ",",")")})"
  }
  private case class Conjunction(override val name: String, override val connectionNames: List[String]) extends Module(name, connectionNames) {
    private val states = ListBuffer.empty[Boolean]
    override def connectInput(input: Module): Unit =
      super.connectInput(input)
      states += false
    override def input(pulse: Boolean, source: Module): Unit =
      val mi = inputs.indexOf(source)
      states(mi) = pulse
      val newPulse = !states.forall(s => s)
      outputs.foreach(m =>
        pulseQueue.enqueue(newPulse, this, m)
      )
    override def toString: String = s"Conjunction($name -> ${connectionNames.mkString("(", ",",")")})"
  }
  private class PulseQueue() {
    var lowPulses: Long = 0L
    var highPulses: Long = 0L
    // These inverters need to receive a low pulse.
    // They all get it after a different (prime) amount of button pulses.
    private val interestingModules = Set("tn", "dr", "bm", "cl")
    val buttonPressesToFirstLowPulse = mutable.Map.empty[String, Long]
    private val pulses = mutable.Queue.empty[(Pulse, Module, Module)]
    def nonEmpty: Boolean = pulses.nonEmpty
    def enqueue(pulse: Pulse, source: Module, receiver: Module): Unit =
      if pulse == low then
        lowPulses += 1L
        pulsesPerModule(receiver) += 1L
      else
        highPulses += 1L
      pulses += ((pulse, source, receiver))
      if pulse == low && interestingModules.contains(receiver.name) then
        buttonPressesToFirstLowPulse(receiver.name) = buttonPresses
    def dequeue(): (Pulse, Module, Module) = pulses.dequeue()
  }
  private val pulsesPerModule = mutable.Map.empty[Module, Long].withDefaultValue(0L)

  private def parseLine(line: String): Module =
    val connectionNames = line.split("->").last.split(',').map(_.trim).toList
    line.head match {
      case '%' => FlipFlop(line.substring(1).takeWhile(_ != ' '), connectionNames)
      case '&' => Conjunction(line.substring(1).takeWhile(_ != ' '), connectionNames)
      case _ => Broadcaster(line.takeWhile(_ != ' '), connectionNames)
    }

  private def connectModules(modules: List[Module]): Map[String, Module] =
    val moduleMap = modules.map(m => (m.name, m)).toMap
    modules.foreach(m =>
      m.connectionNames.foreach(n =>
        if moduleMap.keySet.contains(n) then
          m.connectOutput(moduleMap(n))
          moduleMap(n).connectInput(m)
      )
    )
    moduleMap

  private def runSimulation(runs: Int, modules: List[Module]): Long =
    val modulesByName = connectModules(modules)
    pulseQueue = new PulseQueue()
    val button = modulesByName("button")
    val broadcaster = modulesByName("broadcaster")
    (1 to runs).foreach(_ =>
      pulseQueue.enqueue(false, button, broadcaster)
      while pulseQueue.nonEmpty do
        val (pulse, source, receiver) = pulseQueue.dequeue()
        receiver.input(pulse, source)
    )
    pulseQueue.lowPulses * pulseQueue.highPulses

  private def runSimulation(modules: List[Module]): Long =
    val modulesByName = connectModules(modules)
    pulseQueue = new PulseQueue()
    val button = modulesByName("button")
    val broadcaster = modulesByName("broadcaster")
    // There are 4 inverters that need to get all a low pulse for RX gets a low pulse
    // The final solution is the product of the pulses for each individual inverter.
    while pulseQueue.buttonPressesToFirstLowPulse.keySet.size != 4 do
      buttonPresses += 1L
      pulseQueue.enqueue(false, button, broadcaster)
      while pulseQueue.nonEmpty do
        val (pulse, source, receiver) = pulseQueue.dequeue()
        receiver.input(pulse, source)
    pulseQueue.buttonPressesToFirstLowPulse.values.product

  override def exampleAnswerPart1: Long = 32000000 + 11687500
  override def solvePart1(lines: List[String]): Long =
    val moduleGroups = groupByEmptyLine(lines)
      .map(ls => ("button -> broadcaster" :: "output -> oblivion" :: "rx -> oblivion" :: ls).map(parseLine))
    moduleGroups.map(runSimulation(1000, _)).sum

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    if Day20.skipPart2 then
      Day20.skipPart2 = false
      pulsesPerModule.clear()
      0
    else
      val modules = groupByEmptyLine(lines)
        .map(ls => ("button -> broadcaster" :: "output -> oblivion" :: "rx -> oblivion" :: ls).map(parseLine))
        .head
      runSimulation(modules)

}

object Day20 extends App {
  private var skipPart2 = true
  new Day20().solvePuzzles()
}
