package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

class Day24 extends Puzzle[String] {

  private val AND = "AND"
  private val XOR = "XOR"
  private val OR = "OR"

  private case class Wire(name: String) {
    private var value = Option.empty[Boolean]
    def isSet: Boolean = value.isDefined
    def get: Boolean = value.get
    def set(v: Boolean): Unit =
      value = Option(v)
  }
  private object Wire {
    private val byName = mutable.Map.empty[String, Wire]
    private val swap = mutable.Map.empty[String, String]
    def parse(line: String): Wire =
      line match
        case s"$name: $value" =>
          val wire = Wire(name)
          wire.set(value == "1")
          byName(wire.name) = wire
          wire
    def get(name: String): Wire =
      byName.getOrElseUpdate(name, Wire(name))
    def allSet: Boolean =
      byName.values.forall(_.isSet)
    def wires(p: Wire => Boolean): List[Wire] = byName.values.filter(p).toList
    def clear(): Unit = byName.clear()
  }

  private case class Port(op: String, in1: Wire, in2: Wire, out: Wire) {
    private def switch(): Boolean =
      if in1.isSet && in2.isSet && !out.isSet then
        val newValue = op match
          case "AND" => in1.get & in2.get
          case "OR" => in1.get | in2.get
          case "XOR" => in1.get ^ in2.get
        out.set(newValue)
        true
      else
        false
    def hasAsInput(wire: Wire): Boolean =
      in1 == wire || in2 == wire
    override def toString: String = s"${in1.name} $op ${in2.name} -> ${out.name}"
  }
  private object Port {
    private val ports = mutable.Buffer.empty[Port]
    def parse(line: String): Port =
      val port = line match
        case s"$in1 $op $in2 -> $out" =>
          if in1.compare(in2) < 0 then
            Port(op, Wire.get(in1), Wire.get(in2), Wire.get(out))
          else
            Port(op, Wire.get(in2), Wire.get(in1), Wire.get(out))
      ports += port
      port
    def ports(p: Port => Boolean): List[Port] =
      ports.filter(p).toList
    def switch(): Boolean =
      ports.foldLeft(false)((switched, port) => switched || port.switch())
    def clear(): Unit = ports.clear()
  }

  @tailrec
  private def decode(value: Long, wires: List[Wire]): Long =
    if wires.isEmpty then
      value
    else
      decode(value * 2 + (if wires.head.get then 1 else 0), wires.tail)

  private def decode(wires: List[Wire]): Long =
    decode(0, wires.sortBy(_.name).reverse)

  private def halfAdderCarry(in1: Wire, in2: Wire, sOut: Wire): Option[Wire] =
    val inPorts = Port.ports(p => p.hasAsInput(in1) && p.hasAsInput(in2))
    if inPorts.size == 2 && inPorts.count(_.op == XOR) == 1 && inPorts.count(_.op == AND) == 1 then
      val sum = inPorts.filter(_.op == XOR).head
      if sum.out == sOut then
        val carry = inPorts.filter(_.op == AND).head
        Some(carry.out)
      else
        None
    else
      None

  private def fullAdderCarry(in1: Wire, in2: Wire, cIn: Wire, sOut: Wire): Option[Wire] =
    def optional(ports: List[Port]): Option[Port] =
      ports match
        case p :: Nil => Some(p)
        case _ =>
          if ports.isEmpty then
            println("No matching ports found!")
          else
            println("To many ports found!")
          None
    optional(Port.ports(p => p.hasAsInput(in1) && p.hasAsInput(in2)).filter(_.op == XOR)) match
      case Some(xorIn) =>
        optional(Port.ports(p => p.hasAsInput(cIn) && p.hasAsInput(xorIn.out)).filter(_.op == XOR)) match
          case Some(xorOut) =>
            if xorOut.out == sOut then
              optional(Port.ports(p => p.hasAsInput(in1) && p.hasAsInput(in2)).filter(_.op == AND)) match
                case Some(andIn) =>
                  optional(Port.ports(p => p.hasAsInput(cIn) && p.hasAsInput(xorIn.out)).filter(_.op == AND)) match
                    case Some(carrySumInternal) =>
                      optional(Port.ports(p => p.hasAsInput(carrySumInternal.out) && p.hasAsInput(andIn.out)).filter(_.op == OR)) match
                        case Some(carryOut) =>
                          Some(carryOut.out)
                        case None =>
                          println(s"Missing OR for ${carrySumInternal.out} and ${andIn.out}")
                          None
                    case None =>
                      println(s"Missing AND for internal sum ${xorIn.out} and carry $cIn")
                      None
                case None =>
                  println(s"Missing AND for $in1 and $in2")
                  None
            else
              println(s"Expecting $sOut as output but got ${xorOut.out}")
              None
          case None =>
            optional(Port.ports(p => p.out == sOut && p.hasAsInput(cIn)).filter(_.op == XOR)) match
              case Some(xorOut) =>
                println(s"Missing XOR out for $sOut, but found $xorOut that has the correct carry-in (${cIn.name}), that should use the result of $xorIn")
              case None => println(s"Missing XOR out for $sOut")
            None
      case None =>
        println(s"sMissing XOR port for $in1 and $in2")
        None

  private def checkAdders(): Boolean =
    val xWires = Wire.wires(_.name.startsWith("x")).sortBy(_.name)
    val yWires = Wire.wires(_.name.startsWith("y")).sortBy(_.name)
    val zWires = Wire.wires(_.name.startsWith("z")).sortBy(_.name)
    (1 until xWires.size).foldLeft(halfAdderCarry(xWires.head, yWires.head, zWires.head))((c, n) =>
      c match
        case Some(carry) => fullAdderCarry(xWires(n), yWires(n), carry, zWires(n))
        case None => None
    ) match
      case Some(cOut) => cOut == zWires(45)
      case None => false

  override def exampleAnswerPart1: String = "2024"
  override def solvePart1(lines: List[String]): String =
    Wire.clear()
    Port.clear()
    val config = groupByEmptyLine(lines)
    config.head.foreach(Wire.parse)
    val ports = config.last.map(Port.parse)
    while !Wire.allSet do
      Port.switch()
    decode(Wire.wires(_.name.startsWith("z"))).toString

  override def exampleAnswerPart2: String = "0"
  override def solvePart2(lines: List[String]): String =
    if lines.size <= 50 then
      "0"
    else
      Wire.clear()
      Port.clear()
      val config = groupByEmptyLine(lines)
      config.head.foreach(Wire.parse)

      val mapping = List(
        ("z13", "vcv"),
        ("z19", "vwp"),
        ("z25", "mps"),
        ("cqm", "vjv")
      )
      def swapOutputs(line: String): String =
        mapping.foldLeft(line)((l, m) =>
          if l.endsWith(m._1) then
            l.replace(m._1, m._2)
          else if l.endsWith(m._2) then
            l.replace(m._2, m._1)
          else
            l
        )
      config.last.map(swapOutputs).foreach(Port.parse)
      if checkAdders() then
        mapping.flatMap(m => List(m._1, m._2)).sorted.mkString(",")
      else
        "Needs fixing"

}

object Day24 extends App {
  new Day24().solvePuzzles()
}
