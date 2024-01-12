package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day19 extends ParserPuzzle[Long] {
  private abstract class Rule(name: String) {
    def nextRule: String = name
    def matches(part: Part): Boolean = true
  }
  private case class ElseRule(ruleName: String) extends Rule(ruleName)
  private case class LimitRule(partId: Char, op: Char, limit: Int, ruleName: String) extends Rule(ruleName) {
    override def matches(part: Part): Boolean =
      val partCount = part.parts.filter(_.id == partId).head.score
      if op == '<' then partCount < limit else partCount > limit
  }
  private case class Workflow(name: String, rules: List[Rule])
  private case class Rating(id: Char, score: Int)
  private case class Part(parts: List[Rating]) {
    def score(): Long = parts.map(_.score).sum
  }
  private def elseRule: Parser[Rule] = word ^^ {ruleName => ElseRule(ruleName)}
  private def limitRule: Parser[Rule] = ("\\w".r ~ "[<>]".r ~ integer) ~ ":" ~ word ^^
    { case partId ~ op ~ limit ~ ":" ~ ruleName => LimitRule(partId.head, op.head, limit.toInt, ruleName) }
  private def workflow: Parser[Workflow] = word ~ "{" ~ repsep(limitRule, ",") ~ "," ~ elseRule ~ "}" ^^
    { case name ~ "{" ~ rules ~ "," ~ elseRule ~ "}" => Workflow(name, rules ++ List(elseRule)) }
  private def rating: Parser[Rating] = "\\w".r ~ "=" ~ integer ^^ { case partId ~ "=" ~ score => Rating(partId.head, score.toInt) }
  private def part: Parser[Part] = "{" ~> repsep(rating, ",") <~ "}" ^^ { ratings => Part(ratings) }

  private def process(part: Part, workflows: Map[String, Workflow]): Long =
    @tailrec
    def process(name: String): Long =
      name match {
        case "R" => 0
        case "A" => part.score()
        case _ =>
          val workflow = workflows(name)
          process(workflow.rules.filter(_.matches(part)).head.nextRule)
      }
    process("in")

  private def processAll(workflows: Map[String, Workflow]): Long =
    def processRules(rules: List[Rule], xs: Range, ms: Range, as: Range, ss: Range): Long =
      def split(partId: Char, op: Char, limit: Int, xs: Range, ms: Range, as: Range, ss: Range): (Range, Range, Range, Range, Range, Range, Range, Range) =
        // The first range matches the rule and the second range does not match the rule
        def split(r: Range): (Range, Range) = op match {
          // Using until with the - 1 ensures the range stays inclusive, and that everything performs as expected!
          case '<' => (r.start to limit - 1, limit to r.end)
          case '>' => (limit + 1 to r.end, r.start to limit)
        }
        partId match {
          case 'x' => (split(xs)._1, ms, as, ss, split(xs)._2, ms, as, ss)
          case 'm' => (xs, split(ms)._1, as, ss, xs, split(ms)._2, as, ss)
          case 'a' => (xs, ms, split(as)._1, ss, xs, ms, split(as)._2, ss)
          case 's' => (xs, ms, as, split(ss)._1, xs, ms, as, split(ss)._2)
        }
      rules.head match {
        case ElseRule(ruleName) =>
          process(ruleName, xs, ms, as, ss)
        case LimitRule(partId, op, limit, ruleName) =>
          // Ranges starting with 'm' are for matching the rule,
          // the ranges starting with 'n' are not matching the rule
          val (mxs, mms, mas, mss, nxs, nms, nas, nss) = split(partId, op, limit, xs, ms, as, ss)
          process(ruleName, mxs, mms, mas, mss) + processRules(rules.tail, nxs, nms, nas, nss)
      }
    def process(name: String, xs: Range, ms: Range, as: Range, ss: Range): Long =
      name match {
        case "R" =>
          0L
        case "A" =>
          xs.size.toLong * ms.size.toLong * as.size.toLong * ss.size.toLong
        case _ =>
          processRules(workflows(name).rules, xs, ms, as, ss)
      }
    process("in", 1 to 4000, 1 to 4000, 1 to 4000, 1 to 4000)

  override def exampleAnswerPart1: Long = 19114
  override def solvePart1(lines: List[String]): Long =
    val workflows = groupByEmptyLine(lines).head.map(parse(workflow, _).get).map(w => w.name -> w).toMap
    val parts = groupByEmptyLine(lines).last.map(parse(part, _).get)
    parts.map(process(_, workflows)).sum

  override def exampleAnswerPart2: Long = 167409079868000L
  override def solvePart2(lines: List[String]): Long =
    val workflows = groupByEmptyLine(lines).head.map(parse(workflow, _).get).map(w => w.name -> w).toMap
    processAll(workflows)

}

object Day19 extends App {
  new Day19().solvePuzzles()
}
