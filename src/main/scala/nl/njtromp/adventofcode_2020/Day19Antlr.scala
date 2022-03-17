package nl.njtromp.adventofcode_2020

import nl.njtromp.adventofcode.Puzzle
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, Parser, RecognitionException, Recognizer}

import java.util

class Day19Antlr extends Puzzle {

  class ErrorListener extends org.antlr.v4.runtime.ANTLRErrorListener {
    var errorCount = 0
    override def syntaxError(recognizer: Recognizer[_, _], o: Any, i: Int, i1: Int, s: String, e: RecognitionException): Unit = errorCount += 1
    override def reportAmbiguity(parser: Parser, dfa: DFA, i: Int, i1: Int, b: Boolean, bitSet: util.BitSet, atnConfigSet: ATNConfigSet): Unit =  errorCount += 1
    override def reportAttemptingFullContext(parser: Parser, dfa: DFA, i: Int, i1: Int, bitSet: util.BitSet, atnConfigSet: ATNConfigSet): Unit =  errorCount += 1
    override def reportContextSensitivity(parser: Parser, dfa: DFA, i: Int, i1: Int, i2: Int, atnConfigSet: ATNConfigSet): Unit =  errorCount += 1
  }

  def solvePart1(lines: List[String]): Long = {
    -1
  }

  def solvePart2(lines: List[String]): Long = {
    var matching = 0
    for (line <- lines) {
      val parser = new Day19Part2Parser(new CommonTokenStream(new Day19Part2Lexer(CharStreams.fromString(line))))
      val errorListener = new ErrorListener
      parser.addErrorListener(errorListener)
      PuzzleVisitor19.visit(parser.r0)
      if (errorListener.errorCount == 0) {
        matching += 1
      }
    }
    matching
  }

  object PuzzleVisitor19 extends Day19Part2BaseVisitor[Any] {}

}

object Day19Antlr extends App {
  new Day19Antlr().solvePuzzles("/2020/input-puzzle19-antlr.txt");
}
