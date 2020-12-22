package nl.njtromp.adventofcode_2020

import _root_.nl.njtromp.{Day21BaseVisitor, Day21Lexer, Day21Parser}
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.TerminalNode

import java.util
import scala.collection.JavaConverters

class Day21 extends Puzzle {

  class ErrorListener extends org.antlr.v4.runtime.ANTLRErrorListener {
    var errorCount = 0
    override def syntaxError(recognizer: Recognizer[_, _], o: Any, i: Int, i1: Int, s: String, e: RecognitionException): Unit = errorCount += 1
    override def reportAmbiguity(parser: Parser, dfa: DFA, i: Int, i1: Int, b: Boolean, bitSet: util.BitSet, atnConfigSet: ATNConfigSet): Unit =  errorCount += 1
    override def reportAttemptingFullContext(parser: Parser, dfa: DFA, i: Int, i1: Int, bitSet: util.BitSet, atnConfigSet: ATNConfigSet): Unit =  errorCount += 1
    override def reportContextSensitivity(parser: Parser, dfa: DFA, i: Int, i1: Int, i2: Int, atnConfigSet: ATNConfigSet): Unit =  errorCount += 1
  }

  def solvePart1(lines: List[String]): Long = {
    var errors = 0
    val foods: List[(Set[String], Set[String])] = lines.map(line => {
      val parser = new Day21Parser(new CommonTokenStream(new Day21Lexer(CharStreams.fromString(line.replaceAll(",", "")))))
      val errorListener = new ErrorListener
      parser.addErrorListener(errorListener)
      val food =  PuzzleVisitor21.visit(parser.food)
      errors += errorListener.errorCount
      food
    })
    val ingredients: Set[String] = foods.flatten(_._1).toSet
    val allergens: Set[String] = foods.flatten(_._2).toSet

    val allergensWithIngredients = allergens.map(a => a -> foods.filter(_._2.contains(a)).map(_._1).reduce(_ & _)).toMap
    val nonAllergicIngredients = ingredients.diff(allergensWithIngredients.values.flatten.toSet).toList

    nonAllergicIngredients.map(i => foods.count(_._1.contains(i))).sum
  }

  def solvePart2(lines: List[String]): Long = {
    -1
  }

  object PuzzleVisitor21 extends Day21BaseVisitor[(Set[String], Set[String])] {
    override def visitFood(ctx: Day21Parser.FoodContext): (Set[String], Set[String]) = {
      (IngredientsVisitor.visit(ctx.ingredients()), AllergensVisitor.visit(ctx.allergens()))
    }
  }

  object IngredientsVisitor extends Day21BaseVisitor[Set[String]] {
    override def visitIngredients(ctx: Day21Parser.IngredientsContext): Set[String] = {
      val ingredients: Set[TerminalNode] = JavaConverters.asScalaBuffer(ctx.WORD()).toSet
      ingredients.map(_.getText)
    }
  }

  object AllergensVisitor extends Day21BaseVisitor[Set[String]] {
    override def visitAllergens(ctx: Day21Parser.AllergensContext): Set[String] = {
      val allergens: Set[TerminalNode] = JavaConverters.asScalaBuffer(ctx.WORD()).toSet
      allergens.map(_.getText)
    }
  }

}

object Day21 extends App {
  new Day21().solvePuzzles("/input-puzzle21.txt")
}

