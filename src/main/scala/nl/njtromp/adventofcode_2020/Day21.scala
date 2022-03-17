package nl.njtromp.adventofcode_2020

import nl.njtromp.adventofcode.Puzzle
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.TerminalNode

import scala.collection.JavaConverters

class Day21 extends Puzzle {

  def solvePart1(lines: List[String]): Long = {
    val foods: List[(Set[String], Set[String])] = lines.map(line => {
      val parser = new Day21Parser(new CommonTokenStream(new Day21Lexer(CharStreams.fromString(line.replaceAll(",", "")))))
      PuzzleVisitor21.visit(parser.food)
    })
    val ingredients: Set[String] = foods.flatten(_._1).toSet
    val allergens: Set[String] = foods.flatten(_._2).toSet

    val allergensWithIngredients = allergens.map(a => a -> foods.filter(_._2.contains(a)).map(_._1).reduce(_ & _)).toMap
    val nonAllergicIngredients = ingredients.diff(allergensWithIngredients.values.flatten.toSet).toList

    nonAllergicIngredients.map(i => foods.count(_._1.contains(i))).sum
  }

  def solvePart2(lines: List[String]): Long = {
    val foods: List[(Set[String], Set[String])] = lines.map(line => {
      val parser = new Day21Parser(new CommonTokenStream(new Day21Lexer(CharStreams.fromString(line.replaceAll(",", "")))))
      PuzzleVisitor21.visit(parser.food)
    })
    val allergens: Set[String] = foods.flatten(_._2).toSet

    var allergensWithIngredients: Map[String, Set[String]] = allergens.map(a => a -> foods.filter(_._2.contains(a)).map(_._1).reduce(_ & _)).toMap
    while (allergensWithIngredients.exists(_._2.size > 1)) {
      allergensWithIngredients.filter(_._2.size == 1).values.foreach(i => {
        allergensWithIngredients = allergensWithIngredients.map(ai => if (ai._2.size == 1) ai else (ai._1 -> ai._2.filter(!_.equals(i.head))))
      })
    }

    val dangerousIngredientsList = allergensWithIngredients.toList.sortBy(_._1).map(_._2.head).mkString(",")
    println(dangerousIngredientsList)
    3
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
  new Day21().solvePuzzles("/2020/input-puzzle21.txt")
}
