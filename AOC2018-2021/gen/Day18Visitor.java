// Generated from /Users/nico/Projects/Prive/AdventOfCode/src/main/antlr4/nl/njtromp/Day18_2020.g4 by ANTLR 4.9.2
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link Day18Parser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface Day18Visitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link Day18Parser#prog}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProg(Day18Parser.ProgContext ctx);
	/**
	 * Visit a parse tree produced by {@link Day18Parser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpr(Day18Parser.ExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link Day18Parser#prog2}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProg2(Day18Parser.Prog2Context ctx);
	/**
	 * Visit a parse tree produced by {@link Day18Parser#expr2}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpr2(Day18Parser.Expr2Context ctx);
}
