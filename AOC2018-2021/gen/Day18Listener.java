// Generated from /Users/nico/Projects/Prive/AdventOfCode/src/main/antlr4/nl/njtromp/Day18_2020.g4 by ANTLR 4.9.2
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link Day18Parser}.
 */
public interface Day18Listener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link Day18Parser#prog}.
	 * @param ctx the parse tree
	 */
	void enterProg(Day18Parser.ProgContext ctx);
	/**
	 * Exit a parse tree produced by {@link Day18Parser#prog}.
	 * @param ctx the parse tree
	 */
	void exitProg(Day18Parser.ProgContext ctx);
	/**
	 * Enter a parse tree produced by {@link Day18Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterExpr(Day18Parser.ExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link Day18Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitExpr(Day18Parser.ExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link Day18Parser#prog2}.
	 * @param ctx the parse tree
	 */
	void enterProg2(Day18Parser.Prog2Context ctx);
	/**
	 * Exit a parse tree produced by {@link Day18Parser#prog2}.
	 * @param ctx the parse tree
	 */
	void exitProg2(Day18Parser.Prog2Context ctx);
	/**
	 * Enter a parse tree produced by {@link Day18Parser#expr2}.
	 * @param ctx the parse tree
	 */
	void enterExpr2(Day18Parser.Expr2Context ctx);
	/**
	 * Exit a parse tree produced by {@link Day18Parser#expr2}.
	 * @param ctx the parse tree
	 */
	void exitExpr2(Day18Parser.Expr2Context ctx);
}
