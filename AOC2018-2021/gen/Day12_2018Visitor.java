// Generated from java-escape by ANTLR 4.11.1
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link Day12_2018Parser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface Day12_2018Visitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link Day12_2018Parser#initial_state}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInitial_state(Day12_2018Parser.Initial_stateContext ctx);
	/**
	 * Visit a parse tree produced by {@link Day12_2018Parser#pots}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPots(Day12_2018Parser.PotsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Day12_2018Parser#pot}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPot(Day12_2018Parser.PotContext ctx);
	/**
	 * Visit a parse tree produced by {@link Day12_2018Parser#transformations}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTransformations(Day12_2018Parser.TransformationsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Day12_2018Parser#transformation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTransformation(Day12_2018Parser.TransformationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Day12_2018Parser#newPot}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNewPot(Day12_2018Parser.NewPotContext ctx);
}