// Generated from java-escape by ANTLR 4.11.1
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class Day12_2018Parser extends Parser {
	static { RuntimeMetaData.checkVersion("4.11.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, WHITESPACE=5, EOL=6;
	public static final int
		RULE_initial_state = 0, RULE_pots = 1, RULE_pot = 2, RULE_transformations = 3, 
		RULE_transformation = 4, RULE_newPot = 5;
	private static String[] makeRuleNames() {
		return new String[] {
			"initial_state", "pots", "pot", "transformations", "transformation", 
			"newPot"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'initial state: '", "'.'", "'#'", "'=>'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, "WHITESPACE", "EOL"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "java-escape"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public Day12_2018Parser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Initial_stateContext extends ParserRuleContext {
		public PotsContext pots() {
			return getRuleContext(PotsContext.class,0);
		}
		public TerminalNode EOL() { return getToken(Day12_2018Parser.EOL, 0); }
		public Initial_stateContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_initial_state; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof Day12_2018Listener ) ((Day12_2018Listener)listener).enterInitial_state(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof Day12_2018Listener ) ((Day12_2018Listener)listener).exitInitial_state(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof Day12_2018Visitor ) return ((Day12_2018Visitor<? extends T>)visitor).visitInitial_state(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Initial_stateContext initial_state() throws RecognitionException {
		Initial_stateContext _localctx = new Initial_stateContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_initial_state);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(12);
			match(T__0);
			setState(13);
			pots();
			setState(14);
			match(EOL);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PotsContext extends ParserRuleContext {
		public List<PotContext> pot() {
			return getRuleContexts(PotContext.class);
		}
		public PotContext pot(int i) {
			return getRuleContext(PotContext.class,i);
		}
		public PotsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pots; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof Day12_2018Listener ) ((Day12_2018Listener)listener).enterPots(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof Day12_2018Listener ) ((Day12_2018Listener)listener).exitPots(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof Day12_2018Visitor ) return ((Day12_2018Visitor<? extends T>)visitor).visitPots(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PotsContext pots() throws RecognitionException {
		PotsContext _localctx = new PotsContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_pots);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(17); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(16);
				pot();
				}
				}
				setState(19); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==T__1 || _la==T__2 );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PotContext extends ParserRuleContext {
		public PotContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pot; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof Day12_2018Listener ) ((Day12_2018Listener)listener).enterPot(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof Day12_2018Listener ) ((Day12_2018Listener)listener).exitPot(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof Day12_2018Visitor ) return ((Day12_2018Visitor<? extends T>)visitor).visitPot(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PotContext pot() throws RecognitionException {
		PotContext _localctx = new PotContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_pot);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(21);
			_la = _input.LA(1);
			if ( !(_la==T__1 || _la==T__2) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TransformationsContext extends ParserRuleContext {
		public List<TransformationContext> transformation() {
			return getRuleContexts(TransformationContext.class);
		}
		public TransformationContext transformation(int i) {
			return getRuleContext(TransformationContext.class,i);
		}
		public TransformationsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_transformations; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof Day12_2018Listener ) ((Day12_2018Listener)listener).enterTransformations(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof Day12_2018Listener ) ((Day12_2018Listener)listener).exitTransformations(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof Day12_2018Visitor ) return ((Day12_2018Visitor<? extends T>)visitor).visitTransformations(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TransformationsContext transformations() throws RecognitionException {
		TransformationsContext _localctx = new TransformationsContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_transformations);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(24); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(23);
				transformation();
				}
				}
				setState(26); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==T__1 || _la==T__2 );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TransformationContext extends ParserRuleContext {
		public List<PotContext> pot() {
			return getRuleContexts(PotContext.class);
		}
		public PotContext pot(int i) {
			return getRuleContext(PotContext.class,i);
		}
		public NewPotContext newPot() {
			return getRuleContext(NewPotContext.class,0);
		}
		public TerminalNode EOL() { return getToken(Day12_2018Parser.EOL, 0); }
		public TransformationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_transformation; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof Day12_2018Listener ) ((Day12_2018Listener)listener).enterTransformation(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof Day12_2018Listener ) ((Day12_2018Listener)listener).exitTransformation(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof Day12_2018Visitor ) return ((Day12_2018Visitor<? extends T>)visitor).visitTransformation(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TransformationContext transformation() throws RecognitionException {
		TransformationContext _localctx = new TransformationContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_transformation);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(28);
			pot();
			setState(29);
			pot();
			setState(30);
			pot();
			setState(31);
			pot();
			setState(32);
			pot();
			setState(33);
			match(T__3);
			setState(34);
			newPot();
			setState(35);
			match(EOL);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class NewPotContext extends ParserRuleContext {
		public NewPotContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_newPot; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof Day12_2018Listener ) ((Day12_2018Listener)listener).enterNewPot(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof Day12_2018Listener ) ((Day12_2018Listener)listener).exitNewPot(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof Day12_2018Visitor ) return ((Day12_2018Visitor<? extends T>)visitor).visitNewPot(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NewPotContext newPot() throws RecognitionException {
		NewPotContext _localctx = new NewPotContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_newPot);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(37);
			_la = _input.LA(1);
			if ( !(_la==T__1 || _la==T__2) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\u0004\u0001\u0006(\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002"+
		"\u0005\u0007\u0005\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001"+
		"\u0001\u0004\u0001\u0012\b\u0001\u000b\u0001\f\u0001\u0013\u0001\u0002"+
		"\u0001\u0002\u0001\u0003\u0004\u0003\u0019\b\u0003\u000b\u0003\f\u0003"+
		"\u001a\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001"+
		"\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0000\u0000\u0006\u0000\u0002\u0004\u0006\b\n\u0000\u0001\u0001"+
		"\u0000\u0002\u0003#\u0000\f\u0001\u0000\u0000\u0000\u0002\u0011\u0001"+
		"\u0000\u0000\u0000\u0004\u0015\u0001\u0000\u0000\u0000\u0006\u0018\u0001"+
		"\u0000\u0000\u0000\b\u001c\u0001\u0000\u0000\u0000\n%\u0001\u0000\u0000"+
		"\u0000\f\r\u0005\u0001\u0000\u0000\r\u000e\u0003\u0002\u0001\u0000\u000e"+
		"\u000f\u0005\u0006\u0000\u0000\u000f\u0001\u0001\u0000\u0000\u0000\u0010"+
		"\u0012\u0003\u0004\u0002\u0000\u0011\u0010\u0001\u0000\u0000\u0000\u0012"+
		"\u0013\u0001\u0000\u0000\u0000\u0013\u0011\u0001\u0000\u0000\u0000\u0013"+
		"\u0014\u0001\u0000\u0000\u0000\u0014\u0003\u0001\u0000\u0000\u0000\u0015"+
		"\u0016\u0007\u0000\u0000\u0000\u0016\u0005\u0001\u0000\u0000\u0000\u0017"+
		"\u0019\u0003\b\u0004\u0000\u0018\u0017\u0001\u0000\u0000\u0000\u0019\u001a"+
		"\u0001\u0000\u0000\u0000\u001a\u0018\u0001\u0000\u0000\u0000\u001a\u001b"+
		"\u0001\u0000\u0000\u0000\u001b\u0007\u0001\u0000\u0000\u0000\u001c\u001d"+
		"\u0003\u0004\u0002\u0000\u001d\u001e\u0003\u0004\u0002\u0000\u001e\u001f"+
		"\u0003\u0004\u0002\u0000\u001f \u0003\u0004\u0002\u0000 !\u0003\u0004"+
		"\u0002\u0000!\"\u0005\u0004\u0000\u0000\"#\u0003\n\u0005\u0000#$\u0005"+
		"\u0006\u0000\u0000$\t\u0001\u0000\u0000\u0000%&\u0007\u0000\u0000\u0000"+
		"&\u000b\u0001\u0000\u0000\u0000\u0002\u0013\u001a";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}