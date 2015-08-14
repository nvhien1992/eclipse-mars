// Generated from BKOOL.g4 by ANTLR 4.5

	package bkool.parser;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class BKOOLLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.5", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		CONST_DECL=1, STRING_LITERAL=2, UNCLOSE_STRING=3, LINE_CMT=4, BLOCK_CMT=5, 
		WS=6, BOOL=7, BREAK=8, CLASS=9, CONTINUE=10, DO=11, ELSE=12, EXTENDS=13, 
		FALSE=14, FINAL=15, FLOAT=16, IF=17, INTEGER=18, NEW=19, NULL=20, RETURN=21, 
		SELF=22, STATIC=23, STRING=24, THEN=25, TRUE=26, VOID=27, WHILE=28, IO_RD_INT=29, 
		IO_WR_INT=30, IO_WR_INT_LN=31, IO_RD_FLOAT=32, IO_WR_FLOAT=33, IO_WR_FLOAT_LN=34, 
		IO_RD_BOOL=35, IO_WR_BOOL=36, IO_WR_BOOL_LN=37, IO_RD_STR=38, IO_WR_STR=39, 
		IO_WR_STR_LN=40, IO=41, ADD=42, SUB=43, MUL=44, FLOAT_DIV=45, INT_DIV=46, 
		MOD=47, DIFF=48, EQUAL=49, LESS=50, GREATER=51, LESS_OR_EQUAL=52, GREATER_OR_EQUAL=53, 
		LOGICAL_OR=54, LOGICAL_AND=55, LOGICAL_NOT=56, CONCAT=57, INT_LITERAL=58, 
		FLOAT_LITERAL=59, ID=60, LSB=61, RSB=62, LP=63, RP=64, LB=65, RB=66, SEMICOLON=67, 
		COLON=68, DOT=69, COMMA=70, ASSIGN=71;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"CONST_DECL", "STRING_LITERAL", "UNCLOSE_STRING", "LINE_CMT", "BLOCK_CMT", 
		"WS", "BOOL", "BREAK", "CLASS", "CONTINUE", "DO", "ELSE", "EXTENDS", "FALSE", 
		"FINAL", "FLOAT", "IF", "INTEGER", "NEW", "NULL", "RETURN", "SELF", "STATIC", 
		"STRING", "THEN", "TRUE", "VOID", "WHILE", "IO_RD_INT", "IO_WR_INT", "IO_WR_INT_LN", 
		"IO_RD_FLOAT", "IO_WR_FLOAT", "IO_WR_FLOAT_LN", "IO_RD_BOOL", "IO_WR_BOOL", 
		"IO_WR_BOOL_LN", "IO_RD_STR", "IO_WR_STR", "IO_WR_STR_LN", "IO", "ADD", 
		"SUB", "MUL", "FLOAT_DIV", "INT_DIV", "MOD", "DIFF", "EQUAL", "LESS", 
		"GREATER", "LESS_OR_EQUAL", "GREATER_OR_EQUAL", "LOGICAL_OR", "LOGICAL_AND", 
		"LOGICAL_NOT", "CONCAT", "INT_LITERAL", "FLOAT_LITERAL", "ID", "LSB", 
		"RSB", "LP", "RP", "LB", "RB", "SEMICOLON", "COLON", "DOT", "COMMA", "ASSIGN"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'='", null, "'\"'", null, null, null, "'bool'", "'break'", "'class'", 
		"'continue'", "'do'", "'else'", "'extends'", "'false'", "'final'", "'float'", 
		"'if'", "'integer'", "'new'", "'null'", "'return'", "'self'", "'static'", 
		"'string'", "'then'", "'true'", "'void'", "'while'", "'io.readInt'", "'io.writeInt'", 
		"'io.writeIntLn'", "'io.readFloat'", "'io.writeFloat'", "'io.writeFloatLn'", 
		"'io.readBool'", "'io.writeBool'", "'io.writeBoolLn'", "'io.readStr'", 
		"'io.writeStr'", "'io.writeStrLn'", "'io'", "'+'", "'-'", "'*'", "'/'", 
		"'\\'", "'%'", "'<>'", "'=='", "'<'", "'>'", "'<='", "'>='", "'||'", "'&&'", 
		"'!'", "'^'", null, null, null, "'['", "']'", "'{'", "'}'", "'('", "')'", 
		"';'", "':'", "'.'", "','", "':='"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "CONST_DECL", "STRING_LITERAL", "UNCLOSE_STRING", "LINE_CMT", "BLOCK_CMT", 
		"WS", "BOOL", "BREAK", "CLASS", "CONTINUE", "DO", "ELSE", "EXTENDS", "FALSE", 
		"FINAL", "FLOAT", "IF", "INTEGER", "NEW", "NULL", "RETURN", "SELF", "STATIC", 
		"STRING", "THEN", "TRUE", "VOID", "WHILE", "IO_RD_INT", "IO_WR_INT", "IO_WR_INT_LN", 
		"IO_RD_FLOAT", "IO_WR_FLOAT", "IO_WR_FLOAT_LN", "IO_RD_BOOL", "IO_WR_BOOL", 
		"IO_WR_BOOL_LN", "IO_RD_STR", "IO_WR_STR", "IO_WR_STR_LN", "IO", "ADD", 
		"SUB", "MUL", "FLOAT_DIV", "INT_DIV", "MOD", "DIFF", "EQUAL", "LESS", 
		"GREATER", "LESS_OR_EQUAL", "GREATER_OR_EQUAL", "LOGICAL_OR", "LOGICAL_AND", 
		"LOGICAL_NOT", "CONCAT", "INT_LITERAL", "FLOAT_LITERAL", "ID", "LSB", 
		"RSB", "LP", "RP", "LB", "RB", "SEMICOLON", "COLON", "DOT", "COMMA", "ASSIGN"
	};
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


	public BKOOLLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "BKOOL.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	@Override
	public void action(RuleContext _localctx, int ruleIndex, int actionIndex) {
		switch (ruleIndex) {
		case 2:
			UNCLOSE_STRING_action((RuleContext)_localctx, actionIndex);
			break;
		}
	}
	private void UNCLOSE_STRING_action(RuleContext _localctx, int actionIndex) {
		switch (actionIndex) {
		case 0:
			System.out.print("There is an unclosed string.\n");
			break;
		}
	}

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\2I\u024e\b\1\4\2\t"+
		"\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t+\4"+
		",\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t\61\4\62\t\62\4\63\t\63\4\64\t"+
		"\64\4\65\t\65\4\66\t\66\4\67\t\67\48\t8\49\t9\4:\t:\4;\t;\4<\t<\4=\t="+
		"\4>\t>\4?\t?\4@\t@\4A\tA\4B\tB\4C\tC\4D\tD\4E\tE\4F\tF\4G\tG\4H\tH\3\2"+
		"\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\7\3\u00a3"+
		"\n\3\f\3\16\3\u00a6\13\3\3\3\3\3\3\4\3\4\3\4\3\5\3\5\7\5\u00af\n\5\f\5"+
		"\16\5\u00b2\13\5\3\5\5\5\u00b5\n\5\3\5\3\5\3\6\3\6\3\6\3\6\7\6\u00bd\n"+
		"\6\f\6\16\6\u00c0\13\6\3\6\3\6\3\6\3\6\3\6\3\7\6\7\u00c8\n\7\r\7\16\7"+
		"\u00c9\3\7\3\7\3\b\3\b\3\b\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\t\3\n\3\n\3\n"+
		"\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\f\3\f\3\f"+
		"\3\r\3\r\3\r\3\r\3\r\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\17\3\17"+
		"\3\17\3\17\3\17\3\17\3\20\3\20\3\20\3\20\3\20\3\20\3\21\3\21\3\21\3\21"+
		"\3\21\3\21\3\22\3\22\3\22\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\24"+
		"\3\24\3\24\3\24\3\25\3\25\3\25\3\25\3\25\3\26\3\26\3\26\3\26\3\26\3\26"+
		"\3\26\3\27\3\27\3\27\3\27\3\27\3\30\3\30\3\30\3\30\3\30\3\30\3\30\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\32\3\32\3\32\3\32\3\32\3\33\3\33\3\33"+
		"\3\33\3\33\3\34\3\34\3\34\3\34\3\34\3\35\3\35\3\35\3\35\3\35\3\35\3\36"+
		"\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\37\3\37\3\37\3\37"+
		"\3\37\3\37\3\37\3\37\3\37\3\37\3\37\3\37\3 \3 \3 \3 \3 \3 \3 \3 \3 \3"+
		" \3 \3 \3 \3 \3!\3!\3!\3!\3!\3!\3!\3!\3!\3!\3!\3!\3!\3\"\3\"\3\"\3\"\3"+
		"\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3#\3#\3#\3#\3#\3#\3#\3#\3#\3#\3"+
		"#\3#\3#\3#\3#\3#\3$\3$\3$\3$\3$\3$\3$\3$\3$\3$\3$\3$\3%\3%\3%\3%\3%\3"+
		"%\3%\3%\3%\3%\3%\3%\3%\3&\3&\3&\3&\3&\3&\3&\3&\3&\3&\3&\3&\3&\3&\3&\3"+
		"\'\3\'\3\'\3\'\3\'\3\'\3\'\3\'\3\'\3\'\3\'\3(\3(\3(\3(\3(\3(\3(\3(\3("+
		"\3(\3(\3(\3)\3)\3)\3)\3)\3)\3)\3)\3)\3)\3)\3)\3)\3)\3*\3*\3*\3+\3+\3,"+
		"\3,\3-\3-\3.\3.\3/\3/\3\60\3\60\3\61\3\61\3\61\3\62\3\62\3\62\3\63\3\63"+
		"\3\64\3\64\3\65\3\65\3\65\3\66\3\66\3\66\3\67\3\67\3\67\38\38\38\39\3"+
		"9\3:\3:\3;\6;\u0214\n;\r;\16;\u0215\3<\6<\u0219\n<\r<\16<\u021a\3<\3<"+
		"\7<\u021f\n<\f<\16<\u0222\13<\5<\u0224\n<\3<\3<\5<\u0228\n<\3<\6<\u022b"+
		"\n<\r<\16<\u022c\5<\u022f\n<\3=\3=\7=\u0233\n=\f=\16=\u0236\13=\3>\3>"+
		"\3?\3?\3@\3@\3A\3A\3B\3B\3C\3C\3D\3D\3E\3E\3F\3F\3G\3G\3H\3H\3H\5\u00a4"+
		"\u00b0\u00be\2I\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31"+
		"\16\33\17\35\20\37\21!\22#\23%\24\'\25)\26+\27-\30/\31\61\32\63\33\65"+
		"\34\67\359\36;\37= ?!A\"C#E$G%I&K\'M(O)Q*S+U,W-Y.[/]\60_\61a\62c\63e\64"+
		"g\65i\66k\67m8o9q:s;u<w=y>{?}@\177A\u0081B\u0083C\u0085D\u0087E\u0089"+
		"F\u008bG\u008dH\u008fI\3\2\t\3\3\f\f\5\2\13\f\16\17\"\"\3\2\62;\5\2GG"+
		"gg~~\5\2--//~~\5\2C\\aac|\6\2\62;C\\aac|\u0260\2\3\3\2\2\2\2\5\3\2\2\2"+
		"\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3"+
		"\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2"+
		"\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\2\'\3\2"+
		"\2\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2\2\2\61\3\2\2\2\2\63\3\2"+
		"\2\2\2\65\3\2\2\2\2\67\3\2\2\2\29\3\2\2\2\2;\3\2\2\2\2=\3\2\2\2\2?\3\2"+
		"\2\2\2A\3\2\2\2\2C\3\2\2\2\2E\3\2\2\2\2G\3\2\2\2\2I\3\2\2\2\2K\3\2\2\2"+
		"\2M\3\2\2\2\2O\3\2\2\2\2Q\3\2\2\2\2S\3\2\2\2\2U\3\2\2\2\2W\3\2\2\2\2Y"+
		"\3\2\2\2\2[\3\2\2\2\2]\3\2\2\2\2_\3\2\2\2\2a\3\2\2\2\2c\3\2\2\2\2e\3\2"+
		"\2\2\2g\3\2\2\2\2i\3\2\2\2\2k\3\2\2\2\2m\3\2\2\2\2o\3\2\2\2\2q\3\2\2\2"+
		"\2s\3\2\2\2\2u\3\2\2\2\2w\3\2\2\2\2y\3\2\2\2\2{\3\2\2\2\2}\3\2\2\2\2\177"+
		"\3\2\2\2\2\u0081\3\2\2\2\2\u0083\3\2\2\2\2\u0085\3\2\2\2\2\u0087\3\2\2"+
		"\2\2\u0089\3\2\2\2\2\u008b\3\2\2\2\2\u008d\3\2\2\2\2\u008f\3\2\2\2\3\u0091"+
		"\3\2\2\2\5\u0093\3\2\2\2\7\u00a9\3\2\2\2\t\u00ac\3\2\2\2\13\u00b8\3\2"+
		"\2\2\r\u00c7\3\2\2\2\17\u00cd\3\2\2\2\21\u00d2\3\2\2\2\23\u00d8\3\2\2"+
		"\2\25\u00de\3\2\2\2\27\u00e7\3\2\2\2\31\u00ea\3\2\2\2\33\u00ef\3\2\2\2"+
		"\35\u00f7\3\2\2\2\37\u00fd\3\2\2\2!\u0103\3\2\2\2#\u0109\3\2\2\2%\u010c"+
		"\3\2\2\2\'\u0114\3\2\2\2)\u0118\3\2\2\2+\u011d\3\2\2\2-\u0124\3\2\2\2"+
		"/\u0129\3\2\2\2\61\u0130\3\2\2\2\63\u0137\3\2\2\2\65\u013c\3\2\2\2\67"+
		"\u0141\3\2\2\29\u0146\3\2\2\2;\u014c\3\2\2\2=\u0157\3\2\2\2?\u0163\3\2"+
		"\2\2A\u0171\3\2\2\2C\u017e\3\2\2\2E\u018c\3\2\2\2G\u019c\3\2\2\2I\u01a8"+
		"\3\2\2\2K\u01b5\3\2\2\2M\u01c4\3\2\2\2O\u01cf\3\2\2\2Q\u01db\3\2\2\2S"+
		"\u01e9\3\2\2\2U\u01ec\3\2\2\2W\u01ee\3\2\2\2Y\u01f0\3\2\2\2[\u01f2\3\2"+
		"\2\2]\u01f4\3\2\2\2_\u01f6\3\2\2\2a\u01f8\3\2\2\2c\u01fb\3\2\2\2e\u01fe"+
		"\3\2\2\2g\u0200\3\2\2\2i\u0202\3\2\2\2k\u0205\3\2\2\2m\u0208\3\2\2\2o"+
		"\u020b\3\2\2\2q\u020e\3\2\2\2s\u0210\3\2\2\2u\u0213\3\2\2\2w\u0218\3\2"+
		"\2\2y\u0230\3\2\2\2{\u0237\3\2\2\2}\u0239\3\2\2\2\177\u023b\3\2\2\2\u0081"+
		"\u023d\3\2\2\2\u0083\u023f\3\2\2\2\u0085\u0241\3\2\2\2\u0087\u0243\3\2"+
		"\2\2\u0089\u0245\3\2\2\2\u008b\u0247\3\2\2\2\u008d\u0249\3\2\2\2\u008f"+
		"\u024b\3\2\2\2\u0091\u0092\7?\2\2\u0092\4\3\2\2\2\u0093\u00a4\7$\2\2\u0094"+
		"\u0095\7^\2\2\u0095\u00a3\7d\2\2\u0096\u0097\7^\2\2\u0097\u00a3\7v\2\2"+
		"\u0098\u0099\7^\2\2\u0099\u00a3\7t\2\2\u009a\u00a3\7\f\2\2\u009b\u009c"+
		"\7^\2\2\u009c\u00a3\7h\2\2\u009d\u009e\7^\2\2\u009e\u00a3\7^\2\2\u009f"+
		"\u00a0\7^\2\2\u00a0\u00a3\7$\2\2\u00a1\u00a3\13\2\2\2\u00a2\u0094\3\2"+
		"\2\2\u00a2\u0096\3\2\2\2\u00a2\u0098\3\2\2\2\u00a2\u009a\3\2\2\2\u00a2"+
		"\u009b\3\2\2\2\u00a2\u009d\3\2\2\2\u00a2\u009f\3\2\2\2\u00a2\u00a1\3\2"+
		"\2\2\u00a3\u00a6\3\2\2\2\u00a4\u00a5\3\2\2\2\u00a4\u00a2\3\2\2\2\u00a5"+
		"\u00a7\3\2\2\2\u00a6\u00a4\3\2\2\2\u00a7\u00a8\7$\2\2\u00a8\6\3\2\2\2"+
		"\u00a9\u00aa\7$\2\2\u00aa\u00ab\b\4\2\2\u00ab\b\3\2\2\2\u00ac\u00b0\7"+
		"%\2\2\u00ad\u00af\13\2\2\2\u00ae\u00ad\3\2\2\2\u00af\u00b2\3\2\2\2\u00b0"+
		"\u00b1\3\2\2\2\u00b0\u00ae\3\2\2\2\u00b1\u00b4\3\2\2\2\u00b2\u00b0\3\2"+
		"\2\2\u00b3\u00b5\t\2\2\2\u00b4\u00b3\3\2\2\2\u00b5\u00b6\3\2\2\2\u00b6"+
		"\u00b7\b\5\3\2\u00b7\n\3\2\2\2\u00b8\u00b9\7*\2\2\u00b9\u00ba\7,\2\2\u00ba"+
		"\u00be\3\2\2\2\u00bb\u00bd\13\2\2\2\u00bc\u00bb\3\2\2\2\u00bd\u00c0\3"+
		"\2\2\2\u00be\u00bf\3\2\2\2\u00be\u00bc\3\2\2\2\u00bf\u00c1\3\2\2\2\u00c0"+
		"\u00be\3\2\2\2\u00c1\u00c2\7,\2\2\u00c2\u00c3\7+\2\2\u00c3\u00c4\3\2\2"+
		"\2\u00c4\u00c5\b\6\3\2\u00c5\f\3\2\2\2\u00c6\u00c8\t\3\2\2\u00c7\u00c6"+
		"\3\2\2\2\u00c8\u00c9\3\2\2\2\u00c9\u00c7\3\2\2\2\u00c9\u00ca\3\2\2\2\u00ca"+
		"\u00cb\3\2\2\2\u00cb\u00cc\b\7\3\2\u00cc\16\3\2\2\2\u00cd\u00ce\7d\2\2"+
		"\u00ce\u00cf\7q\2\2\u00cf\u00d0\7q\2\2\u00d0\u00d1\7n\2\2\u00d1\20\3\2"+
		"\2\2\u00d2\u00d3\7d\2\2\u00d3\u00d4\7t\2\2\u00d4\u00d5\7g\2\2\u00d5\u00d6"+
		"\7c\2\2\u00d6\u00d7\7m\2\2\u00d7\22\3\2\2\2\u00d8\u00d9\7e\2\2\u00d9\u00da"+
		"\7n\2\2\u00da\u00db\7c\2\2\u00db\u00dc\7u\2\2\u00dc\u00dd\7u\2\2\u00dd"+
		"\24\3\2\2\2\u00de\u00df\7e\2\2\u00df\u00e0\7q\2\2\u00e0\u00e1\7p\2\2\u00e1"+
		"\u00e2\7v\2\2\u00e2\u00e3\7k\2\2\u00e3\u00e4\7p\2\2\u00e4\u00e5\7w\2\2"+
		"\u00e5\u00e6\7g\2\2\u00e6\26\3\2\2\2\u00e7\u00e8\7f\2\2\u00e8\u00e9\7"+
		"q\2\2\u00e9\30\3\2\2\2\u00ea\u00eb\7g\2\2\u00eb\u00ec\7n\2\2\u00ec\u00ed"+
		"\7u\2\2\u00ed\u00ee\7g\2\2\u00ee\32\3\2\2\2\u00ef\u00f0\7g\2\2\u00f0\u00f1"+
		"\7z\2\2\u00f1\u00f2\7v\2\2\u00f2\u00f3\7g\2\2\u00f3\u00f4\7p\2\2\u00f4"+
		"\u00f5\7f\2\2\u00f5\u00f6\7u\2\2\u00f6\34\3\2\2\2\u00f7\u00f8\7h\2\2\u00f8"+
		"\u00f9\7c\2\2\u00f9\u00fa\7n\2\2\u00fa\u00fb\7u\2\2\u00fb\u00fc\7g\2\2"+
		"\u00fc\36\3\2\2\2\u00fd\u00fe\7h\2\2\u00fe\u00ff\7k\2\2\u00ff\u0100\7"+
		"p\2\2\u0100\u0101\7c\2\2\u0101\u0102\7n\2\2\u0102 \3\2\2\2\u0103\u0104"+
		"\7h\2\2\u0104\u0105\7n\2\2\u0105\u0106\7q\2\2\u0106\u0107\7c\2\2\u0107"+
		"\u0108\7v\2\2\u0108\"\3\2\2\2\u0109\u010a\7k\2\2\u010a\u010b\7h\2\2\u010b"+
		"$\3\2\2\2\u010c\u010d\7k\2\2\u010d\u010e\7p\2\2\u010e\u010f\7v\2\2\u010f"+
		"\u0110\7g\2\2\u0110\u0111\7i\2\2\u0111\u0112\7g\2\2\u0112\u0113\7t\2\2"+
		"\u0113&\3\2\2\2\u0114\u0115\7p\2\2\u0115\u0116\7g\2\2\u0116\u0117\7y\2"+
		"\2\u0117(\3\2\2\2\u0118\u0119\7p\2\2\u0119\u011a\7w\2\2\u011a\u011b\7"+
		"n\2\2\u011b\u011c\7n\2\2\u011c*\3\2\2\2\u011d\u011e\7t\2\2\u011e\u011f"+
		"\7g\2\2\u011f\u0120\7v\2\2\u0120\u0121\7w\2\2\u0121\u0122\7t\2\2\u0122"+
		"\u0123\7p\2\2\u0123,\3\2\2\2\u0124\u0125\7u\2\2\u0125\u0126\7g\2\2\u0126"+
		"\u0127\7n\2\2\u0127\u0128\7h\2\2\u0128.\3\2\2\2\u0129\u012a\7u\2\2\u012a"+
		"\u012b\7v\2\2\u012b\u012c\7c\2\2\u012c\u012d\7v\2\2\u012d\u012e\7k\2\2"+
		"\u012e\u012f\7e\2\2\u012f\60\3\2\2\2\u0130\u0131\7u\2\2\u0131\u0132\7"+
		"v\2\2\u0132\u0133\7t\2\2\u0133\u0134\7k\2\2\u0134\u0135\7p\2\2\u0135\u0136"+
		"\7i\2\2\u0136\62\3\2\2\2\u0137\u0138\7v\2\2\u0138\u0139\7j\2\2\u0139\u013a"+
		"\7g\2\2\u013a\u013b\7p\2\2\u013b\64\3\2\2\2\u013c\u013d\7v\2\2\u013d\u013e"+
		"\7t\2\2\u013e\u013f\7w\2\2\u013f\u0140\7g\2\2\u0140\66\3\2\2\2\u0141\u0142"+
		"\7x\2\2\u0142\u0143\7q\2\2\u0143\u0144\7k\2\2\u0144\u0145\7f\2\2\u0145"+
		"8\3\2\2\2\u0146\u0147\7y\2\2\u0147\u0148\7j\2\2\u0148\u0149\7k\2\2\u0149"+
		"\u014a\7n\2\2\u014a\u014b\7g\2\2\u014b:\3\2\2\2\u014c\u014d\7k\2\2\u014d"+
		"\u014e\7q\2\2\u014e\u014f\7\60\2\2\u014f\u0150\7t\2\2\u0150\u0151\7g\2"+
		"\2\u0151\u0152\7c\2\2\u0152\u0153\7f\2\2\u0153\u0154\7K\2\2\u0154\u0155"+
		"\7p\2\2\u0155\u0156\7v\2\2\u0156<\3\2\2\2\u0157\u0158\7k\2\2\u0158\u0159"+
		"\7q\2\2\u0159\u015a\7\60\2\2\u015a\u015b\7y\2\2\u015b\u015c\7t\2\2\u015c"+
		"\u015d\7k\2\2\u015d\u015e\7v\2\2\u015e\u015f\7g\2\2\u015f\u0160\7K\2\2"+
		"\u0160\u0161\7p\2\2\u0161\u0162\7v\2\2\u0162>\3\2\2\2\u0163\u0164\7k\2"+
		"\2\u0164\u0165\7q\2\2\u0165\u0166\7\60\2\2\u0166\u0167\7y\2\2\u0167\u0168"+
		"\7t\2\2\u0168\u0169\7k\2\2\u0169\u016a\7v\2\2\u016a\u016b\7g\2\2\u016b"+
		"\u016c\7K\2\2\u016c\u016d\7p\2\2\u016d\u016e\7v\2\2\u016e\u016f\7N\2\2"+
		"\u016f\u0170\7p\2\2\u0170@\3\2\2\2\u0171\u0172\7k\2\2\u0172\u0173\7q\2"+
		"\2\u0173\u0174\7\60\2\2\u0174\u0175\7t\2\2\u0175\u0176\7g\2\2\u0176\u0177"+
		"\7c\2\2\u0177\u0178\7f\2\2\u0178\u0179\7H\2\2\u0179\u017a\7n\2\2\u017a"+
		"\u017b\7q\2\2\u017b\u017c\7c\2\2\u017c\u017d\7v\2\2\u017dB\3\2\2\2\u017e"+
		"\u017f\7k\2\2\u017f\u0180\7q\2\2\u0180\u0181\7\60\2\2\u0181\u0182\7y\2"+
		"\2\u0182\u0183\7t\2\2\u0183\u0184\7k\2\2\u0184\u0185\7v\2\2\u0185\u0186"+
		"\7g\2\2\u0186\u0187\7H\2\2\u0187\u0188\7n\2\2\u0188\u0189\7q\2\2\u0189"+
		"\u018a\7c\2\2\u018a\u018b\7v\2\2\u018bD\3\2\2\2\u018c\u018d\7k\2\2\u018d"+
		"\u018e\7q\2\2\u018e\u018f\7\60\2\2\u018f\u0190\7y\2\2\u0190\u0191\7t\2"+
		"\2\u0191\u0192\7k\2\2\u0192\u0193\7v\2\2\u0193\u0194\7g\2\2\u0194\u0195"+
		"\7H\2\2\u0195\u0196\7n\2\2\u0196\u0197\7q\2\2\u0197\u0198\7c\2\2\u0198"+
		"\u0199\7v\2\2\u0199\u019a\7N\2\2\u019a\u019b\7p\2\2\u019bF\3\2\2\2\u019c"+
		"\u019d\7k\2\2\u019d\u019e\7q\2\2\u019e\u019f\7\60\2\2\u019f\u01a0\7t\2"+
		"\2\u01a0\u01a1\7g\2\2\u01a1\u01a2\7c\2\2\u01a2\u01a3\7f\2\2\u01a3\u01a4"+
		"\7D\2\2\u01a4\u01a5\7q\2\2\u01a5\u01a6\7q\2\2\u01a6\u01a7\7n\2\2\u01a7"+
		"H\3\2\2\2\u01a8\u01a9\7k\2\2\u01a9\u01aa\7q\2\2\u01aa\u01ab\7\60\2\2\u01ab"+
		"\u01ac\7y\2\2\u01ac\u01ad\7t\2\2\u01ad\u01ae\7k\2\2\u01ae\u01af\7v\2\2"+
		"\u01af\u01b0\7g\2\2\u01b0\u01b1\7D\2\2\u01b1\u01b2\7q\2\2\u01b2\u01b3"+
		"\7q\2\2\u01b3\u01b4\7n\2\2\u01b4J\3\2\2\2\u01b5\u01b6\7k\2\2\u01b6\u01b7"+
		"\7q\2\2\u01b7\u01b8\7\60\2\2\u01b8\u01b9\7y\2\2\u01b9\u01ba\7t\2\2\u01ba"+
		"\u01bb\7k\2\2\u01bb\u01bc\7v\2\2\u01bc\u01bd\7g\2\2\u01bd\u01be\7D\2\2"+
		"\u01be\u01bf\7q\2\2\u01bf\u01c0\7q\2\2\u01c0\u01c1\7n\2\2\u01c1\u01c2"+
		"\7N\2\2\u01c2\u01c3\7p\2\2\u01c3L\3\2\2\2\u01c4\u01c5\7k\2\2\u01c5\u01c6"+
		"\7q\2\2\u01c6\u01c7\7\60\2\2\u01c7\u01c8\7t\2\2\u01c8\u01c9\7g\2\2\u01c9"+
		"\u01ca\7c\2\2\u01ca\u01cb\7f\2\2\u01cb\u01cc\7U\2\2\u01cc\u01cd\7v\2\2"+
		"\u01cd\u01ce\7t\2\2\u01ceN\3\2\2\2\u01cf\u01d0\7k\2\2\u01d0\u01d1\7q\2"+
		"\2\u01d1\u01d2\7\60\2\2\u01d2\u01d3\7y\2\2\u01d3\u01d4\7t\2\2\u01d4\u01d5"+
		"\7k\2\2\u01d5\u01d6\7v\2\2\u01d6\u01d7\7g\2\2\u01d7\u01d8\7U\2\2\u01d8"+
		"\u01d9\7v\2\2\u01d9\u01da\7t\2\2\u01daP\3\2\2\2\u01db\u01dc\7k\2\2\u01dc"+
		"\u01dd\7q\2\2\u01dd\u01de\7\60\2\2\u01de\u01df\7y\2\2\u01df\u01e0\7t\2"+
		"\2\u01e0\u01e1\7k\2\2\u01e1\u01e2\7v\2\2\u01e2\u01e3\7g\2\2\u01e3\u01e4"+
		"\7U\2\2\u01e4\u01e5\7v\2\2\u01e5\u01e6\7t\2\2\u01e6\u01e7\7N\2\2\u01e7"+
		"\u01e8\7p\2\2\u01e8R\3\2\2\2\u01e9\u01ea\7k\2\2\u01ea\u01eb\7q\2\2\u01eb"+
		"T\3\2\2\2\u01ec\u01ed\7-\2\2\u01edV\3\2\2\2\u01ee\u01ef\7/\2\2\u01efX"+
		"\3\2\2\2\u01f0\u01f1\7,\2\2\u01f1Z\3\2\2\2\u01f2\u01f3\7\61\2\2\u01f3"+
		"\\\3\2\2\2\u01f4\u01f5\7^\2\2\u01f5^\3\2\2\2\u01f6\u01f7\7\'\2\2\u01f7"+
		"`\3\2\2\2\u01f8\u01f9\7>\2\2\u01f9\u01fa\7@\2\2\u01fab\3\2\2\2\u01fb\u01fc"+
		"\7?\2\2\u01fc\u01fd\7?\2\2\u01fdd\3\2\2\2\u01fe\u01ff\7>\2\2\u01fff\3"+
		"\2\2\2\u0200\u0201\7@\2\2\u0201h\3\2\2\2\u0202\u0203\7>\2\2\u0203\u0204"+
		"\7?\2\2\u0204j\3\2\2\2\u0205\u0206\7@\2\2\u0206\u0207\7?\2\2\u0207l\3"+
		"\2\2\2\u0208\u0209\7~\2\2\u0209\u020a\7~\2\2\u020an\3\2\2\2\u020b\u020c"+
		"\7(\2\2\u020c\u020d\7(\2\2\u020dp\3\2\2\2\u020e\u020f\7#\2\2\u020fr\3"+
		"\2\2\2\u0210\u0211\7`\2\2\u0211t\3\2\2\2\u0212\u0214\t\4\2\2\u0213\u0212"+
		"\3\2\2\2\u0214\u0215\3\2\2\2\u0215\u0213\3\2\2\2\u0215\u0216\3\2\2\2\u0216"+
		"v\3\2\2\2\u0217\u0219\t\4\2\2\u0218\u0217\3\2\2\2\u0219\u021a\3\2\2\2"+
		"\u021a\u0218\3\2\2\2\u021a\u021b\3\2\2\2\u021b\u0223\3\2\2\2\u021c\u0220"+
		"\7\60\2\2\u021d\u021f\t\4\2\2\u021e\u021d\3\2\2\2\u021f\u0222\3\2\2\2"+
		"\u0220\u021e\3\2\2\2\u0220\u0221\3\2\2\2\u0221\u0224\3\2\2\2\u0222\u0220"+
		"\3\2\2\2\u0223\u021c\3\2\2\2\u0223\u0224\3\2\2\2\u0224\u022e\3\2\2\2\u0225"+
		"\u0227\t\5\2\2\u0226\u0228\t\6\2\2\u0227\u0226\3\2\2\2\u0227\u0228\3\2"+
		"\2\2\u0228\u022a\3\2\2\2\u0229\u022b\t\4\2\2\u022a\u0229\3\2\2\2\u022b"+
		"\u022c\3\2\2\2\u022c\u022a\3\2\2\2\u022c\u022d\3\2\2\2\u022d\u022f\3\2"+
		"\2\2\u022e\u0225\3\2\2\2\u022e\u022f\3\2\2\2\u022fx\3\2\2\2\u0230\u0234"+
		"\t\7\2\2\u0231\u0233\t\b\2\2\u0232\u0231\3\2\2\2\u0233\u0236\3\2\2\2\u0234"+
		"\u0232\3\2\2\2\u0234\u0235\3\2\2\2\u0235z\3\2\2\2\u0236\u0234\3\2\2\2"+
		"\u0237\u0238\7]\2\2\u0238|\3\2\2\2\u0239\u023a\7_\2\2\u023a~\3\2\2\2\u023b"+
		"\u023c\7}\2\2\u023c\u0080\3\2\2\2\u023d\u023e\7\177\2\2\u023e\u0082\3"+
		"\2\2\2\u023f\u0240\7*\2\2\u0240\u0084\3\2\2\2\u0241\u0242\7+\2\2\u0242"+
		"\u0086\3\2\2\2\u0243\u0244\7=\2\2\u0244\u0088\3\2\2\2\u0245\u0246\7<\2"+
		"\2\u0246\u008a\3\2\2\2\u0247\u0248\7\60\2\2\u0248\u008c\3\2\2\2\u0249"+
		"\u024a\7.\2\2\u024a\u008e\3\2\2\2\u024b\u024c\7<\2\2\u024c\u024d\7?\2"+
		"\2\u024d\u0090\3\2\2\2\21\2\u00a2\u00a4\u00b0\u00b4\u00be\u00c9\u0215"+
		"\u021a\u0220\u0223\u0227\u022c\u022e\u0234\4\3\4\2\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}