package SL;

%%

%class Lexer
%cup
%public
%type Token
%line
%scanerror LexicalError
%state NUM

%{
StringBuffer string = new StringBuffer();

public int getLineNumber() { return yyline + 1; }
%}

%eofval{
  return new Token(sym.EOF, getLineNumber());
%eofval}

DIGIT = [0-9]
NATURAL = 0|([1-9]{DIGIT}*)
FLOAT = {NATURAL}\.{DIGIT}*[1-9]
NUMBER = {NATURAL}|{FLOAT}
LETTER = [a-z]|[A-Z]
IDENTIFIER = (_|{LETTER})(_|{DIGIT}|{LETTER})*
QUOTE = \"([^\"\n\\]|\\[tns\\\"])*\"

Ignore = \r|\n|\r\n|\t


/* comments */
COMMENT = {TraditionalComment} | {EndOfLineComment} | {DocumentationComment}
TraditionalComment   = "/*" [^*] ~"*/" | "/*" "*"+ "/"
InputCharacter = [^\r\n]
LineTerminator = \r|\n|\r\n
EndOfLineComment     = "//" {InputCharacter}* {LineTerminator}?
DocumentationComment = "/**" {CommentContent} "*"+ "/"
CommentContent       = ( [^*] | \*+ [^/*] )*

TABLE = TABLE | table

%%

<YYINITIAL> {
  
  "assert" 		{ return new Token(yytext(), sym.ASSERT, getLineNumber());}
  "def" 		{ return new Token(yytext(), sym.DEF, getLineNumber());} 
  "else" 		{ return new Token(yytext(), sym.ELSE, getLineNumber());}
  "if" 			{ return new Token(yytext(), sym.IF, getLineNumber());}   
  "for" 		{ return new Token(yytext(), sym.FOR, getLineNumber()); } 
  "var" 		{ return new Token(yytext(), sym.VAR, getLineNumber());} 
  "COLS" 		{ return new Token(yytext(), sym.COLS, getLineNumber()); }
  "ROWS"		{ return new Token(yytext(), sym.ROWS, getLineNumber()); }
  "=" 			{ return new Token(yytext(), sym.ASSIGN, getLineNumber());}
  "+=" 			{ return new Token(yytext(), sym.ASSIGN_PLUS, getLineNumber());}
  "-="		 	{ return new Token(yytext(), sym.ASSIGN_MINUS, getLineNumber());} 
  "*="			{ return new Token(yytext(), sym.ASSIGN_TIMES, getLineNumber()); }
  "/=" 			{ return new Token(yytext(), sym.ASSIGN_DIV, getLineNumber());}
  ":" 			{ return new Token(yytext(), sym.COLON, getLineNumber());}
  "," 			{ return new Token(yytext(), sym.COMMA, getLineNumber());} 
  "/"           { return new Token(yytext(), sym.DIVIDE, getLineNumber()); }
  ".." 			{ return new Token(yytext(), sym.DOTDOT, getLineNumber());}
  "==" 			{ return new Token(yytext(), sym.EQ, getLineNumber());}
  ">" 			{ return new Token(yytext(), sym.GT, getLineNumber());} 
  ">=" 			{ return new Token(yytext(), sym.GTE, getLineNumber());}
  "/\\" 		{ return new Token(yytext(), sym.LAND, getLineNumber());}
  "["           { return new Token(yytext(), sym.LB, getLineNumber()); }
  "(" 			{ return new Token(yytext(), sym.LP, getLineNumber());}
  "{"           { return new Token(yytext(), sym.LCB, getLineNumber()); }
  "!" 			{ return new Token(yytext(), sym.NOT, getLineNumber());}
  "\\/" 		{ return new Token(yytext(), sym.LOR, getLineNumber());}
  "<" 			{ return new Token(yytext(), sym.LT, getLineNumber());}  
  "<="   		{ return new Token(yytext(), sym.LTE, getLineNumber()); }
  "-"		    { return new Token(yytext(), sym.MINUS, getLineNumber()); }
  "*" 			{ return new Token(yytext(), sym.TIMES, getLineNumber());} 
  "!=" 			{ return new Token(yytext(), sym.NEQ, getLineNumber());}
  "+" 			{ return new Token(yytext(), sym.PLUS, getLineNumber());}
  "]"           { return new Token(yytext(), sym.RB, getLineNumber()); }
  "}"           { return new Token(yytext(), sym.RCB, getLineNumber()); }
  ")" 			{ return new Token(yytext(), sym.RP, getLineNumber());} 
  ";" 			{ return new Token(yytext(), sym.SEMI, getLineNumber()); } 
  "error"       { return new Token(yytext(), sym.ERROR, getLineNumber()); }
  {TABLE} 		{ return new Token(yytext(), sym.TABLE, getLineNumber());}
  {NUMBER} 		{ return new Token(yytext(), sym.NUM, getLineNumber(), new Float(yytext()));} 
  {QUOTE} 		{ return new Token(yytext(), sym.QUOTE, getLineNumber(), yytext().substring(1, yytext().length()-1));}
  {IDENTIFIER}  { return new Token(yytext(), sym.ID, getLineNumber(), yytext());} 
  {COMMENT}     {}  
  {Ignore}    {}
  " "                 {}
  "."                 {}
}
