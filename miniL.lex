	/* cs152-miniL phase1 */
   
%{   
	/* write your C code here for definitions of variables and including headers */
	int currentRow = 1;
	int currentCol = 0;
%}

	/* some common rules */
   
	/* reserved words */
FUNCTION        "function"
BEGIN_PARAMS    "beginparams"
END_PARAMS      "endparams"
BEGIN_LOCALS    "beginlocals"
END_LOCALS      "endlocals"
BEGIN_BODY      "beginbody"
END_BODY        "endbody"
INTEGER         "integer"
ARRAY           "array"
OF              "of"
IF              "if"
THEN            "then"
ENDIF           "endif"
ELSE            "else"
WHILE           "while"
DO              "do"
BEGINLOOP       "beginloop"
ENDLOOP         "endloop"
CONTINUE        "continue"
BREAK           "break"
READ            "read"
WRITE           "write"
NOT             "not"
TRUE            "true"
FALSE           "false"
RETURN          "return"

	/* arithmetic operators */
ADD		"+"
SUB		"-"
MULT		"*"
DIV		"/"
MOD		"%"
  
	/* comparison operators */
EQ    "=="
NEQ   "<>"
LT    "<"     
GT    ">"
LTE   "<="
GTE   ">="

	/* identifiers and numbers */
IDENT		[a-zA-Z]+([a-zA-Z0-9_]*[a-zA-Z0-9])*
IDENT_ERROR_1	[0-9_]+[a-zA-Z]+([a-zA-Z0-9_]*[a-zA-Z0-9])*
IDENT_ERROR_2	[a-zA-Z]+([a-zA-Z0-9_]*[a-zA-Z0-9])*_+
NUMBER		[0-9]+

	/* special symbols */
SEMICOLON        ";"
COLON            ":"
COMMA            ","
L_PAREN          "("
R_PAREN          ")"
L_SQUARE_BRACKET "["
R_SQUARE_BRACKET "]"
ASSIGN           ":="

	/* other */
COMMENT_STRING	[#][#][^\n]*[\n]
NEWLINE		^[\n]
WHITESPACE	[\r\t\f\v ]

%%
	/* specific lexer rules in regex */
	/*captures comment line */

	/*reserved words*/
{FUNCTION}			{ printf("FUNCTION\n");  currentCol += yyleng;}
{BEGIN_PARAMS}			{ printf("BEGIN_PARAMS");  currentCol += yyleng;}
{END_PARAMS}			{ printf("END_PARAMS");  currentCol += yyleng;}
{BEGIN_LOCALS}			{ printf("BEGIN_LOCALS");  currentCol += yyleng;}
{END_LOCALS}			{ printf("END_LOCALS");  currentCol += yyleng;}
{BEGIN_BODY}{WHITESPACE}+	{ printf("BEGIN_BODY\n");  currentCol += yyleng;}
{BEGIN_BODY}			{ printf("BEGIN_BODY");  currentCol += yyleng;}
{END_BODY}			{ printf("END_BODY");  currentCol += yyleng;}
{INTEGER}			{ printf("INTEGER\n");  currentCol += yyleng;}
{ARRAY}				{ printf("ARRAY\n");  currentCol += yyleng;}
{OF}				{ printf("OF\n");  currentCol += yyleng;}
{IF}				{ printf("IF\n");  currentCol += yyleng;}
{THEN}"\n"			{ printf("THEN\n"); currentRow++; currentCol = 0;}
{THEN}				{ printf("THEN\n");  currentCol += yyleng;}
{ENDIF}				{ printf("ENDIF\n"); currentCol += yyleng; }
{ELSE}				{ printf("ELSE");  currentCol += yyleng;}
{WHILE}				{ printf("WHILE\n");  currentCol += yyleng;}
{DO}				{ printf("DO");  currentCol += yyleng;}
{BEGINLOOP}			{ printf("BEGINLOOP");  currentCol += yyleng;}
{ENDLOOP}			{ printf("ENDLOOP\n");  currentCol += yyleng;}
{CONTINUE}			{ printf("CONTINUE\n");  currentCol += yyleng;}
{BREAK}				{ printf("BREAK\n");  currentCol += yyleng;}
{READ}				{ printf("READ\n");  currentCol += yyleng;}
{WRITE}				{ printf("WRITE\n"); currentCol += yyleng; }
{NOT}				{ printf("NOT\n"); currentCol += yyleng; }
{TRUE}				{ printf("TRUE\n");  currentCol += yyleng;}
{FALSE}				{ printf("FALSE\n");  currentCol += yyleng;}
{RETURN}			{ printf("RETURN\n");  currentCol += yyleng;}

	/* arithmetic operators */

{ADD}          { printf("ADD\n");  currentCol += yyleng;}
{SUB}          { printf("SUB\n");  currentCol += yyleng;}
{MULT}         { printf("MULT\n");  currentCol += yyleng;}
{DIV}          { printf("DIV\n");  currentCol += yyleng;}
{MOD}          { printf("MOD\n");  currentCol += yyleng;}

	/* comparison operators */

{LTE}          { printf("LTE\n");  currentCol += yyleng;}
{GTE}          { printf("GTE\n");  currentCol += yyleng;}
{EQ}           { printf("EQ\n");  currentCol += yyleng;}
{NEQ}          { printf("NEQ\n");  currentCol += yyleng;}
{LT}           { printf("LT\n");  currentCol += yyleng;}
{GT}           { printf("GT\n");  currentCol += yyleng;}

	/* identifiers */
{IDENT_ERROR_1}       { printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter.", currentRow, currentCol,yytext); return;}
{IDENT_ERROR_2}       { printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore.", currentRow, currentCol, yytext); return;}
{IDENT}               { printf("IDENT %s\n", yytext);  currentCol += yyleng;}
{NUMBER}              { printf("NUMBER %d\n", atoi(yytext));  currentCol += yyleng;}

	/* special symbols*/

{SEMICOLON}"\n"      { printf("SEMICOLON\n"); currentRow++; currentCol = 0; }
{SEMICOLON}          { printf("SEMICOLON\n");  currentCol += yyleng;}
{COLON}              { printf("COLON\n");  currentCol += yyleng;}
{COMMA}       	     { printf("COMMA\n");  currentCol += yyleng;}
{L_PAREN}            { printf("L_PAREN\n"); currentCol += yyleng; }
{R_PAREN}            { printf("R_PAREN\n"); currentCol += yyleng; }
{L_SQUARE_BRACKET}   { printf("L_SQUARE_BRACKET\n");  currentCol += yyleng;}
{R_SQUARE_BRACKET}   { printf("R_SQUARE_BRACKET\n"); currentCol += yyleng; }
{ASSIGN}             { printf("ASSIGN\n"); currentCol += yyleng; }

	/* other */
   {COMMENT_STRING}            {currentRow++; currentCol = 0; } 
   {NEWLINE}                   {currentRow++; currentCol = 0; }
   [\n]                        {printf("\n"); currentRow++; currentCol = 0; }
   {WHITESPACE}                {currentCol+=yyleng;}

   [^\n]                       {printf("Error at line %d, column %d: unrecognized symbol \"%s\".", currentRow, currentCol, yytext); return;}
%%
	/* C functions used in lexer */

int main(int argc, char ** argv)
{
   yylex();
   printf("\n");
}

