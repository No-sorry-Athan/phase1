   /* cs152-miniL phase1 */
   
%{   
   /* write your C code here for definitions of variables and including headers */
   int currentRow = 0;
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
ADD "+"
SUB "-"
MULT "*"
DIV "/"
MOD "%"
  
   /* comparison operators */

EQ    "=="
NEQ   "<>"
LT    "<"     
GT    ">"
LTE   "<="
GTE   ">="
   /* identifiers and numbers */
IDENT    [a-zA-Z]+([a-zA-Z0-9_][a-zA-Z0-9])*
IDENT_ERROR_1 [0-9_]+[a-zA-Z]+([a-zA-Z0-9_][a-zA-Z0-9])*
IDENT_ERROR_2 [a-zA-Z]+([a-zA-Z0-9_][a-zA-Z0-9])*_
NUMBER   [0-9]+

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
COMMENT_STRING [#][#][^\n]*[\n]
NEWLINE     ^[\n]
WHITESPACE  [\r\t\f\v ]

%%
   /* specific lexer rules in regex */
 /*captures comment line */


 /*reserved words*/
{FUNCTION}    { printf("FUNCTION\n"); }
{BEGIN_PARAMS} { printf("BEGIN_PARAMS"); }
{END_PARAMS}   { printf("END_PARAMS"); }
{BEGIN_LOCALS} { printf("BEGIN_LOCALS"); }
{END_LOCALS}   { printf("END_LOCALS"); }
{BEGIN_BODY}{WHITESPACE}+ { printf("BEGIN_BODY\n"); }
{BEGIN_BODY}   { printf("BEGIN_BODY"); }
{END_BODY}     { printf("END_BODY"); }
{INTEGER}      { printf("INTEGER\n"); }
{ARRAY}        { printf("ARRAY\n"); }
{OF}           { printf("OF\n"); }
{IF}           { printf("IF\n"); }
{THEN}"\n"{WHITESPACE}+ {printf("THEN\n");}
{THEN}"\n"     { printf("THEN"); }
{THEN}         { printf("THEN\n"); }
{ENDIF}        { printf("ENDIF\n"); }
{ELSE}         { printf("ELSE"); }
{WHILE}        { printf("WHILE\n"); }
{DO}           { printf("DO"); }
{BEGINLOOP}    { printf("BEGINLOOP"); }
{ENDLOOP}      { printf("ENDLOOP\n"); }
{CONTINUE}     { printf("CONTINUE\n"); }
{BREAK}        { printf("BREAK\n"); }
{READ}         { printf("READ\n"); }
{WRITE}        { printf("WRITE\n"); }
{NOT}          { printf("NOT\n"); }
{TRUE}         { printf("TRUE\n"); }
{FALSE}        { printf("FALSE\n"); }
{RETURN}       { printf("RETURN\n"); }

 /* arithmetic operators */

{ADD}          { printf("ADD\n"); }
{SUB}          { printf("SUB\n"); }
{MULT}         { printf("MULT\n"); }
{DIV}          { printf("DIV\n"); }
{MOD}          { printf("MOD\n"); }

 /* comparison operators */

{LTE}          { printf("LTE\n"); }
{GTE}          { printf("GTE\n"); }
{EQ}           { printf("EQ\n"); }
{NEQ}          { printf("NEQ\n"); }
{LT}           { printf("LT\n"); }
{GT}           { printf("GT\n"); }

 /* identifiers */
{IDENT_ERROR_1} {printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter.", currentRow, currentCol,yytext); }
{IDENT_ERROR_2} {printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore.", currentRow, currentCol, yytext); }
{IDENT}     { printf("IDENT %s\n", yytext); }
{NUMBER}    { printf("NUMBER %d\n", atoi(yytext)); }

 /* special symbols*/

{SEMICOLON}"\n"      { printf("SEMICOLON\n"); }
{SEMICOLON}          { printf("SEMICOLON\n"); }
{COLON}              { printf("COLON\n"); }
{COMMA}       	     { printf("COMMA\n"); }
{L_PAREN}            { printf("L_PAREN\n"); }
{R_PAREN}            { printf("R_PAREN\n"); }
{L_SQUARE_BRACKET}   { printf("L_SQUARE_BRACKET\n"); }
{R_SQUARE_BRACKET}   { printf("R_SQUARE_BRACKET\n"); }
{ASSIGN}             { printf("ASSIGN\n"); }

 /* other */
   {COMMENT_STRING}            {currentRow++; currentCol = 0; } 
   {NEWLINE}                   {currentRow++; currentCol = 0; }
   {WHITESPACE}                {currentCol++;}

   [^\n]                       {printf("Error at line %d, column %d: unrecognized symbol \"%s\".", currentRow, currentCol, yytext);}
%%
	/* C functions used in lexer */

int main(int argc, char ** argv)
{
   yylex();
}

