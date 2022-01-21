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
IDENT "identifier"
NUMBER "number"

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
COMMENT_STRING "##.*"
CHARACTERS [a-zA-Z]
NUMBERS    [0-9]
WHITESPACE [\r\t\f\v ]
ENDLINE    [$]

%%
   /* specific lexer rules in regex */
 /*captures comment line */


 /*reserved words*/
{FUNCTION}    { printf("FUNCTION\n"); }
{BEGIN_PARAMS} { printf("BEGIN_PARAMS"); }
{END_PARAMS}   { printf("END_PARAMS"); }
{BEGIN_LOCALS} { printf("BEGIN_LOCALS"); }
{END_LOCALS}   { printf("END_LOCALS"); }
{BEGIN_BODY}   { printf("BEGIN_BODY"); }
{END_BODY}     { printf("END_BODY"); }
{INTEGER}      { printf("INTEGER"); }
{ARRAY}        { printf("ARRAY"); }
{OF}           { printf("OF"); }
{IF}           { printf("IF"); }
{THEN}         { printf("THEN"); }
{ENDIF}        { printf("ENDIF"); }
{ELSE}         { printf("ELSE"); }
{WHILE}        { printf("WHILE"); }
{DO}           { printf("DO"); }
{BEGINLOOP}    { printf("BEGINLOOP"); }
{ENDLOOP}      { printf("ENDLOOP"); }
{CONTINUE}     { printf("CONTINUE"); }
{BREAK}        { printf("BREAK"); }
{READ}         { printf("READ"); }
{WRITE}        { printf("WRITE"); }
{NOT}          { printf("NOT"); }
{TRUE}         { printf("TRUE"); }
{FALSE}        { printf("FALSE"); }
{RETURN}       { printf("RETURN"); }

 /* arithmetic operators */

{ADD}          { printf("ADD"); }
{SUB}          { printf("SUB"); }
{MULT}         { printf("MULT"); }
{DIV}          { printf("DIV"); }
{MOD}          { printf("MOD"); }

 /* comparison operators */

{LTE}          { printf("LTE"); }
{GTE}          { printf("GTE"); }
{EQ}           { printf("EQ"); }
{NEQ}          { printf("NEQ"); }
{LT}           { printf("LT"); }
{GT}           { printf("GT"); }

 /* identifiers */

{IDENT}{CHARACTERS}+ { printf("IDENT %s", yytext); }
{NUMBER}{NUMBERS}+   { printf("NUMBER %d", atoi(yytext)); }

 /* special symbols*/
{SEMICOLON}   { printf("SEMICOLON"); }
{COLON}       { printf("COLON\n"); }
{COMMA}       { printf("COMMA"); }
{L_PAREN}           { printf("L_PAREN"); }
{R_PAREN}           { printf("R_PAREN"); }
{L_SQUARE_BRACKET}  { printf("L_SQUARE_PAREN"); }
{R_SQUARE_BRACKET}  { printf("R_SQUARE_PAREN"); }
{ASSIGN}            { printf("ASSIGN "); }

 /* other */
 {WHITESPACE}*{COMMENT_STRING}  {}
 /* {ENDLINE}                   {} */
 /* " "                         {} */
 /* {WHITESPACE}*{CHARACTERS}+  { printf("INDENT ");} */
 /* {CHARACTERS}+        {printf("%s\n", yytext);} */
 /* {NUMBERS}+                  { printf("%d\n", atoi(yytext)); } */
%%
	/* C functions used in lexer */

int main(int argc, char ** argv)
{
   yylex();
}

