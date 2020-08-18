%option noyywrap
%{

 /* DEMO scanner
  * 
  * Written at Rice University, Houston, Texas.
  *
  */
 
 /* definitions */

#ifdef FLEX_SCANNER
extern int yylineno;
#define YYINCR(n) yylineno += n
#else
#define YYINCR(n)
#endif

#include "tokens.h"
#include "strings.h"
#include "demo.h"


int CTRL_M = 0;
/*int total_E = 0;*/

%}

/* Reserved Words */
AND   "and"
BY    "by"
CHAR  "char"
ELSE  "else"
FOR   "for"
IF    "if"
INT   "int"
NOT   "not"
OR    "or"
PROCEDURE "procedure"
READ  "read"
THEN  "then"
TO    "to"
WHILE "while"
WRITE "write"
 
LT    "<"
LE    "<="
EQ    "=="
NE    "!="
GT    ">"
GE    ">="
 
 
COMMENT   "//"[^\n]*
NAME      [A-Za-z][A-Za-z0-9]*
NUMBER    [0-9][0-9]*
CHARCONST \'(.|\\n|\\t)\'
 
%%
 /* rules */
[\n] {YYINCR(1);}
[ \t]+ {}

{AND} {return AND;}
{BY} {return BY;}
{CHAR} {return CHAR;}
{ELSE} {return ELSE;}
{FOR} {return FOR;}
{IF} {return IF;}
{INT} {return INT;}
{NOT} {return NOT;}
{OR} {return OR;}
{PROCEDURE} {return PROCEDURE;}
{READ} {return READ;}
{THEN} {return THEN;}
{TO} {return TO;}
{WHILE} {return WHILE;}
{WRITE} {return WRITE;}

{LT} {return LT;}
{LE} {return LE;}
{EQ} {return EQ;}
{NE} {return NE;}
{GT} {return GT;}
{GE} {return GE;}

{COMMENT} {}
{NAME} {return NAME;}
{NUMBER} {return NUMBER;}
{CHARCONST} {return CHARCONST;}

"+" {return '+';}
"-" {return '-';}
"*" {return '*';}
"/" {return '/';}
":" {return ':';}
";" {return ';';}
"," {return ',';}
"=" {return '=';}
"(" {return '(';}
")" {return ')';}
"[" {return '[';}
"]" {return ']';}
"{" {return '{';}
"}" {return '}';}

. { 
    return UNKNOWN;
  }
  

   
%%

/* user code */
/*
void yyerror(const char* msg){
    fprintf (stderr,"unexpected error %s at line %d \n", msg, yylineno);
    total_E++;
}

int yywarp(){
    return 1;
}
*/
