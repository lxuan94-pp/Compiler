%{ 
/* Copyright 2016, Keith D. Cooper & Linda Torczon
 * 
 * Written at Rice University, Houston, Texas as part
 * of the instructional materials for COMP 506.
 *
 * The University may have some rights in this work.
 *
 */

#define YYERROR_VERBOSE

#include <stdio.h>
#include "demo.h"

int yylineno;
char *yytext;
void yyerror( char const *);

%}

%token AND
%token BY
%token CHAR
%token ELSE
%token FOR
%token IF
%token INT
%token NOT
%token OR
%token PROCEDURE
%token READ
%token THEN 
%token TO
%token WHILE
%token WRITE

%token LT
%token LE
%token EQ
%token NE
%token GT
%token GE

%token COMMENT
%token NUMBER
%token NAME
%token CHARCONST
%token ENDOFFLINE

%token '+'
%token '-'
%token '*'
%token '/'
%token ':'     
%token ';'     
%token ','     
%token '='    
%token '('    
%token ')'    
%token '['    
%token ']'     
%token '{'     
%token '}' 

%token UNKNOWN

%left '+''-'
%left '*''/'

%nonassoc NO_ELSE
%nonassoc ELSE

/* Bison declarations */
%start  Grammar
%%
Grammar: Procedure
;

Procedure: PROCEDURE NAME '{' Decls Stmts '}'
          | PROCEDURE NAME '{' Decls Stmts {yyerror("Missing a '}' at the end of statment");yyclearin;yyerrok;}
          | PROCEDURE NAME '{' Decls Stmts '}' '}' {yyerror("Unexpected redundant '}', expecting ENDOFFILE or PROCEDURE");yyclearin;yyerrok;}
;

Decls: Decls Decl ';'
     | Decl ';'
;

Decl: Type SpecList
;

Type: INT
    | CHAR
;

SpecList: SpecList ',' Spec
        | Spec
;

Spec: NAME
    | NAME '[' Bounds ']'
;

Bounds: Bounds ',' Bound
      | Bound
;

Bound: NUMBER ':' NUMBER
;

KeyWord : AND
        | BY
        | CHAR
        | ELSE
        | FOR
        | IF
        | INT
        | NOT
        | OR
        | PROCEDURE
        | READ
        | THEN
        | TO
        | WHILE
        | WRITE
;

OP : '+'
   | '-'
   | '*'
   | '/'
;

Stmts: Stmts Stmt
     | Stmt
;

Stmt: Reference '=' Expr ';'
    | Reference OP '=' Expr ';' {yyerror("Unexpected operators, expecting '='");yyclearin;yyerrok;}
    | '{' Stmts '}'
    /*| '{' Stmts {yyerror("Missing a '}' at the end");yyclearin;}*/
    | WHILE '(' Bool ')' '{' Stmts '}'
    /*| WHILE '{' Bool ')' '{' Stmts {yyerror("Missing a '}' at the end of the statement");yyclearin;}*/
    | FOR NAME '=' Expr TO Expr BY Expr '{' Stmts '}'
    /*| FOR NAME '=' Expr TO Expr BY Expr '{' Stmts {yyerror("missing a '}' at the end");yyclearin;}*/
    | IF '(' Bool ')' THEN Stmt ELSEClause 
    | READ Reference ';'
    | WRITE Expr ';'
    /*| WRITE '=' Expr ';' {yyerror("can not assign to 'write' ");yyclearin;}*/
    /*| READ '=' Expr ';' {yyerror("can not assign to 'read' ");yyclearin;}*/
    /*| WHILE '=' Expr ';' {yyerror("can not assign to 'while' ");yyclearin;}*/
    /*| IF '=' Expr ';' {yyerror("can not assign to 'if' ");yyclearin;}*/
    | KeyWord '=' Expr ';' {yyerror("syntax error, unexpected '=', can not assign to a reserved word, expecting CHARCONST LPAREN or NAME or NUMBER ");yyclearin;yyerrok;}
    | ';'{ yyerror("Unexpected semicolon ';', indicates an empty statement");yyclearin;yyerrok;}
    | '{' '}'{ yyerror("Empty statement list"); yyclearin;}
    | NAME NAME ';' {yyerror("syntax error, invalid NAME, expecting '='");yyclearin;yyerrok;}
    | error ';'
;

ELSEClause: %prec NO_ELSE
          | ELSE Stmt
;


Bool: NOT OrTerm
    | OrTerm
;

OrTerm: OrTerm OR AndTerm
      | AndTerm
;

AndTerm: AndTerm AND RelExpr
       | RelExpr
;

RelExpr: RelExpr CompareClause
       | Expr
;

CompareClause: LT Expr
             | LE Expr
             | EQ Expr
             | NE Expr
             | GE Expr
             | GT Expr
;

Expr: Expr '+' Term
    | Expr '-' Term
    | Term
;

Term: Term '*' Factor
    | Term '/' Factor
    | Factor
;

Factor: '(' Expr ')'
      | Reference
      | NUMBER
      | CHARCONST
;

Reference: NAME
         | NAME '[' Exprs ']'
;

Exprs: Expr ',' Exprs
     | Expr
;


 /* Grammar rules  */
Grammar:  {return 1;};

%%


/* Epilogue */


void yyerror(s)
   const char *s;
{
	fprintf(stderr, "Line: %d error: %s  \n", yylineno,s);
  syntax_error++;
}

int yywrap()
{
  return 1;
};
