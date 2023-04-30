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
struct NODE * children[CHILD_NUM];

%}

%union {
  unsigned long int val;
  char             *cptr;
  struct NODE     *nodeptr;
  struct BP        *bp;
  struct LOOP      *loop;
  struct LOOPEXPR  *loopex;
  struct ITEHEAD   *ite;
}


%token <val> BY
%token <val> CHAR
%token <val> CHARCONST
%token <val> COLON
%token <val> COMMA
%token <val> DIVIDE
%token <val> ELSE
%token <val> ENDOFFILE
%token <val> EQUALS
%token <val> FOR
%token <val> IF
%token <val> INT
%token <val> LBRACKET
%token <val> LPAREN
%token <val> LSQUARE
%token <val> MINUS
%token <val> NAME
%token <val> ELE
%token <val> NOT
%token <val> NUMBER
%token <val> PLUS
%token <val> PROCEDURE
%token <val> RBRACKET
%token <val> READ
%token <val> RPAREN
%token <val> RSQUARE
%token <val> SEMICOLON
%token <val> THEN
%token <val> TIMES
%token <val> TO
%token <val> WHILE
%token <val> WRITE
%token <val> UNKNOWN
%token <val> STRING

%token <val> LT
%token <val> LE
%token <val> EQ
%token <val> NE
%token <val> GE
%token <val> GT
%token <val> AND
%token <val> OR

%type <val> Grammar

/* TODO: expand these types with your own grammar rules */

%type <val> Decl
%type <nodeptr> Expr
%type <nodeptr> Factor
%type <val> Name
%type <nodeptr> Number
%type <val> Proc
%type <val> ProcList
%type <val> While
%type <val> Bool
%type <val> Reference
%type <val> SpecList
%type <val> Spec
%type <val> Stmts
%type <val> Stmt
%type <nodeptr> Term
%type <val> Type
%type <val> Write

%start  Grammar

%%

 /* High level list of procedures */

Grammar:  ProcList ENDOFFILE { walk($1);
                                                           freeAST($1);
                                                           freeST(sym_table);
       return 1; }
;


ProcList :  ProcList Proc { add_sybling_node($1, $2);	/* Stmts = root Stmt = sibling*/
							$$ = $1;}
       |    Proc {	children[0] = $1;
					$$ = make_op_node( NODE_STATEMENTS, children, 1);}
;

Proc   :  PROCEDURE Name LBRACKET
              Decls Stmts
          RBRACKET { $$ = $5; }
       |  PROCEDURE Name LBRACKET Decls Stmts ENDOFFILE
          { yyerror("Missing final right bracket ('}')"); }
; 

/* Declaration Syntax */

Decls  :   Decls Decl SEMICOLON { cur = TYPE_UNK; }
       |   Decl SEMICOLON { cur = TYPE_UNK; }
;

Decl   :   Type  SpecList
;

Type   :   INT	{ cur = TYPE_INT; }
;

SpecList:  SpecList COMMA Spec
       |   Spec
;

Spec   :   Name
;

/* The Statment Grammar */
Stmts  :   Stmts Stmt	{ 	add_sybling_node($1, $2);	/* Stmts = root Stmt = sibling*/
							$$ = $1; }
       |  Stmt  { 	children[0] = $1;
       				$$ = make_op_node( NODE_STATEMENTS, children, 1); }
;

Stmt : Reference EQUALS Expr SEMICOLON {  children[0] = $1;
											children[1] = $3;
											$$ = make_op_node( NODE_ASSIGN, children, 2);
                                          }
      | Write
      | Name Name{
            	printf("%s at ",$1);
            	yyerror("is not a key word.\n");
      }
;

Write  : WRITE Expr SEMICOLON { children[0]=$2;
								$$ = make_op_node(NODE_WRITE, children, 1); }
;


/* The Expression Grammar */
Expr   : Expr PLUS Term {	children[0] = $1;
							children[1] = $3;
							$$ = make_op_node(NODE_PLUS, children, 2);}
       | Term {$$ = $1;}
;

Term   : Factor {$$ = $1;}
;

Factor : Reference { $$ = $1; }
       | Number { $$ = $1; }
;

Reference :  Name { $$ = make_leaf_node(NODE_NAME, strdup($1), 0);}

;

Name   : NAME {
				  $$ = strdup(TokenString);
				  if (getsym(TokenString) == NULL && cur != TYPE_UNK) {

					putsym(TokenString, cur, 0);
				  }
              }
;

Number : NUMBER {$$ = make_leaf_node(TYPE_INT,"number", TokenString);}
;

%%
static int ErrLine = 0;

int yywrap()
{
  return 1;
} 

void yyerror(char *s)
{
  if (yylineno != ErrLine) {
    fprintf(stderr, "Line %d: %s\n",yylineno,s);
    syntax_error++;
  }

  ErrLine = yylineno;
  fprintf(LOG, "Parser, line %d: %s\n",yylineno,s);
  fprintf(LOG, "\tlexeme is '%s'.\n", yytext);

  /* will delete CODE and Storage Layout files back in main() */
}
