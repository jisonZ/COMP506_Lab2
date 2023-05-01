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

%type <val> MatchStmt
%type <val> OpenStmt

%type <val> OrTerm
%type <val> AndTerm
%type <val> RelExpr

%type <val> Charconst

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
        | PROCEDURE Name LBRACKET
              Decls
          RBRACKET {yyerror("No statements");}
; 

/* Declaration Syntax */

Decls  :   Decls Decl SEMICOLON { cur = TYPE_UNK; }
       |   Decl SEMICOLON { cur = TYPE_UNK; }
;

Decl   :   Type  SpecList
        /* |  error SpecList{yyerror("type error");} */
;

/* ----- TODO add AST for following rules ---- */ 
Type   :   INT	{ cur = TYPE_INT; }
       |   CHAR { cur = TYPE_CHAR; }
       |  READ { yyerror("Read not allowed\n"); yyerrok;}
;

SpecList:  SpecList COMMA Spec
       |   Spec
;

Spec   :   Name

/* ----------------------------------------- */ 

/* The Statment Grammar */
Stmts  :   Stmts Stmt	{ 
              add_sybling_node($1, $2);	/* Stmts = root Stmt = sibling*/
							$$ = $1; }
       |  Stmt  { 	
              children[0] = $1;
       				$$ = make_op_node( NODE_STATEMENTS, children, 1); }
;

/* ----- TODO add AST for following rules ---- */ 

Stmt  :   MatchStmt {$$ = $1;}
      |   OpenStmt {$$ = $1;}
;

KeyWords: AND
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

MatchStmt: Reference EQUALS Expr SEMICOLON {  
                      children[0] = $1;
											children[1] = $3;
											$$ = make_op_node( NODE_ASSIGN, children, 2);
                                          }
      | KeyWords EQUALS Expr SEMICOLON {
        yyerror("cannot assign to a keyword\n");
      }
      | Write
      | Name Name {
            	printf("%s at ",$1);
            	yyerror("is not a key word.\n");
      }
      | IF LPAREN Bool RPAREN THEN MatchStmt ELSE MatchStmt {  
                      children[0] = $3;
											children[1] = $6;
                      children[2] = $8;
											$$ = make_op_node( NODE_IF_ELSE, children, 3);
                                          }
      /* | LBRACKET Stmts RBRACKET {
        $$ = $1;} */

      | WHILE LPAREN Bool RPAREN LBRACKET Stmts RBRACKET {
                      children[0] = $3;
                      children[1] = $6;
                      $$ = make_op_node( NODE_WHILE, children, 2);
      }
;

OpenStmt: IF LPAREN Bool RPAREN THEN Stmt {  
                      children[0] = $3;
                      children[1] = $6;
											$$ = make_op_node( NODE_IF, children, 2);
                                          }
        | IF LPAREN Bool RPAREN THEN MatchStmt ELSE OpenStmt {  
                      children[0] = $3;
                      children[1] = $6; 
                      children[2] = $8;
											$$ = make_op_node( NODE_IF_ELSE, children, 3);
                                          }
;

Bool: NOT OrTerm {  children[0] = $2;
											$$ = make_op_node( NODE_NOT, children, 1);
                                          }
    | OrTerm {$$ = $1;}
;

OrTerm: OrTerm OR AndTerm {  
                      children[0] = $1;
											children[1] = $3;
                      // printf("OrTerm(%s:%d) OR AndTerm(%s:%d)\n", 
                      // children[0]->var_name, children[0]->var_val, 
                      // children[1]->var_name, children[1]->var_val);
											$$ = make_op_node( NODE_OR, children, 2);
                                          }
      | AndTerm {$$ = $1;}
;

AndTerm: AndTerm AND RelExpr {  
                      children[0] = $1;
											children[1] = $3;
											$$ = make_op_node( NODE_AND, children, 2);
                                          }
        | RelExpr {           $$ = $1;}
;

RelExpr: RelExpr LT Expr {  children[0] = $1;
											children[1] = $3;
											$$ = make_op_node( NODE_LT, children, 2);
                                          }
      | RelExpr LE Expr {  children[0] = $1;
											children[1] = $3;
											$$ = make_op_node( NODE_LE, children, 2);
                                          }
      | RelExpr EQ Expr {  children[0] = $1;
											children[1] = $3;
											$$ = make_op_node( NODE_EQ, children, 2);
                                          }
      | RelExpr NE Expr {  children[0] = $1;
											children[1] = $3;
											$$ = make_op_node( NODE_NE, children, 2);
                                          }
      | RelExpr GE Expr {  children[0] = $1;
											children[1] = $3;
											$$ = make_op_node( NODE_GE, children, 2);
                                          }
      | RelExpr GT Expr {  children[0] = $1;
											children[1] = $3;
											$$ = make_op_node( NODE_GT, children, 2);
                                          }
      | Expr {$$ = $1;}
;
/* ----------------------------------------- */ 

Write  : WRITE Expr SEMICOLON { 
                children[0]=$2;
								$$ = make_op_node(NODE_WRITE, children, 1); }
;

/* ----- TODO add AST for following rules ---- */ 

/* The Expression Grammar */
Expr   : Expr PLUS Term {	children[0] = $1;
							children[1] = $3;
							$$ = make_op_node(NODE_PLUS, children, 2);}
       | Expr MINUS Term {	children[0] = $1;
							children[1] = $3;
							$$ = make_op_node(NODE_MINUS, children, 2);}
       | Term {$$ = $1;}
;

Term   : Factor {$$ = $1;}
        | Term TIMES Factor {
          children[0] = $1;
          children[1] = $3;
          $$ = make_op_node(NODE_TIMES, children, 2);
        }
        | Term DIVIDE Factor {
          children[0] = $1;
          children[1] = $3;
          $$ = make_op_node(NODE_DIVIDE, children, 2);
        }
;

Factor : Reference { $$ = $1; }
       | Number { $$ = $1; }
       | LPAREN Expr RPAREN {$$ = $1; }
       | Charconst
;

Charconst: CHARCONST {
  $$ = make_leaf_node(TYPE_CHAR,"char", TokenString);
}
;

/* ----------------------------------------- */ 

Reference :  Name { 
  // printf("Reference: get node name: %s\n", $1);
  $$ = make_leaf_node(NODE_NAME, strdup($1), 0);}
  /* | error {
    printf("keyword error");
  } */
;

Name   : NAME {
				  $$ = strdup(TokenString);
				  if (getsym(TokenString) == NULL && cur != TYPE_UNK) {

					putsym(TokenString, cur, 0);
				  }
              }
    /* | error {
      yyerror ("name error");
    } */
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
  /* printf("call yyerror: %d, %d\n", yylineno, ErrLine); */
  if (yylineno != ErrLine) {
    fprintf(stderr, "Line %d: %s\n",yylineno,s);
    syntax_error++;
  }

  ErrLine = yylineno;
  fprintf(LOG, "Parser, line %d: %s\n",yylineno,s);
  fprintf(LOG, "\tlexeme is '%s'.\n", yytext);

  /* will delete CODE and Storage Layout files back in main() */
}
