/*_____________________C DECLARATIONS_____________________*/

%{
#include<stdio.h>
#include<string.h>
#include<stdlib.h>

typedef struct node
{
	char *token;
	struct node *left;
	struct node *center;
	struct node *right;
} node;



node *mknode (char *token, node *left, node* center, node *right);
void printtree (node *tree, int tab);
#define YYSTYPE struct node*

%}

/*_____________________YACC DECLARATIONS_____________________*/

%token BOOLEAN, INTEGER, CHARACTER, REAL, STRING, INT_P, CHAR_P, REAL_P, VAR, FUNC, PROC, IDENTIFIER, MAIN
%token IF, ELSE, WHILE
%token RETURN, NUL
%token AND, DIV, ASS, EQL, BIG, BIG_EQ, SML, SML_EQ, OR, ADD, MUL, AMP, CAR, SEMI, COL, COMMA, ABS, L_BRACE, R_BRACE, L_PARENTHESE, R_PARENTHESE, L_BRACKET, R_BRACKET
%token BOOL_VAR, CHAR_VAR, INT_VAR, REAL_VAR, STRING_VAR


%right ASS SEMI NOT
%left L_PARENTHESE R_PARENTHESE
%left EQL BIG BIG_EQ SML SML_EQ NOT_EQ
%left ADD SUB AND OR
%left MUL DIV

/*_____________________GRAMMER RULES_____________________*/

%%
s: main {printtree($1, 0);};

main: proc_or_func main {$$ = mknode("", $1, NULL, $2);}
	| main_proc {$$ = mknode("", $1, NULL, NULL);};

/*-------PROCEDURES-------*/

proc_or_func: func {$$ = mknode("", $1, NULL, NULL);} 
	| proc {$$ = mknode("", $1, NULL, NULL);};

proc: PROC id L_PARENTHESE args R_PARENTHESE proc_body {$$ = mknode("(PROC", $2, $4, $6);};

func: FUNC id args_and_types func_body {$$ = mknode("(FUNC", $2, $3, $4);};

return_var_type: RETURN var_type {$$ = mknode("(RETURN", $2, NULL, NULL);}
	| RETURN string_type {$$ = mknode("(RETURN", $2, NULL, NULL);};

main_proc: PROC MAIN L_PARENTHESE R_PARENTHESE proc_body {$$ = mknode("(MAIN PROC", $5, NULL, NULL);};

args: args_insertion {$$ = mknode("(ARGS", $1, NULL, NULL);};

args_and_types: L_PARENTHESE args right_parenthese return_var_type {$$ = mknode("", $2, $3, $4);};

args_insertion: id COMMA args_insertion {$$ = mknode("", $1, $3, NULL);}
	| id COL var_type SEMI args_insertion {$$ = mknode("", $1, $3, $5);} //TODO: change.
	| id COL var_type {$$ = mknode("", $1, $3, NULL);}
	| /*EMPTY*/ {$$ = mknode(" NONE", NULL, NULL, NULL);};

/*-------DECLARATION-------*/

declare: var_declare
	| string_declare;

var_declare: VAR var_id COL var_type SEMI {$$ = mknode("", $4, $2, NULL);};

string_declare: VAR var_id COL string_type L_BRACKET num R_BRACKET {$$ = mknode("", $4, $2, $6);};

string_type: STRING {$$ = mknode("STRING", NULL, NULL, NULL);};

var_id: id {$$ = mknode("", $1, NULL, NULL);}
	| id COMMA var_id {$$ = mknode("", $1, $3, NULL);};

var_type: BOOLEAN {$$ = mknode("BOOL", NULL, NULL, NULL);}
	| INTEGER {$$ = mknode("INT", NULL, NULL, NULL);}
	| CHARACTER {$$ = mknode("CHAR", NULL, NULL, NULL);}
	| REAL {$$ = mknode("REAL", NULL, NULL, NULL);}
	| INT_P {$$ = mknode("INT*", NULL, NULL, NULL);}
	| CHAR_P {$$ = mknode("CHAR*", NULL, NULL, NULL);}
	| REAL_P {$$ = mknode("REAL*", NULL, NULL, NULL);};

/*-------STATEMENTS-------*/

statements: statement statements {$$ = mknode("", $1, $2, NULL);}
	| statement {$$ = mknode("", $1, NULL, NULL);}
	| return_statement {$$ = mknode("", $1, NULL, NULL);};
	
statement: if_statement {$$ = mknode("", $1, NULL, NULL);}
	| while_statement {$$ = mknode("", $1, NULL, NULL);}
	| assignment_statement {$$ = mknode("", $1, NULL, NULL);};

if_statement: IF L_PARENTHESE exp R_PARENTHESE block {$$ = mknode("(IF", $3, $5, NULL);}
	| IF L_PARENTHESE exp R_PARENTHESE block ELSE block {$$ = mknode("(IF-ELSE", $3, $5, $7);}
	| IF L_PARENTHESE exp R_PARENTHESE statement {$$ = mknode("(IF", $3, $5, NULL);}
	| IF L_PARENTHESE exp R_PARENTHESE statement ELSE statement {$$ = mknode("(IF-ELSE", $3, $5, $7);};

while_statement: WHILE L_PARENTHESE exp R_PARENTHESE block {$$ = mknode("(WHILE", $3, $5, NULL);}
	| WHILE L_PARENTHESE exp R_PARENTHESE statement {$$ = mknode("(WHILE", $3, $5, NULL);};

return_statement: RETURN exp SEMI {$$ = mknode("(RETURN ", $2, NULL, NULL);};

assignment_statement: id ASS exp SEMI {$$ = mknode("=", $1, NULL, $3);};

func_proc_call: id L_PARENTHESE args R_PARENTHESE {$$ = mknode("func call", $1, NULL, $3);};

/*-------EXPRESSIONS-------*/

exp: exp OR exp {$$ = mknode("||", $1, NULL, $3);}
	| exp AND exp {$$ = mknode("&&", $1, NULL, $3);}
	| exp EQL exp {$$ = mknode("==", $1, NULL, $3);}
	| exp SML exp {$$ = mknode("<", $1, NULL, $3);}
	| exp SML_EQ exp {$$ = mknode("<=", $1, NULL, $3);}
	| exp BIG exp {$$ = mknode(">", $1, NULL, $3);}
	| exp BIG_EQ exp {$$ = mknode(">=", $1, NULL, $3);}
	| exp NOT_EQ exp {$$ = mknode("!=", $1, NULL, $3);}
	| NOT exp {$$ = mknode("!", NULL, NULL, $2);}
	| exp ADD exp {$$ = mknode("+", $1, NULL, $3);}
	| exp SUB exp {$$ = mknode("-", $1, NULL, $3);}
	| exp MUL exp {$$ = mknode("*", $1, NULL, $3);}
	| exp DIV exp {$$ = mknode("/", $1, NULL, $3);}
	| consts
	| pointers
	| parentheses;

consts: num | boolean | string | char | id | pointers | func_proc_call ;

id: IDENTIFIER {$$ = mknode(yylval, NULL, NULL, NULL);};

pointers: CAR id {$$ = mknode("^", $2, NULL, NULL);}
	| AMP id {$$ = mknode("&", $2, NULL, NULL);}
	| AMP id brackets {$$ = mknode("&", $2, NULL, $3);};

brackets: L_BRACKET num right_bracket {$$ = mknode("[", $2, NULL, $3);};

right_bracket: R_BRACKET {$$ = mknode("]", NULL, NULL, NULL);};

parentheses: L_PARENTHESE exp right_parenthese {$$ = mknode("(", $2, NULL, $3);};
	
right_parenthese: R_PARENTHESE {$$ = mknode(")", NULL, NULL, NULL);};
	
num: INT_VAR {$$ = mknode(yylval, NULL, NULL, NULL);}
	| REAL_VAR {$$ = mknode(yylval, NULL, NULL, NULL);};

boolean: BOOL_VAR {$$ = mknode(yylval, NULL, NULL, NULL);};

string: STRING_VAR {$$ = mknode(yylval, NULL, NULL, NULL);};

char: CHAR_VAR {$$ = mknode(yylval, NULL, NULL, NULL);};

/*-------BLOCKS-------*/

empty_block: L_BRACE right_brace {$$ = mknode("(BLOCK", $2, NULL, NULL);};

non_return_block: L_BRACE lines right_brace {$$ = mknode("(BLOCK", $2, NULL, $3);}
	| empty_block;

return_block: L_BRACE return_statement right_brace {$$ = mknode("(BLOCK", $2, NULL, $3);}
	| L_BRACE lines return_statement right_brace {$$ = mknode("(BLOCK", $2, $3, $4);};
	
block: return_block | non_return_block;

lines: declare {$$ = mknode("", $1, NULL, NULL);}
	| declare lines {$$ = mknode("", $1, NULL, $2);}
	| statements{$$ = mknode("", $1, NULL, NULL);};

right_brace: R_BRACE {$$ = mknode(")", NULL, NULL, NULL);};

/*-------BODY-------*/

empty_body: L_BRACE right_brace {$$ = mknode("(BODY", $2, NULL, NULL);};

proc_body: L_BRACE lines right_brace {$$ = mknode("(BODY", $2, NULL, $3);}
	| empty_body;

func_body: L_BRACE return_statement right_brace {$$ = mknode("(BODY", $2, NULL, $3);}
	| L_BRACE lines return_statement right_brace {$$ = mknode("(BODY", $2, $3, $4);};

	
%%

/*_____________________ADDITIONAL C CODE_____________________*/

#include "lex.yy.c"

int main()
{
	return yyparse();
}

node *mknode(char *token, node *left, node *center, node *right)
{
	node *newnode = (node*)malloc (sizeof(node));
	char *newstr = (char*)malloc (sizeof(token) + 1);
	strcpy(newstr, token);
	newnode->left = left;
	newnode->center = center;
	newnode->right = right;
	newnode->token = newstr;
	return newnode;
}

void printtree(node *tree, int tab_num)
{
	for(int i = 0; i < tab_num; i++)
		printf(" ");
	printf("%s\n", tree->token);
	if(tree->left)
		printtree(tree->left, tab_num + 1);
	if(tree->center)
		printtree(tree->center, tab_num + 1);
	if(tree->right)
		printtree(tree->right, tab_num + 1);
}

int yyerror()
{
	printf("MY ERROR\n");
	return 0;
}



