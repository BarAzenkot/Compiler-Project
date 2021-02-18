/*_____________________C DECLARATIONS_____________________*/

%{
#include<stdio.h>
#include<string.h>
#include<stdlib.h>

int lines = 1;
int SCOPES = 0;

typedef struct node
{
	char *token;
	struct node *left;
	struct node *center;
	struct node *right;
} node;

typedef struct symbol
{
	char *name;
	char *type;
	char *data;
	int procOrNot;
	int scope_num;
	struct node *args;
	struct symbol *next; 
} symbol;

typedef struct scope
{

	char* name;
	struct symbol *symbols;
	struct scope *next;
	int scope_num;
	int scope_lvl;
} scope;

symbol *head = NULL;
scope *stack = NULL;
int i = 0;


void LABELS(node* n);
void NEXT(node* n);
void TAC(node* n);
char* append(char* s1, char* s2);
char* gen(int n, ...);
char* freshVar();
char* freshLabel();
int isID(node* n);
void TAC_ID(node* n);
void TAC_ASSIGN(node* n);
void TAC_ARITHMETIC_OP(node* n, char* sign);
void TAC_UNARY(node* n, char* sign);
void TAC_EXP_PAREN(node* n);
void TAC_RETURN(node* n);
void TAC_SKIPS(node* n);
void LABELS_WHILE_ST(node* n);
void NEXT_WHILE_ST(node* n);
void TAC_WHILE_ST(node* n);
void LABELS_IF_ST(node* n);
void NEXT_IF_ST(node* n);
void TAC_IF_ST(node* n);
void LABELS_IF_ELSE_ST(node* n);
void NEXT_IF_ELSE_ST(node* n);
void TAC_IF_ELSE_ST(node* n);
void TAC_LOGIC_OP(node* n, char* sign);
void NEXT_BOOL_PAREN(node* n);
void TAC_BOOL_PAREN(node* n);
void TAC_BOOL_TRUE(node* n);
void TAC_BOOL_FALSE(node* n);
void NEXT_BOOL_NOT(node* n);
void TAC_BOOL_NOT(node* n);
void LABELS_OR(node* n);
void NEXT_OR(node* n);
void TAC_OR(node* n);
void LABELS_AND(node* n);
void NEXT_AND(node* n);
void TAC_AND(node* n);
void LABELS_MASTER(node* n);
void TAC_MASTER(node* n);
void LABELS_ST(node* n);
void NEXT_ST(node* n);
void TAC_ST(node* n);
void TAC_FUNC_PROC_DECLARE(node* n);
void TAC_FUNC_PROC_CALL(node* n);
void pushStatements(node* n, int lvl);
void pushNode(node* n, int level);
char* checkEvaluation(node* n);
void pushScopeStatements(node* n);
int check_Declaration(node* n);
int check_Numeric(char* token);
int check_Const(node* n);
void check_FP_Decleration(char* token, node *callArgs);
int check_Args(node* callArgs, node *declaredArgs);
symbol* symbolLookUp(symbol** headRef, char* token);
symbol* scopeLookUp(char* token);
void pushDeclarations(char* type, node* n);
void pushScope(struct scope** head, node* statements, node* parameters, char* name, int lvl);
void pushParameters(scope **current, node* parameters);
char* getType(struct node* n);
void pushSymbols(struct scope** node, char* id, char* type, char* data, struct node* parameters, int procOrNot);
node *mknode(char *token, node *left, node *center, node *right);
int checkForDoubles(scope* s);
int sameSymbols(char* scopeName, symbol* s);
void start(node *root);
void printScopes(scope* s);
void printSymbols(scope* s);
void printTable();
node *mknode (char *token, node *left, node* center, node *right);
void printtree (node *tree, int tab);
#define YYSTYPE struct node*

%}

/*_____________________YACC DECLARATIONS_____________________*/

%token BOOLEAN INTEGER CHARACTER REAL STRING INT_P CHAR_P REAL_P VAR FUNC PROC IDENTIFIER MAIN
%token IF ELSE WHILE
%token RETURN NUL
%token AND DIV ASS EQL BIG BIG_EQ SML SML_EQ OR ADD MUL AMP CAR SEMI COL COMMA ABS L_BRACE R_BRACE L_PARENTHESE R_PARENTHESE L_BRACKET R_BRACKET
%token BOOL_VAR CHAR_VAR INT_VAR REAL_VAR STRING_VAR


%right ASS SEMI NOT
%left L_PARENTHESE R_PARENTHESE
%left EQL BIG BIG_EQ SML SML_EQ NOT_EQ
%left ADD SUB AND OR
%left MUL DIV

/*_____________________GRAMMER RULES_____________________*/

%%
s: code {
		puts("======== LABELS ========");
		LABELS($1);
		puts("======== NEXT ========");
		NEXT($1);
		puts("======== TAC ========");
		TAC($1);
		puts("======== CODE ========");
		puts($1->code);
		};

code: main {$$ = mknode("CODE", $1, NULL, NULL);};

main: proc_or_func main {$$ = mknode("MAIN-PROGRAM", $1, NULL, $2);}
	| main_proc {$$ = mknode("MAIN-PROGRAM", $1, NULL, NULL);};

/*-------PROCEDURES-------*/

proc_or_func: func 
	| proc ;

proc: PROC id L_PARENTHESE args R_PARENTHESE proc_body {$$ = mknode("PROC", $2, $4, $6);};

func: FUNC id args_and_return func_body {$$ = mknode("FUNC", $2, $3, $4);};

/*return_var_type: RETURN var_type {$$ = mknode("(RETURN", $2, NULL, NULL);}
	| RETURN string_type {$$ = mknode("(RETURN", $2, NULL, NULL);};*/

main_proc: PROC MAIN L_PARENTHESE R_PARENTHESE proc_body {$$ = mknode("MAIN", NULL, NULL, $5);};

args: args_insertion {$$ = mknode("ARGS", $1, NULL, NULL);}
	| /*EMPTY*/ ;

args_call: args_call2 {$$ = mknode("ARGS CALL", $1, NULL, NULL);};

args_call2: exp COMMA args_call2 {$$ = mknode("", $1, $3, NULL);}
	| exp 
	| /*EMPTY*/ {$$ = mknode("EMPTY", NULL, NULL, NULL);};

args_and_return: L_PARENTHESE args right_parenthese RETURN var_type {$$ = mknode("", $2, $3, $5);};

args_insertion: id COMMA args_insertion {$$ = mknode("", $1, NULL, $3);}
	| id COL var_type SEMI args_insertion {$$ = mknode("END TYPE;", $1, $3, $5);}
	| id COL var_type {$$ = mknode("END TYPE", $1, $3, NULL);};

/*-------DECLARATION-------*/

declare: var_declare
	| string_declare
	| proc_or_func;
	

var_declare: VAR var_id COL var_type SEMI {$$ = mknode("DECLARE", $2, $4, NULL);};

string_declare: VAR var_id COL string_type L_BRACKET num R_BRACKET SEMI {$$ = mknode("DECLARE", $2, $4, $6);};

string_type: STRING {$$ = mknode("string", NULL, NULL, NULL);};

var_id: id 
	| id COMMA var_id {$$ = mknode("", $1, $3, NULL);};

var_type: BOOLEAN {$$ = mknode("bool", NULL, NULL, NULL);}
	| INTEGER {$$ = mknode("int", NULL, NULL, NULL);}
	| CHARACTER {$$ = mknode("char", NULL, NULL, NULL);}
	| REAL {$$ = mknode("real", NULL, NULL, NULL);}
	| INT_P {$$ = mknode("int*", NULL, NULL, NULL);}
	| CHAR_P {$$ = mknode("char*", NULL, NULL, NULL);}
	| REAL_P {$$ = mknode("real*", NULL, NULL, NULL);};
	
/*-------STATEMENTS-------*/

statements: statement statements {$$ = mknode("STATEMENT", $1, NULL, $2);}
	| statement {$$ = mknode("STATEMENT", $1, NULL, NULL);};
	
statement: if_statement
	| while_statement
	| assignment_statement
	| proc_or_func
	| func_proc_call SEMI 
	| proc_body ;

if_statement: IF L_PARENTHESE exp R_PARENTHESE block {$$ = mknode("IF", $3, $5, NULL);}
	| IF L_PARENTHESE exp R_PARENTHESE block ELSE block {$$ = mknode("IF-ELSE", $3, $5, $7);}
	| IF L_PARENTHESE exp R_PARENTHESE statement {$$ = mknode("IF", $3, $5, NULL);}
	| IF L_PARENTHESE exp R_PARENTHESE statement ELSE statement {$$ = mknode("IF-ELSE", $3, $5, $7);};

while_statement: WHILE L_PARENTHESE exp R_PARENTHESE non_return_block {$$ = mknode("WHILE", $3, $5, NULL);}
	| WHILE L_PARENTHESE exp R_PARENTHESE statement {$$ = mknode("WHILE", $3, $5, NULL);};

return_statement: RETURN exp SEMI {$$ = mknode("RETURN", $2, NULL, NULL);};

assignment_statement: id ASS exp SEMI {$$ = mknode("=", $1, NULL, $3);}
	| CAR id ASS exp SEMI {$$ = mknode("=", $2, NULL, $4);}
	| id brackets ASS exp SEMI {$$ = mknode("=", $1, $2, $4);};

/*-------EXPRESSIONS-------*/

exp: exp OR exp {$$ = mknode("||", $1, NULL, $3);}
	| exp AND exp {$$ = mknode("&&", $1, NULL, $3);}
	| exp EQL exp {$$ = mknode("==", $1, NULL, $3);}
	| exp SML exp {$$ = mknode("<", $1, NULL, $3);}
	| exp SML_EQ exp {$$ = mknode("<=", $1, NULL, $3);}
	| exp BIG exp {$$ = mknode(">", $1, NULL, $3);}
	| exp BIG_EQ exp {$$ = mknode(">=", $1, NULL, $3);}
	| exp NOT_EQ exp {$$ = mknode("!=", $1, NULL, $3);}
	| NOT exp {$$ = mknode("!", $2, NULL, NULL);}
	| exp ADD exp {$$ = mknode("+", $1, NULL, $3);}
	| exp SUB exp {$$ = mknode("-", $1, NULL, $3);}
	| exp MUL exp {$$ = mknode("*", $1, NULL, $3);}
	| exp DIV exp {$$ = mknode("/", $1, NULL, $3);}
	| consts
	| parentheses;

consts: num | boolean | string | char | id | pointers | func_proc_call | abs ;

id: IDENTIFIER {$$ = mknode(yylval, NULL, NULL, NULL);};

pointers: CAR id {$$ = mknode("^", $2, NULL, NULL);}
	| AMP id {$$ = mknode("&", $2, NULL, NULL);}
	| AMP id brackets {$$ = mknode("&", $2, NULL, $3);};

brackets: L_BRACKET exp right_bracket {$$ = mknode("[", $2, NULL, $3);};

right_bracket: R_BRACKET {$$ = mknode("]", NULL, NULL, NULL);};

parentheses: L_PARENTHESE exp right_parenthese {$$ = mknode("()", $2, NULL, $3);};
	
right_parenthese: R_PARENTHESE {$$ = mknode(")", NULL, NULL, NULL);};
	
num: INT_VAR {$$ = mknode(yylval, mknode("int", NULL, NULL, NULL), NULL, NULL);}
	| REAL_VAR {$$ = mknode(yylval, mknode("real", NULL, NULL, NULL), NULL, NULL);};

boolean: BOOL_VAR {$$ = mknode(yylval, mknode("bool", NULL, NULL, NULL), NULL, NULL);};

string: STRING_VAR {$$ = mknode(yylval, mknode("string", NULL, NULL, NULL), NULL, NULL);};

char: CHAR_VAR {$$ = mknode(yylval, mknode("char", NULL, NULL, NULL), NULL, NULL);};


abs: ABS id ABS {$$ = mknode("ABS", mknode("int", NULL, NULL, NULL), $1, NULL);};

func_proc_call: id L_PARENTHESE args_call R_PARENTHESE {$$ = mknode("CALL", $1, $3, NULL);};

/*-------BLOCKS-------*/

empty_block: L_BRACE right_brace {$$ = mknode("BLOCK", $2, NULL, NULL);};

non_return_block: L_BRACE lines right_brace {$$ = mknode("BLOCK", $2, NULL, $3);}
	| empty_block;

return_block: L_BRACE return_statement right_brace {$$ = mknode("BLOCK", $2, NULL, $3);}
	| L_BRACE lines return_statement right_brace {$$ = mknode("BLOCK", $2, $3, $4);};
	
block: return_block | non_return_block;

lines: declare {$$ = mknode("", $1, NULL, NULL);}
	| declare lines {$$ = mknode("", $1, $2, NULL);}
	| statements 
	| non_return_block;

right_brace: R_BRACE {$$ = mknode(")", NULL, NULL, NULL);};

/*-------BODY-------*/

empty_body: L_BRACE right_brace {$$ = mknode("BODY", NULL, NULL, $2);};

proc_body: L_BRACE lines right_brace {$$ = mknode("BODY", $2, NULL, $3);}
	| empty_body;

func_body: L_BRACE return_statement right_brace {$$ = mknode("BODY", NULL, $2, $3);}
	| L_BRACE lines return_statement right_brace {$$ = mknode("BODY", $2, $3, $4);};

	
%%


/*_____________________ADDITIONAL C CODE_____________________*/

#include "lex.yy.c"

int main()
{
	return yyparse();
}


void pushStatements(node* n, int lvl)
{

	if (n == NULL)
		return;
	

	if (!strcmp(n->token, "CODE")){
		lvl++;
		pushScope(&stack, n->left, NULL, "CODE", lvl);
	}
		
	if (!strcmp(n->token, "IF")){
		lvl++;
		pushScope(&stack, n->center, NULL, "IF", lvl);
	}
	
	if (!strcmp(n->token, "IF-ELSE")){
		lvl++;
		pushScope(&stack, n->center, NULL, "IF", lvl);
		pushScope(&stack, n->right, NULL, "ELSE", lvl);
	}
	
	if (!strcmp(n->token, "WHILE")){
		lvl++;
		pushScope(&stack, n->center, NULL, "WHILE", lvl);
	}

	if (!strcmp(n->token, "PROC")){
		lvl++;
		pushScope(&stack, n->right, n->center->left, "PROC", lvl);
	}	
	
	if (!strcmp(n->token, "FUNC")){
		lvl++;
		pushScope(&stack, n->right, n->center->left->left, "FUNC", lvl);
		//TODO: check if the type that the function returns equals to the type that declared 		in the signature. 
	}

	if (!strcmp(n->token, "MAIN")){
		lvl++;
		pushScope(&stack, n->left->left, NULL, "MAIN", lvl);
	}
	
	pushStatements(n->left, lvl);
	pushStatements(n->center, lvl);
	pushStatements(n->right, lvl);
}


void pushNode(node* n, int level)
{
	if(n == NULL)
		return;
}


char* checkEvaluation(node* n)
{
	if (check_Const(n)){
		return n->left->token;
	}
	else if (!n->left && !n->center && !n->right){
		char* type;
		symbol* s;
		s = scopeLookUp(n->token);
		if (s){
			type = s->type;
			return type;
		}
		else return "null";
	}
	else if (!strcmp(n->token, "CALL")){
		char* type;
		symbol* s;
		s = scopeLookUp(n->left->token);
		if (s){
			type = s->type; 
			return type;
		}
		else return "null";
	}
	else if (!strcmp(n->token, "+") || !strcmp(n->token, "-") || !strcmp(n->token, "*") || !strcmp(n->token, "/")){
		char *right, *left;
		left = checkEvaluation(n->left);
		right = checkEvaluation(n->right);
		if (!strcmp(left, "int") && !strcmp(right, "int"))
			return "int";
		if (!strcmp(left, "real") && !strcmp(right, "real"))
			return "real";
		if (!strcmp(left, "int") && !strcmp(right, "real"))
			return "real";
		if (!strcmp(left, "real") && !strcmp(right, "int"))
			return "real";
		else {
			if (strcmp(left, "ExpressionError") || strcmp(right, "ExpressionError"))
				printf("ExpressionError: Operator '%s' Can't match '%s' and '%s'.\n", n->token,left, right);
			return "ExpressionError"; 
		}
	}
	else if(!strcmp(n->token, "&&") || !strcmp(n->token, "||")){
		char *right, *left;
		left = checkEvaluation(n->left);
		right = checkEvaluation(n->right);
		if (!strcmp(left, "bool") && !strcmp(right, "bool"))
			return "bool";
		else {
			if (strcmp(left, "ExpressionError") || strcmp(right, "ExpressionError"))
				printf("ExpressionError: Operator '%s' Can't match '%s' and '%s'.\n", n->token,left, right);
			return "ExpressionError"; 
		}
	}
	else if (!strcmp(n->token, "<") || !strcmp(n->token, ">") || !strcmp(n->token, "<=") || !strcmp(n->token, ">=")){
		char *right, *left;
		left = checkEvaluation(n->left);
		right = checkEvaluation(n->right);
		if (!strcmp(left, "int") && !strcmp(right, "int"))
			return "bool";
		if (!strcmp(left, "real") && !strcmp(right, "real"))
			return "bool";
		if (!strcmp(left, "int") && !strcmp(right, "real"))
			return "bool";
		if (!strcmp(left, "real") && !strcmp(right, "int"))
			return "bool";
		else {
			if (strcmp(left, "ExpressionError") || strcmp(right, "ExpressionError")){
				printf("ExpressionError: Operator '%s' Can't match '%s' and '%s'.\n", n->token,left, right);
			}
			return "ExpressionError"; 
		}
	}
	else if(!strcmp(n->token, "==") || !strcmp(n->token, "!=")){
		char *right, *left;
		left = checkEvaluation(n->left);
		right = checkEvaluation(n->right);
		if (!strcmp(left, "int") && !strcmp(right, "int"))
			return "bool";
		if (!strcmp(left, "real") && !strcmp(right, "real"))
			return "bool";
		if (!strcmp(left, "char") && !strcmp(right, "char"))
			return "bool";
		if (!strcmp(left, "bool") && !strcmp(right, "bool"))
			return "bool";
		if (!strcmp(left, "int*") && !strcmp(right, "int*"))
			return "bool";
		if (!strcmp(left, "real*") && !strcmp(right, "real*"))
			return "bool";
		if (!strcmp(left, "char*") && !strcmp(right, "char*"))
			return "bool";
		else {
			if (strcmp(left, "ExpressionError") || strcmp(right, "ExpressionError"))
				printf("ExpressionError: Operator '%s' Can't match '%s' and '%s'.\n", n->token,left, right);
			return "ExpressionError"; 
		}
	}
	else if (!strcmp(n->token, "!")){
		char* left;
		left = checkEvaluation(n->left);
		if(!strcmp(left, "bool"))
			return "bool";
		else {
			if (strcmp(left, "ExpressionError"))
				printf("ExpressionError: Operator '%s' Can't operate on '%s'\n",n->token, left);
			return "ExpressionError"; 
		}
	}/*
	else if (!strcmp(n->token, "ABS")){
		char* center;
		center = checkEvaluation(n->center);
		if(!strcmp(center, "string"))
			return "int";
		else {
			if (strcmp(left, "ExpressionError"))
				printf("ExpressionError: Operator '%s' Can't operate on '%s'\n",n->token, left);
			return "ExpressionError"; 
		}
	}*/
	else if (!strcmp(n->token, "()")){
		char* left;
		left = checkEvaluation(n->left);
		if(!strcmp(left, "bool"))
			return "bool";
		if(!strcmp(left, "int"))
			return "int";
		if(!strcmp(left, "real"))
			return "real";
		if(!strcmp(left, "char"))
			return "char";
		else {
			if (strcmp(left, "ExpressionError"))
				printf("ExpressionError: Operator '%s' Can't operate on '%s'\n",n->token, left);
			return "ExpressionError"; 
		}
	}
	else if (!strcmp(n->token, "=")){
		char *right, *left;
		left = checkEvaluation(n->left);
		right = checkEvaluation(n->right);
		if (!strcmp(left, "int") && !strcmp(right, "int"))
			return "int";
		if (!strcmp(left, "real") && !strcmp(right, "real"))
			return "real";
		if (!strcmp(left, "char") && !strcmp(right, "char"))
			return "char";
		if (!strcmp(left, "bool") && !strcmp(right, "bool"))
			return "bool";
		if (!strcmp(left, "int*") && !strcmp(right, "int*"))
			return "int*";
		if (!strcmp(left, "real*") && !strcmp(right, "real*"))
			return "real*";
		if (!strcmp(left, "char*") && !strcmp(right, "char*"))
			return "char*";
		if (!strcmp(left, "string") && !strcmp(right, "string"))
			return "string";
		else {
			if (strcmp(left, "ExpressionError") || strcmp(right, "ExpressionError"))
				printf("ExpressionError: Operator '%s' Can't match '%s' and '%s'.\n", n->token,left, right);
			return "ExpressionError"; 
		}
	}
}


void pushScopeStatements(node* n)
{

   	if(n == NULL)
		return;
    	

	//for any of the following statements, we do not want to explore them recursively
	//so we return upon seeing them
	
	if(!strcmp(n->token,"IF")){
		return;
	}
    
	if(!strcmp(n->token,"IF-ELSE")){
		return;
	}
    

	if(!strcmp(n->token,"WHILE")){
		return;
	}

	if(!strcmp(n->token,"PROC")){
		pushSymbols(&stack, n->left->token, "void", "PROC", n->center->left, 1);
		return;
	}
	
	if(!strcmp(n->token,"FUNC")){
		pushSymbols(&stack, n->left->token, n->center->right->token, "FUNC", n->center->left->left, 1);
		return;
	}

	if(!strcmp(n->token,"DECLARE")){
		pushDeclarations(n->center->token,n->left);
		return;
	}
    
	if (!strcmp(n->token, "CALL")){  
		check_FP_Decleration(n->left->token, n->center->left);
	}
    
	if (!strcmp(n->token, "STATEMENT")){
		int check;
		if (!strcmp(n->left->token, "IF") || !strcmp(n->left->token, "IF-ELSE")){
			check = check_Declaration(n->left->left);
			if (check)
				checkEvaluation(n->left->left);
		}
		else if (!strcmp(n->left->token, "=")){
			check = check_Declaration(n->left);

			if (check){
				char* left = scopeLookUp(n->left->left->token)->type;
				char* right = checkEvaluation(n->left->right);
				if (strcmp(right,left))
					printf("AssignmentError: Can't assign '%s' into '%s' variable.\n", left, right, n->left->left->token);

			}
		}
		else if(!strcmp(n->left->token, "WHILE")){
			check = check_Declaration(n->left->left);
			if (check){
				checkEvaluation(n->left->left);
			}
		}
	}


		pushScopeStatements(n->left);
		pushScopeStatements(n->center);
	 	pushScopeStatements(n->right);
}


int check_Declaration(node* n)
{
	if (!strcmp(n->token, ")"))
		return 1;
	//check if another if needed (equals to "procedure")
	if (!strcmp(n->token,"EMPTY"))
		return 0;
	if (!n->left && !n->center && !n->right){
		symbol* s = scopeLookUp(n->token);
		if(!s){
			printf("UndefinedError: Variable '%s' is not recognized.\n", n->token);
			return 0;
		}
		else return 1;
	}
	
	if (!strcmp(n->token, "ABS")){
		symbol* s = scopeLookUp(n->center->token);
		if(!s){
			printf("UndefinedError: Variable '%s' is not recognized.\n", n->center->token);
			return 0;
		}
		else return 1;
	}
	
	
	if (check_Const(n))
		return 1;
	
	int result = 1;
	if(n->left)
		if (strcmp(n->left->token, "STATEMENT"))
			result = check_Declaration(n->left) && result;
	if(n->center)
		if (strcmp(n->center->token, "STATEMENT"))
			result = check_Declaration(n->center) && result;
	if(n->right)
		if (strcmp(n->right->token, "STATEMENT"))
			result = check_Declaration(n->right) && result;
	return result;
		
}


int check_Numeric(char* token)
{
	int i;
	if (strlen(token) > 2){
		if (token[0] == '0' && (token[1] == 'x' || token[1] == 'X')){
			for(i = 2; i < strlen(token); i++){
				if(!isxdigit(token[i]))
					return 0;
			}
			return 1;
		}
	}
	
	for(i = 0; i < strlen(token); i++){
		if(!isdigit(token[i]))
			return 0;
	}
	return 1;
}


int check_Const(node* n)
{
	if (!n->left)
		return 0;
	else if (!strcmp(n->left->token, "int"))
		return 1;
	else if (!strcmp(n->left->token, "real"))
		return 1;
	else if (!strcmp(n->left->token, "bool"))
		return 1;
	else if (!strcmp(n->left->token, "string"))
		return 1;
	else if (!strcmp(n->left->token, "char"))
		return 1;
	else if (check_Numeric(n->token))
		return 1;
	return 0;
}


void check_FP_Decleration(char* token, node *callArgs)
{

	scope *currentSc = stack;
	symbol *currentSy;
	
	while(currentSc){
		currentSy = currentSc->symbols;
		if (currentSy){
			symbol *FP_symbol = symbolLookUp(&currentSy, token);
			if (FP_symbol){
				if (check_Args(callArgs, FP_symbol->args))
					return 1;
			}
			 
		}
		currentSc = currentSc->next;
	}
	printf("UndefinedError: Procedure '%s' doesn't match the types of parameters it gets.\n", token);
	return 0;
}


int check_Args(node* callArgs, node *declaredArgs)
{

	if (!strcmp(callArgs->token, "EMPTY") && !declaredArgs->left){ //both empty
		return 1;
	}
	if ((strcmp(callArgs->token, "EMPTY") && !declaredArgs->left) || (!strcmp(callArgs->token, "EMPTY") && declaredArgs->left)) //one of them empty
		return 0;
		
	if (!strcmp(callArgs->token, "ARGS CALL")) //if call isn't empty, go left to his args tree
		callArgs = callArgs->left;
	if (!strcmp(declaredArgs->token, "ARGS")) //if declare isn't empty, go left to his args tree
		declaredArgs = declaredArgs->left;
		
	if (strcmp(callArgs->token, "") && !strcmp(declaredArgs->token, "END TYPE")){
		symbol *args = scopeLookUp(callArgs->token);
		if (args){
			if(!strcmp(args->type, getType(declaredArgs))){
				return 1;
			}
			else return 0;
		}
		else if (!args){
			if (!callArgs->left && !callArgs->center)
				return 0;
			if (!strcmp(checkEvaluation(callArgs), declaredArgs->center->token))
				return 1;
			else return 0;
		}

		
	}
	else if ((strcmp(callArgs->token, "") && !strcmp(declaredArgs->token, ""))
		|| (strcmp(callArgs->token, "") && !strcmp(declaredArgs->token, "END TYPE;"))
		|| (!strcmp(callArgs->token, "") && !strcmp(declaredArgs->token, "END TYPE")))
			return 0;
	
	else return check_Args(callArgs->left, mknode("END TYPE", declaredArgs->left, mknode(getType(declaredArgs), NULL, NULL, NULL) , NULL))
		&& check_Args(callArgs->center, declaredArgs->right);
}


symbol* symbolLookUp(struct symbol** headRef, char* token) 
{
	struct symbol *temp = *headRef;
	
	while(temp){
		if(!strcmp(temp->name, token))
			return temp;
		temp = temp->next;
	}
	return NULL;
}


symbol* scopeLookUp(char* token) 
{
	scope* current = stack;
	symbol* result;
	int lvl;
	while (current){
		lvl = current->scope_lvl;
		result = symbolLookUp(&current->symbols, token);
		if (result)
			return result;
		if (lvl == 1)
			return NULL;
		while (current->scope_lvl > 1 && current->scope_lvl >= lvl)
			current = current->next;
	}
	return NULL;
}


void pushDeclarations(char* type, node* n) 
{
	if(strcmp(n->token, "")){
		pushSymbols(&stack, n->token, type, NULL, NULL, 0);
		return;
	}
	pushDeclarations(type, n->left);
	pushDeclarations(type, n->center);
}


void pushScope(struct scope** head, node* statements, node* parameters, char* name, int lvl) // V
{
	if(strcmp(name, "DECLARE")) {
		struct scope* s = (struct scope*) malloc(sizeof(struct scope));
		s->name = (char*) malloc(sizeof(name) + 1);
		strcpy(s->name, name);
		SCOPES++;
		s->scope_num = SCOPES;
		s->scope_lvl = lvl - 1;
		
		if(parameters){
			pushParameters(&s, parameters);
		}	
		s->next = *head;
		*head = s;
	}
	pushScopeStatements(statements);
	
}


void pushParameters(scope **current, node* parameters) 
{

	if(!parameters){
		return;
	}
	
	if(!strcmp(parameters->token, "END TYPE") || !strcmp(parameters->token, "END TYPE;")){
		pushSymbols(current, parameters->left->token, parameters->center->token, NULL, NULL, 0);
		
	}
	
	else if(!strcmp(parameters->token, "")){

		pushSymbols(current, parameters->left->token, getType(parameters->right), NULL, NULL, 0);
	}
	
	pushParameters(current, parameters->left);
	pushParameters(current, parameters->right);
	
}


char* getType(struct node* n)
{ 
	if(!strcmp(n->token, "END TYPE") || !strcmp(n->token, "END TYPE;"))
		return n->center->token;
	return getType(n->right);
}


void pushSymbols(struct scope** node, char* id, char* type, char* data, struct node* parameters, int procOrNot) 
{

	struct symbol* s = (struct symbol*) malloc(sizeof(struct symbol));
	s->procOrNot = procOrNot;
	s->name = (char*) (malloc(sizeof(id) + 1));
	strcpy(s->name, id);

	if (data){
		s->data = (char*)(malloc(sizeof(s) + 1));
		strcpy(s->data, data);
	}
	s->type = (char*)(malloc(sizeof(type) + 1));
	strcpy(s->type, type);

	s->scope_num = (*node)->scope_num;

	if (parameters){
		s->args = (struct node*) malloc(sizeof(struct node));
		memcpy(s->args, parameters, sizeof(struct node));
	}

	else s->args = NULL;
	
	s->next = (*node)->symbols;
	(*node)->symbols = s;
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



int checkForDoubles(scope* s)
{
	scope* current = s;
	while (current){
		if (!sameSymbols(current->name, current->symbols))
			return 0;
		current = current->next;
	}
	return 1;
}


int sameSymbols(char* scopeName, symbol* s)
{
	symbol* s1 = s;
	symbol* s2;
	
	while (s1){
		s2 = s1;
		while (s2){
			if (s1 != s2){
				if (!strcmp(s1->name, s2->name)){
					if (s1->procOrNot)
						printf("Error: Procedure '%s' is already declared in scope [%s]\n", s1->name, scopeName);
					else 	printf("Error: Variable '%s' is already declared in scope [%s]\n", s1->name, scopeName);
					return 0;
				}
			}
			s2 = s2->next;
		}
		s1 = s1->next;
	}
	return 1;
}


/*----------TOOLS----------*/


char* append(char* s1, char* s2)
{
	char* str;
	
	if ((str = (char*) malloc(strlen(s1) + strlen(s2) + 1)) !=NULL) {
		str[0] = '\0';
		strcat(str, s1);
		strcat(str, s2);
	}
	else printf("malloc failed!\n");
	
	return str;
}


char* gen(int n, ...)
{
	va_list vl;
	int i;
	char* str = "";
	va_start(vl, n);
	
	for(i = 0; i < n; i++){
		char* s = va_arg(vl, char*);
		if (s)
			str = append(str, s); //maybe needs a realloc
	}
	va_end(vl);
	
	return str;
	
}


char* freshVar()
{
	char var_num[10];
	char* new_var;
	sprintf(var_num, "%d", vars++);
	new_var = gen(2, "_t", var_num);
	
	return new_var;
	
}


char* freshLabel()
{
	char label_num[10];
	char* new_label;
	sprintf(label_num, "%d", labels++);
	new_label = gen(2, "L", label_num);
	
	return new_label;
}


int isID(node* n)
{
	if (!n->left && !n->center && !n->right){
		if (!strcmp(n->token, "EMPTY"))
			return 0;
		if (!strcmp(n->token, "string"))
			return 0;
		if (!strcmp(n->token, "bool"))
			return 0;
		if (!strcmp(n->token, "int"))
			return 0;
		if (!strcmp(n->token, "char"))
			return 0;
		if (!strcmp(n->token, "real"))
			return 0;
		if (!strcmp(n->token, "int*"))
			return 0;
		if (!strcmp(n->token, "char*"))
			return 0;
		if (!strcmp(n->token, "real*"))
			return 0;
		if (!strcmp(n->token, "]"))
			return 0;
		if (!strcmp(n->token, ")"))
			return 0;
		return 1;
	}
	return 0;
}


/*----------GENERAL----------*/


void TAC_ID(node* n)
{
	n->var = n->token;
	n->code = "";
}


void TAC_ASSIGN(node* n)
{
	n->var = freshVar();
	n->code = gen(3, n->right->code, gen(5,"\t",  n->var, " = ", n->right->var, "\n"), gen(5, "\t", n->left->var, " = ", n->var, "\n"));
}


void TAC_ARITHMETIC_OP(node* n, char* sign) // sign = '+'/'-'/'*'/'/'
{
	n->var = freshVar();
	n->code = gen(4, n->left->code, n->right->code, gen(8, "\t", n->var, " = ", n->left->var, " ", sign, " ", n->right->var), "\n");
}


void TAC_UNARY(node* n, char* sign)
{
	n->var = freshVar();
	n->code = gen(3, n->left->code, gen(4, n->var, " = ", sign, n->left->var), "\n");
	
}


void TAC_EXP_PAREN(node* n)
{
	n->var = n->left->var;
	n->code = n->left->code;
}


void TAC_RETURN(node* n)
{
	n->code = gen(4, n->left->code, "\tReturn ", n->left->var, "\n");
}


void TAC_SKIPS(node* n)
{
	char* leftBuffer = NULL;
	char* centerBuffer = NULL;
	char* rightBuffer = NULL;
	
	if (n->left)
		leftBuffer = n->left->code;
	if (n->center)
		centerBuffer = n->center->code;
	if (n->right)
		rightBuffer = n->right->code;
	
	n->code = gen(3, leftBuffer, centerBuffer, rightBuffer);
}


/*----------WHILE STATEMENT----------*/


void LABELS_WHILE_ST(node* n)
{
	n->begin = freshLabel();
	n->left->trueLabel = freshLabel();
	n->left->falseLabel = freshLabel();
}


void NEXT_WHILE_ST(node* n)
{
	n->after = n->left->falseLabel;
	n->center->after = n->begin;
}


void TAC_WHILE_ST(node* n)
{
	n->code = gen(12, n->begin, ": ", n->left->code, gen(3, "\tgoto ", n->left->falseLabel, "\n"), n->left->trueLabel, ": ", n->center->code, "\tgoto ", n->begin, "\n", n->left->falseLabel, ": ");
}


/*----------IF STATEMENTS----------*/


void LABELS_IF_ST(node* n)
{
	n->left->trueLabel = freshLabel();
	n->after = freshLabel();
}


void NEXT_IF_ST(node* n)
{
	n->left->falseLabel = n->after;
	n->center->after = n->after;
}


void TAC_IF_ST(node* n)
{
	n->code = gen(7, n->left->code, gen(3, "\tgoto ", n->after, "\n"), n->left->trueLabel, ": ", n->center->code, n->after, ":\n");
}


void LABELS_IF_ELSE_ST(node* n)
{
	n->left->trueLabel = freshLabel();
	//n->left->falseLabel = freshLabel();
	n->after = freshLabel();
}


void NEXT_IF_ELSE_ST(node* n)
{
	n->center->after = n->after;
	n->right->after = n->after;
}


void TAC_IF_ELSE_ST(node* n)
{
	n->code = gen(8, n->left->code, n->right->code, gen(3, "\tgoto ", n->after, "\n"), n->left->trueLabel, ": ", n->center->code, n->after, ": ");
}


/*----------BOOL----------*/


void TAC_LOGIC_OP(node* n, char* sign)
{
	n->var = freshVar();
	n->code = gen(11, n->left->code, n->right->code, "\tif ", n->left->var, " ", sign, " ", n->right->var, " goto ", n->trueLabel, "\n");
}


void NEXT_BOOL_PAREN(node* n)
{
	n->left->trueLabel = n->trueLabel;
	n->left->falseLabel = n->falseLabel;
}


void TAC_BOOL_PAREN(node* n)
{
	n->code = n->left->code;
}


void TAC_BOOL_TRUE(node* n)
{
	n->code = gen(3, "\tgoto ", n->trueLabel, "\n");
}


void TAC_BOOL_FALSE(node* n)
{
	n->code = gen(3, "\tgoto ", n->falseLabel, "\n");
}


void NEXT_BOOL_NOT(node* n)
{
	n->left->trueLabel = n->falseLabel;
	n->left->falseLabel = n->trueLabel;
}


void TAC_BOOL_NOT(node* n)
{
	n->code = n->left->code;
}


/*----------OR----------*/


void LABELS_OR(node* n)
{
	n->left->falseLabel = freshLabel();
}


void NEXT_OR(node* n)
{
	n->left->trueLabel = n->trueLabel;
	n->right->trueLabel = n->trueLabel;
	n->right->falseLabel = n->falseLabel;
}


void TAC_OR(node* n)
{
	n->code = gen(5, n->left->code, n->left->falseLabel, ": ", n->right->code, "\n");
}


/*----------AND----------*/


void LABELS_AND(node* n)
{
	n->left->trueLabel = freshLabel();
}


void NEXT_AND(node* n)
{
	n->left->falseLabel = n->falseLabel;
	n->right->trueLabel = n->trueLabel;
	n->right->falseLabel = n->falseLabel;
}


void TAC_AND(node* n)
{
	n->code = gen(5, n->left->code, n->left->trueLabel, ": ", n->right->code, "\n");
}


/*----------STATEMENTS----------*/


void LABELS_MASTER(node* n)
{
	n->left->after = freshLabel();
}


void TAC_MASTER(node* n)
{
	n->code = gen(4, n->left->code, n->left->after, ": ", "\n");
}


void LABELS_ST(node* n)
{
	n->left->after = freshLabel();
}


void NEXT_ST(node* n)
{
	if (n->right)
		n->right->after = n->after;
}


void TAC_ST(node* n)
{
	char* buffer = NULL;
	if (n->right)
		buffer = n->right->code;
	else if (n->after)
		buffer = gen(3, "goto ", n->after, "\n");
	n->code = gen(2, n->left->code, buffer);
}


/*----------PROCS, FUNCS AND MAIN----------*/


void TAC_FUNC_PROC_DECLARE(node* n)
{
	vars = 0;
	n->code = gen(4, n->left->token, ":\n\tBeginFunc\n", n->right->code, "\tEndFunc\n\n");
}


void TAC_FUNC_PROC_CALL(node* n)
{
	node* args = n->center->left;
	char* buffer = NULL;
	char* sec_buffer = NULL; 
	char num[10];
	int count = 1;
	if(args){
		char* argsCode = NULL;
		buffer = freshVar();
		argsCode = gen(4, buffer, " = ", args->token, "\n");
		if(!strcmp(args->left->token, "int"))
			count *= 4;	
		if(!strcmp(args->left->token, "real"))
			count *= 8;
		if(!strcmp(args->left->token, "bool"))
			count *= 1;
		if(!strcmp(args->left->token, "char"))
			count *= 1;
		sec_buffer = gen(5, "\t", argsCode, "\tPushParam ", buffer, "\n");

		sprintf(num, "%d", count);
	}

	n->var = freshVar();
	n->code = gen(6, sec_buffer, "\t", n->var, " = LCall ", n->left->token, "\n");
	if (buffer)
		n->code = gen(4, n->code, "\tPopParams ", num, "\n");
}

/*
void TAC_FUNC_PROC_CALL(node* n)
{
	puts(n->center->left->token);
	n->var = freshVar();
	n->code = gen(5, "\t", n->var, " = LCall ", n->left->token, "\n");
}
*/
void TAC_MAIN(node* n)
{
	n->code = gen(3, "main:\n\tBeginFunc\n", n->right->code, "\tEndFunc\n\n");
}


/*==========ACTIVATION FUNCS==========*/


//creates the labels and place them.
void LABELS(node* n)
{
	if (n == NULL)
		return;
		
	LABELS(n->left);
	LABELS(n->center);
	LABELS(n->right);
	
	/*
	if (!strcmp(n->token, "CODE"))
		LABELS_MASTER(n);
		
	if (!strcmp(n->token, "MAIN-PROGRAM"))
		LABELS_ST(n);
	
	if (!strcmp(n->token, "STATEMENT"))
		LABELS_ST(n);
	*/
	if (!strcmp(n->token, "IF"))
		LABELS_IF_ST(n);
		
	if (!strcmp(n->token, "IF-ELSE"))
		LABELS_IF_ELSE_ST(n);
		
	if (!strcmp(n->token, "WHILE"))
		LABELS_WHILE_ST(n);
		
	if (!strcmp(n->token, "||"))
		LABELS_OR(n);
	
	if (!strcmp(n->token, "&&"))
		LABELS_AND(n);
	
	else return;
}


//creates an order between the nodes by connecting their "forward" fields.
void NEXT(node* n)
{
	if (n == NULL)
		return;
				
	if (!strcmp(n->token, "MAIN-PROGRAM"))
		NEXT_ST(n);
		
	if (!strcmp(n->token, "STATEMENT"))
		NEXT_ST(n);
	
	if (!strcmp(n->token, "IF"))
		NEXT_IF_ST(n);
		
	if (!strcmp(n->token, "IF-ELSE"))
		NEXT_IF_ELSE_ST(n);
		
	if (!strcmp(n->token, "WHILE"))
		NEXT_WHILE_ST(n);
		
	if (!strcmp(n->token, "||"))
		NEXT_OR(n);
	
	if (!strcmp(n->token, "&&"))
		NEXT_AND(n);
		
	if(!strcmp(n->token, "()"))
		NEXT_BOOL_PAREN(n);
		
	if(!strcmp(n->token, "!"))
		NEXT_BOOL_NOT(n);
	
	NEXT(n->left);
	NEXT(n->center);
	NEXT(n->right);
	
}


//creates and insert a three-address code into one string.
void TAC(node* n)
{
	if (!n)
		return;
	

	if(n->left)
		TAC(n->left);

	if(n->center)
		TAC(n->center);
	
	if(n->right)
		TAC(n->right);

	//printf("current node: %s\n", n->token);
	//printf("current CODE: %s\n", n->code);
	
	if (!strcmp(n->token, "PROC"))
		TAC_FUNC_PROC_DECLARE(n);
	
	else if (!strcmp(n->token, "FUNC"))
		TAC_FUNC_PROC_DECLARE(n);
	
	else if (!strcmp(n->token, "MAIN"))
		TAC_MAIN(n);

	else if (!strcmp(n->token, "STATEMENT"))
		TAC_ST(n);
	
	else if (!strcmp(n->token, "IF"))
		TAC_IF_ST(n);
		
	else if (!strcmp(n->token, "IF-ELSE"))
		TAC_IF_ELSE_ST(n);
		
	else if (!strcmp(n->token, "WHILE"))
		TAC_WHILE_ST(n);
		
	else if (!strcmp(n->token, "||"))
		TAC_OR(n);
	
	else if (!strcmp(n->token, "&&"))
		TAC_AND(n);
	
	else if (!strcmp(n->token, "RETURN"))
		TAC_RETURN(n);
	
	else if (!strcmp(n->token, "="))
		TAC_ASSIGN(n);
	
	else if (!strcmp(n->token, "CALL"))
		TAC_FUNC_PROC_CALL(n);
		
	else if (isID(n))
		TAC_ID(n);
	
	else if (!strcmp(n->token, "+"))
		TAC_ARITHMETIC_OP(n, n->token);
	
	else if (!strcmp(n->token, "-"))
		TAC_ARITHMETIC_OP(n, n->token);
		
	else if (!strcmp(n->token, "*"))
		TAC_ARITHMETIC_OP(n, n->token);
		
	else if (!strcmp(n->token, "/"))
		TAC_ARITHMETIC_OP(n, n->token);
	
	else if (!strcmp(n->token, "()"))
		TAC_BOOL_PAREN(n);
		
	else if (!strcmp(n->token, "!"))
		TAC_BOOL_NOT(n);
		
	else if (!strcmp(n->token, "!="))
		TAC_LOGIC_OP(n, n->token);
	
	else if (!strcmp(n->token, "=="))
		TAC_LOGIC_OP(n, n->token);

	else if (!strcmp(n->token, "<="))
		TAC_LOGIC_OP(n, n->token);
		
	else if (!strcmp(n->token, ">="))
		TAC_LOGIC_OP(n, n->token);
		
	else if (!strcmp(n->token, "<"))
		TAC_LOGIC_OP(n, n->token);
		
	else if (!strcmp(n->token, ">"))
		TAC_LOGIC_OP(n, n->token);
		
	else if (!strcmp(n->token, "&"))
		TAC_UNARY(n, n->token);
		
	else if (!strcmp(n->token, "^"))
		TAC_UNARY(n, n->token);
	
	else {
		n->var = n->token;	
		TAC_SKIPS(n);
	
	}
}


//*************************************************************************


void start(node *root)
{
	pushStatements(root, 1);
	//printtree(root, 1);
	if (!checkForDoubles(stack))
		printf("Build failed. Please search for compile errors.\n");
	printTable();
}


void printScopes(scope* s)
{
	scope* current = s;
	while (current){
		printf("\tScope name: [%s]   Scope level: [%d]   Scope Number: [%d]\n", current->name, current->scope_lvl, current->scope_num);
		current = current->next;
	}
	printf("\nTotal number of scopes: [%d]\n", SCOPES);
}


void printSymbols(scope* s)
{
	scope *cScope = s;
	symbol *cSymbol;
	while (cScope){
		cSymbol = cScope->symbols;
		printf("\tScope [%s] symbols:\n", cScope->name);
		while (cSymbol){
			printf("\t\tSymbol id: [%s]   Type: [%s]\n", cSymbol->name, cSymbol->type);
			cSymbol = cSymbol->next;
		}
		printf("\n");
		cScope = cScope->next;
	}
}

void printTable()
{
	printf("\n----------\nScopes:\n----------\n");
	printScopes(stack);
	printf("\n----------\nSymbols:\n----------\n");
	printSymbols(stack);
}


void printtree(node *tree, int tab_num)
{
	for(int i = 0; i < tab_num; i++)
		printf("  ");
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
	printf("Syntax error in line %d\n", lines);
	return 0;
}



