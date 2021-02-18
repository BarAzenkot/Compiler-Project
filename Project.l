
%x COMMENT

%%
"bool"						return BOOLEAN;
"int"						return INTEGER;
"char"						return CHARACTER;
"real"						return REAL;
"string"					return STRING;
"if"						return IF;
"else"						return ELSE;
"while"					return WHILE;
"var"						return VAR;
"func"						return FUNC;
"proc"						return PROC;
"return" 					return RETURN;
"null"						return NUL;
"Main"						return MAIN;


"int*"						return INT_P;
"char*"					return CHAR_P;
"real*"					return REAL_P;


"/%"						BEGIN COMMENT;
<COMMENT>.					;
<COMMENT>\n					lines++;
<COMMENT>"%/"					BEGIN 0;


"&&"						return AND;
"/"						return DIV;
"="						return ASS;
"=="						return EQL;
">"						return BIG;
">="						return BIG_EQ;
"<"						return SML;
"<="						return SML_EQ;
"-"						return SUB;
"!"						return NOT;
"!="						return NOT_EQ;
"||"						return OR;
"+"						return ADD;
"*"						return MUL;
"&"						return AMP;
"^"						return CAR;
";"						return SEMI;
":"						return COL;
","						return COMMA;
"|"						return ABS;
"{"						return L_BRACE;
"}"						return R_BRACE;
"("						return L_PARENTHESE;
")"						return R_PARENTHESE;
"["						return L_BRACKET;
"]"						return R_BRACKET;


true|TRUE|false|FALSE				{yylval = strdup(yytext); return BOOL_VAR;}
[\'].[\']					{yylval = strdup(yytext); return CHAR_VAR;}
[-]?[1-9][0-9]*|0[x|X][1-9A-F][0-9A-F]*|[0]	{yylval = strdup(yytext); return INT_VAR;}
[-]?[0-9]+"."[0-9]+[[eE][-+][1-9][0-9]+]?	{yylval = strdup(yytext); return REAL_VAR;}
[\"][^\"]*[\"]					{yylval = strdup(yytext); return STRING_VAR;}
[a-zA-Z][_a-zA-Z0-9]*				{yylval = strdup(yytext); return IDENTIFIER;}


\n	lines++;
.	;

%%
