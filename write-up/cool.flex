/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

void adjust()
{
curr_lineno = yylineno;
}

static std::string currentString;
static int iCommentNestTimes = 0;
static bool bStringError = false;

%}

%option yylineno

%x COMMENT INLINE_COMMENT STRING


/*
 * Define names for regular expressions here.
 */

%%

[ \n\f\r\t\v] 		{}


 /*
  *  Nested comments
  */
"(*" 			{ adjust(); BEGIN COMMENT; iCommentNestTimes++; }
"*)" 			{ adjust(); yylval.error_msg = "Unmatched *)"; return ERROR; }

<COMMENT>
{
<<EOF>>		{ yylval.error_msg = "EOF in comment"; BEGIN 0; return ERROR; }
"(*" 		{ adjust(); iCommentNestTimes++; }
"*)" 		{ adjust(); iCommentNestTimes--; if(iCommentNestTimes == 0) {BEGIN 0;} }
. 		{ adjust(); }
\n 		{ adjust(); }
}

"--" 		{ adjust(); BEGIN INLINE_COMMENT; }
<INLINE_COMMENT>
{
<<EOF>>		{ adjust(); BEGIN 0; }
\n 		{ adjust(); BEGIN 0; }
. 		{ adjust(); }
}

 /*
  *  The multiple-character operators.
  */
 
"=>"		{ adjust(); return DARROW; } 
"<-"		{ adjust(); return ASSIGN; }
"<=" 		{ adjust(); return LE; }


 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
(?i:class)			{ adjust(); return CLASS; }
(?i:else) 			{ adjust(); return ELSE; }
(?i:fi) 			{ adjust(); return FI; }
(?i:if)				{ adjust(); return IF; }
(?i:in)				{ adjust(); return IN; }
(?i:inherits) 			{ adjust(); return INHERITS; }
(?i:isvoid) 			{ adjust(); return ISVOID; }
(?i:let) 			{ adjust(); return LET; }
(?i:loop) 			{ adjust(); return LOOP; }
(?i:pool) 			{ adjust(); return POOL; }
(?i:then) 			{ adjust(); return THEN; }
(?i:while) 			{ adjust(); return WHILE; }
(?i:case) 			{ adjust(); return CASE; }
(?i:esac) 			{ adjust(); return ESAC; }
(?i:new) 			{ adjust(); return NEW; }
(?i:of) 			{ adjust(); return OF; }
(?i:not) 			{ adjust(); return NOT; }
t[rR][uU][eE] 			{ adjust(); yylval.boolean = 1; return BOOL_CONST; }
f[aA][lL][sS][eE] 		{ adjust(); yylval.boolean = 0; return BOOL_CONST; }


 /* Integers, Identifiers, and Special Notation */
[0-9]+			{ adjust(); yylval.symbol = inttable.add_string(yytext); return INT_CONST; }
[a-z][a-zA-Z0-9_]* 	{ adjust(); yylval.symbol = idtable.add_string(yytext); return OBJECTID; } 
[A-Z][a-zA-Z0-9_]* 	{ adjust(); yylval.symbol = idtable.add_string(yytext); return TYPEID; }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

\" 			{ adjust(); BEGIN STRING; currentString.clear(); bStringError = false;}
<STRING>
{
\" 			{ 
			adjust(); 
			BEGIN 0;
			if(bStringError){
				// ignore
			}
			else{
				if(currentString.size() >= MAX_STR_CONST) {
					yylval.error_msg = "String constant too long";
					return ERROR;
				}
				else {
					yylval.symbol = stringtable.add_string((char*)currentString.c_str()); 
					return STR_CONST;
				}
			}
			}

\\[ \t]*\n 		{ adjust(); currentString += '\n'; }
\n 			{ adjust(); BEGIN 0; if(!bStringError) { yylval.error_msg = "Unterminated string constant"; return ERROR;} }
\0 			{ adjust(); yylval.error_msg = "String contains null character"; bStringError = true; return ERROR; }
<<EOF>>			{ yylval.error_msg = "EOF in string constant"; BEGIN 0; return ERROR; }
\\b 			{ adjust(); currentString += '\b'; }
\\t 			{ adjust(); currentString += '\t'; }
\\n 			{ adjust(); currentString += '\n'; }
\\f 			{ adjust(); currentString += '\f'; }
\\\"			{ adjust(); currentString += '\"'; }
\\\\ 			{ adjust(); currentString += '\\'; }
\\ 			{ adjust(); /* ignore */}
.			{ adjust(); currentString += yytext; }
}

"+" { return int('+'); }

"-" { return int('-'); }

"*" { return int('*'); }

"/" { return int('/'); }

"<" { return int('<'); }

"=" { return int('='); }

"." { return int('.'); }

";" { return int(';'); }

"~" { return int('~'); }

"{" { return int('{'); }

"}" { return int('}'); }

"(" { return int('('); }

")" { return int(')'); }

":" { return int(':'); }

"@" { return int('@'); }

"," { return int(','); }


. 		{ adjust(); yylval.error_msg = yytext; return ERROR; }

%%
