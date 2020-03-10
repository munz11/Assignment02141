// Open the file that defines the type "expr" we shall use as AST
%{
open TypesAST
%}

// Declare the tokens (terminal symbols)
%token <int> NUM
%token <string> XVAR
%token TIMES DIV PLUS MINUS POW LPAR RPAR BITAND BITOR OR AND LSPAR RSPAR NOT EQUAL NEQUAL WUTT WUTF GREATER GREATEREQ LESSER LESSEREQ ARROW ASSIGN SKIP STATE IF FI DO OD EOF
// NOTE: the actual formats of these tokens are defined in the lexer file
//       as regular expressions

// Specify precedence and associativity of operators
// Precedence is given by the order (from low to high)
%left EQUAL NEQUAL LESSER GREATER LESSEREQ GREATEREQ
%left OR BITOR
%left AND BITAND
%right NOT
%right POW
%left PLUS MINUS
%left TIMES DIV
%right LBRACE RBRACE LSPAR RSPAR

// We declare the initial non-terminal symbol
%start start

// We specify the return type of each of then non-terminal symbols
%type <CExp> start
%type <AExp> expressionA
%type <BExp> expressionB
%type <CExp> expressionC
%type <GCExp> expressionGC
// Grammar productions
%%

// The first production in "formal" notation is
// start -> expression
// here written:
start: expressionC EOF             { $1 }

// Note that we need to insert an End-Of-File (EOF)
// The code annotation { $1 } specifies that parsing with this production
// returns whatever parsing the expression returns: $1 is the result of parsing
// the first component of the production (i.e. expression)

// The productions for expressions are like in the grammar we saw in class
// written in the yacc format:
expressionA:
  | expressionA TIMES expressionA   { TimesExpr($1,$3) }
  | expressionA DIV expressionA     { DivExpr($1,$3) }
  | expressionA PLUS expressionA    { PlusExpr($1,$3) }
  | expressionA MINUS expressionA   { MinusExpr($1,$3) }
  | expressionA POW expressionA     { PowExpr($1,$3) }
  | PLUS expressionA               { UPlusExpr($2) }
  | MINUS expressionA              { UMinusExpr($2) }
  | NUM                           { Num($1) }   
  | XVAR                          { Xval($1) }
  | LPAR expressionA RPAR          { $2 }
  | XVAR LSPAR expressionA RSPAR   { ArrayExpr($1,$3) }


expressionB:
  | expressionB BITAND expressionB {BitAndB($1,$3)}
  | expressionB BITOR expressionB  {BitOrB($1,$3)}
  | expressionB AND expressionB  {LogAndB($1,$3)}
  | expressionB OR expressionB  {LogOrB($1,$3)}
  | NOT expressionB  {LogNotB($2)}
  | expressionA EQUAL expressionA  {BEqual($1,$3)}
  | expressionA NEQUAL expressionA {NotEqualB($1,$3)}
  | expressionA GREATER expressionA {GThanB($1,$3)}
  | expressionA GREATEREQ expressionA {GEThanB($1,$3)}
  | expressionA LESSER expressionA {LThanB($1,$3)}
  | expressionA LESSEREQ expressionA {LEThanB($1,$3)}
  | LPAR expressionB RPAR  { $2 }
  | WUTT    {WutT}
  | WUTF    {WutF}

expressionGC:
  | expressionB ARROW expressionC {ARROWGC($1,$3)}
  | expressionGC LSPAR RSPAR expressionGC {StateGC($1,$4)}
    
expressionC:
  | XVAR ASSIGN expressionA    {AssignC($1,$3)}
  | XVAR LSPAR expressionA RSPAR ASSIGN expressionA {AssignArrayC($1,$3,$6)}
  | SKIP  {SkipC}
  | expressionC STATE expressionC  {StateC($1,$3)}
  | IF expressionGC FI  {IfStateC($2)}
  | DO expressionGC OD  {DoloopC($2)}

//expressionGC:
 // | expressionB ARROW expressionC {ARROWGC($1,$3)}
 // | expressionGC LSPAR RSPAR expressionGC {StateGC($1,$4)}
// Again, the code annotation specifies the result of parsing
// For example { TimesExpr($1,$3) } specifies that parsing with the production
// returns the value TimesExpr($1,$3), where $i is the result of parsing
// component i in the production (in this case the lhs and rhs operands)

%%
