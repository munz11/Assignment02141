# Assignment02141
**We are currently still working through some issues concerning array memory and the GC [] GC command. 

The overall goal of the assignment is to build a tool for running and analyzing programs written in a variant of the Guarded Command Language (GCL). 
The language for all the files is F#
The assignment is divided in tasks. Each task is devoted to a module of the tool (a parser, a compiler, an interpreter and several analysers). Each module should be runnable as a standalone program that takes as input a GCL program and produces a result.

The currently the module represents a parser which reads a GCL program as input and returns an abstract syntax tree. At the moment the program is set up such that the GCL program input must start with the expressions supported by C. If the program is incorrect, currently no hints are given to fix it, rather an error message will appear. 

The Guarded command language supported by this parser is:
C ::= x := a | A[a] := a | skip | C ; C | if GC fi | do GC od
GC ::= b -> C | GC [] GC
a ::= n | x | A[a] | a + a | a - a | a * a | a / a | - a | a ^ a | (a)
b ::= true | false | b & b | b | b | b && b | b || b | !b | a = a | a != a | a > a | a >= a | a < a | a <= a | (b)

The variables x are strings matching the regular expression ['a'-'z''A'-'Z']['a'-'z''A'-'Z'\d_]* 
The numbers N match the regular expression \d+
A whitespace matches the regular expression [\u00A0 \n \r \t], with a mandatory whitespace after if, do, and before fi, od. Whitespaces are ignored anywhere else.
Precedence and associativity rules:
  - In arithmetic expressions, precedence is highest for - (unary minus), then ^, then * and /, and lowest for + and - (binary minus).
  - In boolean expressions, precedence is highest for !, then & and &&, and lowest for | and ||.
  - Operators *, /, +, -, &, |, &&, and || are left-associative.
  - Operators ^, [], and ; are right associative.
  
  We are currently still working through some issues concerning array memory and the GC [] GC command. 
