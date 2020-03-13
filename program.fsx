// This script implements our interactive calculator
// We need to import a couple of modules, including the generated lexer and parser
#r "../../FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"
open Microsoft.FSharp.Text.Lexing
open System
open System.IO
#load "TypesAST.fs"
open TypesAST
#load "Parser.fs"
open Parser
#load "Lexer.fs"
open Lexer
// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
let rec EvalA e = 
    match e with
    | Num(e1) -> string e1
    | Xval(e1) -> e1 
    | UPlusExpr(e1) -> EvalA(e1)
    | PlusExpr(e1, e2) -> EvalA e1 + "+" + EvalA e2
    | MinusExpr(e1, e2) -> EvalA e1 + "-" + EvalA e2
    | PowExpr(e1, e2) -> EvalA e1 + "^" + EvalA e2
    | TimesExpr(e1, e2) -> EvalA e1+ "*" + EvalA e2
    | UMinusExpr(e1) -> "-" + EvalA e1
    | ArrayExpr(e1, e2) -> e1 + "[" + EvalA e2 + "]"
    | DivExpr(e1,e2) -> EvalA e1 + "/" + EvalA e2

let rec evalB e =
    match e with
    | BitAndB(e1, e2) -> evalB e1 + "&" + evalB e2
    | BitOrB(e1, e2) -> evalB e1 + "|" + evalB e2
    | LogAndB(e1, e2) -> evalB e1 + "&&" + evalB e2
    | LogOrB(e1, e2) -> evalB e1 + "||" + evalB e2
    | LogNotB(e1) -> "!" + evalB e1
    | BEqual(e1, e2) -> EvalA e1 + "=" + EvalA e2
    | NotEqualB(e1, e2) -> EvalA e1 + "!=" + EvalA e2
    | GThanB(e1, e2) -> EvalA e1 + ">" + EvalA e2
    | LThanB(e1, e2) -> EvalA e1 + "<" + EvalA e2
    | GEThanB(e1, e2) -> EvalA e1 + ">=" + EvalA e2
    | LEThanB(e1, e2) -> EvalA e1 + "<=" + EvalA e2
    | WutT -> "true"
    | WutF -> "false"

let rec edgesC (counter: int) (e: CExp) =
  match e with
  |AssignC (e1,e2) -> [(counter, e1 + ":=" + EvalA(e2), counter+1)]
  |AssignArrayC (e1,e2,e3) -> [(counter, e1 + "[" + EvalA e2 + "]" + ":=" + EvalA e3, counter+1)]
  |SkipC -> [(counter, "Skip" , counter+1)]
  |StateC (e1, e2) -> let xlist = edgesC (counter) e1
                      let rec statC (e1) = match e1 with 
                                           |(c,st,c2)::[] -> edgesC (c2) e2
                                           |head::tail -> statC tail  
                                           |[] -> edgesC (counter+1) e2
                      xlist@(statC (xlist)) 
  |IfStateC (gc1) -> edgesGC (counter) gc1
  |DoloopC (gc1) -> edgesGC (counter) gc1
and edgesGC (counter:int) (e: GCExp) =
    match e with
    |ARROWGC (b1,c1) -> (counter, evalB(b1), counter+1) :: edgesC (counter+1) c1
    |StateGC (g1,g2) -> edgesGC (counter) g1 @ edgesGC (counter) g2

// We parse the input
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = Parser.start Lexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// The response from the requests made on this website
// will contain system output from e.g. printfn


let strings = [|
    ("a:=10","AssignC(a,NUM 10)")
    //these two are straight from the fm4fun website
    ("y:=1; do x>0 -> y:=x*y; x:=x-1 od", "factorial function")
    ("i:=0; j:=0; do (i<n)&((j=m)|(i<j)) -> A[i]:=A[i]+27; i:=i+1 [] (j<m)&((i=n)|(!(i<j))) -> B[j]:=B[j]+12; j:=j+1 od", "database")
        |]
        
Array.map 
        (fun (toParse,expectedResult) -> 
            let actualResult = ((parse(toParse)))
            printfn "parsing %s gives the result:  %A  expected %A" toParse actualResult (edgesC 0 actualResult)
        )
        strings

// Feel free to copy this example and write some more test cases.
// NB: currently newline character \n will not be formatted
