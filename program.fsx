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

let rec Pow (e1) (e2) =
    match e2 with
    |1 -> e1
    |x -> Pow (e1*e1) (e2-1)


let rec calA e mem = 
    match e with
    | Num(e1) -> e1
    | Xval(e1) -> 0 // take value from the memory
    | UPlusExpr(e1) -> calA (e1) (mem)
    | PlusExpr(e1, e2) -> (calA e1 mem) + (calA e2 mem)
    | MinusExpr(e1, e2) -> (calA e1 mem) - (calA e2 mem)
    | PowExpr(e1, e2) -> Pow (calA e1 mem) ( calA e2 mem)
    | TimesExpr(e1, e2) -> (calA e1 mem) * ( calA e2 mem)
    | UMinusExpr(e1) -> - calA e1 mem
    | ArrayExpr(e1, e2) -> calA e2 mem // need to change this  
    | DivExpr(e1,e2) -> (calA e1 mem) / ( calA e2 mem)

let rec calB e mem =
    match e with
    | BitAndB(e1, e2) -> let lhs = calB(e1) mem
                         let rhs = calB(e2) mem
                         lhs && rhs
    | BitOrB(e1, e2) ->let lhs = calB(e1) mem
                       let rhs = calB(e2) mem
                       lhs || rhs
    | LogAndB(e1, e2) -> calB e1 mem && calB e2 mem
    | LogOrB(e1, e2) -> calB e1 mem || calB e2 mem
    | LogNotB(e1) ->  not (calB e1 mem)
    | BEqual(e1, e2) -> calA e1 mem = calA e2 mem
    | NotEqualB(e1, e2) -> calA e1 mem <> calA e2 mem
    | GThanB(e1, e2) -> calA e1 mem > calA e2 mem
    | LThanB(e1, e2) -> calA e1 mem < calA e2 mem
    | GEThanB(e1, e2) -> calA e1 mem >= calA e2 mem
    | LEThanB(e1, e2) -> calA e1 mem <= calA e2 mem
    | WutT -> true
    | WutF -> false

let rec calC e mem = // return memory
  match e with
  |AssignC (e1,e2) // change memory
  | AssignArrayC (e1,e2,e3) // change memory
  | SkipC -> mem
  | StateC (e1,e2) // follow the book
  | IfStateC (gc1) //follow the book
  | DoloopC (gc1) // follow the book
and calGC e mem =
 match e with 
 |ARROWGC (b1,c1) //follow the book
 |StateGC (g1,g2) //follow the book


let rec subfunc (used)=
    match used with
    |[] -> 0 //never the case 
    |x::[] ->x // last node in the used 
    |x::xs ->subfunc(xs)

let booltostring (d)=
    if (d)
    then "true"
    else "false"

let rec doneGC e =
    match e with
    |ARROWGC (b1,c1) -> "!" + evalB (b1)
    |StateGC (g1,g2) -> doneGC (g1) + "$$" + doneGC (g2)

let rec edgesC (qo: int) (e: CExp) (qe: int) (used) = //initially qe=1, qo=0, used= [0,1]
  match e with
  |AssignC (e1,e2) -> ([(qo, e1 + ":=" + EvalA(e2), qe)],used)
  |AssignArrayC (e1,e2,e3) -> ([(qo, e1 + "[" + EvalA e2 + "]" + ":=" + EvalA e3, qe)],used)
  |SkipC -> ([(qo, "Skip" , qe)],used)
  |StateC (e1, e2) -> let lastq = subfunc(used)
                      let (e1list,used1) = edgesC (qo) (e1) (lastq+1) (used@[lastq+1]) 
                      let (e2list,used2) = edgesC (lastq+1) (e2) (qe) (used1)
                      (e1list@e2list,used2) 
  |IfStateC (gc1) ->let (E,used1,d) = edgesGC (qo) gc1 (qe) (used) (false)
                    (E,used1)
  |DoloopC (gc1) -> let (E,used1,d) = edgesGC (qo) gc1 (qo) (used) (false)
                    (E@[(qo, "!" + booltostring(d), qe)],used1)
and edgesGC (qo:int) (e: GCExp) (qe: int) (used) (d)= // d is bool 
    match e with
    |ARROWGC (b1,c1) -> let lastq = subfunc(used)
                        let (clist,used1) = edgesC (lastq+1) (c1) (qe) (used@[lastq+1])
                        ([qo,evalB(b1) + "&&" + "!" + booltostring(d),lastq+1]@clist,used1, d)// this should be b|d but for that need to B to be boolean and too lazy to do that
    |StateGC (g1,g2) -> let (gclist1,used1,d1) = edgesGC (qo) (g1) (qe) (used) (d)              
                        let (gclist2,used2,d2) = edgesGC (qo) (g2) (qe) (used1) (d1)
                        (gclist1@gclist2,used2,d2)



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

//Set the following boolean constant to true if you want the deterministic Program Graph


let strings = [|
    ("a:=10","AssignC(a,NUM 10)")
    //these two are straight from the fm4fun website
    ("y:=1; do x>0 -> y:=x*y; x:=x-1 od", "factorial function")
    ("i:=0; j:=0; do (i<n)&((j=m)|(i<j)) -> A[i]:=A[i]+27; i:=i+1 [] (j<m)&((i=n)|(!(i<j))) -> B[j]:=B[j]+12; j:=j+1 od", "database")
        |]
     
let rec listtograph (edgeslist) =
    match edgeslist with
    |[] -> "digraph program_graph {\n node [shape = circle]; q0; \n node [shape = doublecircle]; q1; \n node [shape = circle]"
    |x::xs->let (qo,label,qe) = x 
            listtograph(xs) + "\n" + "q" + string qo + "->" + "q" + string qe + "[label = \"" + label + "\"" + "];"
        
Array.map 
        (fun (toParse,expectedResult) -> 
            let actualResult = ((parse(toParse)))
            let (edgeslist,used) = (edgesC (0) (actualResult) (1) ([0;1]))
            printfn "evaluating the AST %A gives the result:  %A \n %s \n }" actualResult (edgeslist) (listtograph (edgeslist))
                 // when printing out the graphviz, the text includes \ which needs to be removed...
             
        )
        strings

// Feel free to copy this example and write some more test cases.
// NB: currently newline character \n will not be formatted
