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
// of edges of the graph

let rec Pow (e1) (e2) =
    match e2 with
    |1 -> e1
    |x -> Pow (e1*e1) (e2-1)
let rec evalA e =
    match e with
    | Num(e1) -> e1
    | Xval(e1) -> 0
    | UPlusExpr(e1) -> evalA (e1) 
    | PlusExpr(e1, e2) -> evalA e1 + evalA e2
    | MinusExpr(e1, e2) -> evalA e1 - evalA e2
    | PowExpr(e1, e2) -> Pow (evalA e1) (evalA e2)
    | TimesExpr(e1, e2) -> evalA e1 * evalA e2
    | UMinusExpr(e1) ->  - evalA e1
    | ArrayExpr(e1, e2) -> evalA e2
    | DivExpr(e1,e2) ->   evalA e1 /evalA e2


let rec evalB e =
    match e with
    | BitAndB(e1, e2) -> let lhs = evalB(e1) 
                         let rhs = evalB(e2) 
                         lhs && rhs
    | BitOrB(e1, e2) ->let lhs = evalB(e1) 
                       let rhs = evalB(e2) 
                       lhs || rhs
    | LogAndB(e1, e2) -> evalB e1 && evalB e2 
    | LogOrB(e1, e2) -> evalB e1 || evalB e2
    | LogNotB(e1) ->  not (evalB e1)
    | BEqual(e1, e2) ->  evalA e1 = evalA e2
    | NotEqualB(e1, e2) ->evalA e1 <> evalA e2
    | GThanB(e1, e2) ->  evalA e1 > evalA e2
    | LThanB(e1, e2) ->   evalA e1 < evalA e2
    | GEThanB(e1, e2) ->  evalA e1 >= evalA e2
    | LEThanB(e1, e2) ->  evalA e1 <= evalA e2
    | WutT -> true
    | WutF -> false
    
let rec calA e mem =
    match e with
    | Num(e1) ->  Some e1
    | Xval(e1) -> Map.tryFind e1 mem
    | UPlusExpr(e1) -> calA (e1) (mem)
    | PlusExpr(e1, e2) -> let value1 = (calA e1 mem) 
                          let value2 = (calA e2 mem)
                          match value1 with 
                          | Some a -> match value2 with 
                                      |Some a2 -> Some (a + a2)
                                      |None ->None
                          |None ->None
    | MinusExpr(e1, e2) -> let value1 = (calA e1 mem) 
                           let value2 = (calA e2 mem)
                           match value1 with 
                           | Some a -> match value2 with 
                                       |Some a2 -> Some(a - a2)
                                       |None ->None
                           |None ->None
    | PowExpr(e1, e2) ->  let value1 = (calA e1 mem) 
                          let value2 = (calA e2 mem)
                          match value1 with 
                          | Some a -> match value2 with 
                                      |Some a2 ->Some (Pow (a) (a2))
                                      |None ->None
                          |None ->None
    | TimesExpr(e1, e2) -> let value1 = (calA e1 mem) 
                           let value2 = (calA e2 mem)
                           match value1 with 
                           | Some a -> match value2 with 
                                       |Some a2 ->Some( a * a2)
                                       |None ->None
                           |None ->None
    | UMinusExpr(e1) ->   let value1 = (calA e1 mem) 
                          match value1 with 
                          | Some a -> Some (- a)
                          |None ->None
    | ArrayExpr(e1, e2) -> Some 3  // just for testing... need to be fixed such that we return the value stored at the memory location e1[e2] 
    | DivExpr(e1,e2) ->   let value1 = (calA e1 mem) 
                          let value2 = (calA e2 mem)
                          match value1 with 
                          | Some a -> match value2 with 
                                      |Some a2 -> Some (a / a2)
                                      |None ->None
                          |None ->None


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
    | BEqual(e1, e2) ->  let value1 = calA e1 mem 
                         let value2 = calA e2 mem
                         match value1 with 
                         |Some b1 -> match value2 with 
                                     | Some b2 -> b1=b2
                                     | None -> false
                         |None -> false
    | NotEqualB(e1, e2) -> let value1 = calA e1 mem 
                           let value2 = calA e2 mem
                           match value1 with 
                           |Some b1 -> match value2 with 
                                       | Some b2 -> b1<>b2
                                       | None -> false
                           |None -> false
    | GThanB(e1, e2) ->  let value1 = calA e1 mem 
                         let value2 = calA e2 mem
                         match value1 with 
                         |Some b1 -> match value2 with 
                                     | Some b2 -> b1>b2
                                     | None -> false
                         |None -> false
    | LThanB(e1, e2) ->  let value1 = calA e1 mem 
                         let value2 = calA e2 mem
                         match value1 with 
                         |Some b1 -> match value2 with 
                                     | Some b2 -> b1<b2
                                     | None -> false
                         |None -> false
    | GEThanB(e1, e2) -> let value1 = calA e1 mem 
                         let value2 = calA e2 mem
                         match value1 with 
                         |Some b1 -> match value2 with 
                                     | Some b2 -> b1>=b2
                                     | None -> false
                         |None -> false
    | LEThanB(e1, e2) -> let value1 = calA e1 mem 
                         let value2 = calA e2 mem
                         match value1 with 
                         |Some b1 -> match value2 with 
                                     | Some b2 -> b1<=b2
                                     | None -> false
                         |None -> false
    | WutT -> true
    | WutF -> false

let rec calLabel e mem =
  match e with 
  |MemUpdate ( (e1,e2)) -> let value = calA e2 mem
                           match value with 
                           |Some a -> match (Map.tryFind e1 mem) with
                                      |Some x ->  Some (Map.add e1 a mem)
                                      |None -> None
                           |None -> None
  |SkipPG -> Some mem
  |CheckBol(b)-> if (calB b mem)
                 then Some mem
                 else None
  |MemUpdateArray ( (e1,e2,e3)) ->  Some mem // need to figure out how to access memories for arrays 


// We need a function which takes the edges and recursively runs the calLabel on each label until the end node is reached

let rec findnode xlist e mem= // xlist is edges e is node to find and mem is memory 
    match xlist with 
    |[] -> None
    |(qstart,CheckBol(b),qend)::xs when qstart=e ->  if (calB b mem)
                                                     then Some (qstart,CheckBol(b),qend)
                                                     else findnode xs e mem
    |(qstart,label,qend)::xs when qstart=e -> Some (qstart,label,qend)
    |(qstart,label,qend)::xs -> findnode xs e mem



let rec interpreter xlist e mem = // xlist is the list of edges, e is the current node and mem is the memory
  match e with
  |1-> Some mem // as 1 is the end node in our program
  |x -> let edge = findnode xlist e mem
        match edge with
        |None -> None
        |Some (qo,label,qe) -> let mem2 = calLabel label mem
                               match mem2 with 
                               |Some a -> interpreter xlist qe a
                               |None ->None
  
let rec subfunc (used)=
    match used with
    |[] -> 0 //never the case 
    |x::[] ->x // last node in the used 
    |x::xs ->subfunc(xs)
let booltostring (d)=
    if (d)
    then "true"
    else "false"

//Creates edges for the interpreter 

    
let matchdtob d =
    match d with
    |true -> WutT
    |false -> WutF
    
let rec edgesC (qo: int) (e: CExp) (qe: int) (used) = //initially qe=1, qo=0, used= [0,1]
  match e with
  |AssignC (e1,e2) -> ([(qo, MemUpdate((e1,e2)), qe)],used)
  |AssignArrayC (e1,e2,e3) -> ([(qo, MemUpdateArray((e1,e2,e3)), qe)],used)
  |SkipC -> ([(qo, SkipPG , qe)],used)
  |StateC (e1, e2) -> let lastq = subfunc(used)
                      let (e1list,used1) = edgesC (qo) (e1) (lastq+1) (used@[lastq+1]) 
                      let (e2list,used2) = edgesC (lastq+1) (e2) (qe) (used1)
                      (e1list@e2list,used2) 
  |IfStateC (gc1) ->let (E,used1,d) = edgesGC (qo) gc1 (qe) (used) (false)
                    (E,used1)
  |DoloopC (gc1) -> let (E,used1,d) = edgesGC (qo) gc1 (qo) (used) (false)
                    (E@[(qo, CheckBol(LogNotB(matchdtob(d))), qe)],used1)
and edgesGC (qo:int) (e: GCExp) (qe: int) (used) (d)= // d is bool 
    match e with
    |ARROWGC (b1,c1) -> let lastq = subfunc(used)
                        let (clist,used1) = edgesC (lastq+1) (c1) (qe) (used@[lastq+1])
                        ([qo,CheckBol(LogAndB(b1,LogNotB(matchdtob(d)))),lastq+1]@clist,used1, ( (evalB (b1))|| d))// this should be b|d but for that need  B to be boolean and too lazy to do that
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
let mem = Map.ofList[("z",0);("y",10);("x",8)]
let strings = [|
   // ("a:=10","AssignC(a,NUM 10)")
    //these two are straight from the fm4fun website
    //  ("y:=1; do x>0 -> y:=x*y; x:=x-1 od", "factorial function")
     (" if x >= y -> z:=x [] y > x -> z:=y fi ","max")
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
            printfn "evaluating the AST %A gives the result:  %A \n }" actualResult (edgeslist) //(interpreter edgeslist 0 mem)
                 // when printing out the graphviz, the text includes \ which needs to be removed...
        )
        strings
// Feel free to copy this example and write some more test cases.
// NB: currently newline character \n will not be formatted
