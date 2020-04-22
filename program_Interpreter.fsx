// This script implements our interactive calculator

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

let Sign (e1:int) = if (e1 > 0) then Set.ofList["+"]  else if (e1 < 0) then Set.ofList["-"] else Set.ofList["0"]

let Plus (s1) (s2) = Set.ofList(["+"])

let Minus (s1) (s2) = Set.ofList(["+"])
let Pow2 (s1) (s2) = Set.ofList(["+"])
let Times (s1) (s2) = Set.ofList(["+"])
let Div (s1) (s2) = Set.ofList(["+"])

let Equal (s1) (s2) =true
let NotEqual (s1) (s2) =true
let GreaterThan (s1) (s2) =true
let LessThan (s1) (s2) = true
let GEThan (s1) (s2) = true
let LEThan (s1) (s2) =true

let rec UMinus (s1) result = 
    match s1 with 
    |[] -> Set.ofList(result)
    |x::xlist -> let element = (if (x = "-" ) then "+" else if (x = "+") then "-" else "0")
                 UMinus (xlist) (result@([element]@xlist))
    



let rec SignA (a:AExp) absmemelement = 
    match a with 
    | Num(e1) ->Set.ofList([Sign (e1)])
    | Xval(e1) ->let (map1,map2) = absmemelement 
                 Map.find (e1) map1
    | UPlusExpr(e1) ->SignA (e1) absmemelement
    | PlusExpr(e1,e2) ->Plus (SignA e1 absmemelement) (SignA e2 absmemelement)
    | MinusExpr(e1,e2) ->Minus (SignA e1 absmemelement) (SignA e2 absmemelement)
    | PowExpr(e1,e2) ->Pow2 (SignA e1 absmemelement) (SignA e2 absmemelement)
    | TimesExpr(e1,e2) ->Times (SignA e1 absmemelement) (SignA e2 absmemelement)
    | UMinusExpr(e1) ->let s = SignA e1 absmemelement
                       UMinus (s) []
    | ArrayExpr(e1, e2) ->Set.ofList(["0"])
    | DivExpr(e1,e2) ->Div (SignA e1 absmemelement) (SignA e2 absmemelement)




let rec SignB (b:BExp) absmemelement =
    match b with
    | BitAndB(e1, e2) -> let lhs = (SignB e1 absmemelement)
                         let rhs = (SignB e2 absmemelement) 
                         lhs && rhs
    | BitOrB(e1, e2) ->let lhs = (SignB e1 absmemelement) 
                       let rhs = (SignB e2 absmemelement)
                       lhs || rhs
    | LogAndB(e1, e2) -> (SignB e1 absmemelement) && (SignB e2 absmemelement)
    | LogOrB(e1, e2) -> (SignB e1 absmemelement) || (SignB e2 absmemelement)
    | LogNotB(e1) ->  if (SignB e1 absmemelement) then false else true
    | BEqual(e1, e2) ->  Equal (SignA e1 absmemelement) (SignA e2 absmemelement)
    | NotEqualB(e1, e2) ->NotEqual (SignA e1 absmemelement) (SignA e2 absmemelement)
    | GThanB(e1, e2) ->  GreaterThan (SignA e1 absmemelement) (SignA e2 absmemelement)
    | LThanB(e1, e2) ->  LessThan (SignA e1 absmemelement) (SignA e2 absmemelement)
    | GEThanB(e1, e2) ->  GEThan (SignA e1 absmemelement) (SignA e2 absmemelement)
    | LEThanB(e1, e2) ->  LEThan (SignA e1 absmemelement) (SignA e2 absmemelement)
    | WutT -> true
    | WutF -> false



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
    | DivExpr(e1,e2) ->   evalA e1 / evalA e2

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
    
let rec calLength e1 sum = 
    match e1 with
    | [] -> sum
    | x::xlist -> calLength xlist (sum+1)
    
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
                          
    | ArrayExpr(e1, e2) -> let stringe1 = e1 + "[" + string (evalA e2) + "]"
                           let value1 = Map.tryFind stringe1 (mem: Map<string,int>)
                           value1
                            
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



let rec ApplyForAllElement (x) (a) absmemlist result = 
    match absmemlist  with 
    |[] ->Some (Set.ofList(result))
    |x2::xlist-> let s = SignA (a) x2
                 let (map1,map2) = x2
                 let map1Update = (Map.add (x,s) map1)
                 //ApplyForAllElement (x) (a) (xlist) (result@[(map1Update,map2)]@xlist) gives weird comparison errors
                 Some (Set.ofList(absmemlist)) // we neeed to fix this
                



let rec calLabel e absmem =
  match e with 
  |MemUpdate ( (e1,e2)) -> ApplyForAllElement (e1) (e2) (Set.toList(absmem)) ([])
  |SkipPG -> Some absmem
  |CheckBol(b)-> Some (Set.filter(fun x -> SignB (b) x) absmem) 
  |MemUpdateArray ( (e1,e2,e3)) -> Some absmem // will fix this some day       
                    
// We need a function which takes the edges and recursively runs the calLabel on each label until the end node is reached

let rec SignBforAll (b) absmemlist  = 
    match absmemlist  with 
    |[] -> true
    |x2::xlist-> (SignB b x2) && (SignBforAll (b) xlist)




let rec findnode xlist e absmem= // xlist is edges, e is node to find, and mem is memory 
    match xlist with 
    |[] -> None
    |(qstart,CheckBol(b),qend)::xs when qstart=e ->  if (SignBforAll b (Set.toList(absmem)))
                                                     then Some (qstart,CheckBol(b),qend)
                                                     else findnode xs e absmem
    |(qstart,label,qend)::xs when qstart=e -> Some (qstart,label,qend)
    |(qstart,label,qend)::xs -> findnode xs e absmem



let rec interpreter xlist e (absmem:Set<'a>) = // xlist is the list of edges, e is the current node, and mem is the memory
  match e with
  |1-> Some absmem 
  |x -> let edge = findnode xlist e absmem
        match edge with
        |None -> None
        |Some (qo,label,qe) -> let mem2 = calLabel label absmem
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
                      
                      
  |IfStateC (gc1) -> let (E,used1,d) = edgesGC (qo) gc1 (qe) (used) (false)
                     (E,used1)


  |DoloopC (gc1) -> let (E,used1,d) = edgesGC (qo) gc1 (qo) (used) (false)
                    (E@[(qo, CheckBol(LogNotB(matchdtob(d))), qe)],used1)
                    
                    
and edgesGC (qo:int) (e: GCExp) (qe: int) (used) (d)= // d is bool 
    match e with
    |ARROWGC (b1,c1) -> let lastq = subfunc(used)
                        let (clist,used1) = edgesC (lastq+1) (c1) (qe) (used@[lastq+1])
                        ([qo,CheckBol(LogAndB(b1,LogNotB(matchdtob(d)))),lastq+1]@clist,used1, ( (evalB (b1))|| d))
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
let absmem = Set.ofList([((Map.ofList[("z",Set.ofList["0"]);("y",Set.ofList["+"]);("x",Set.ofList["-"])]),(Map.ofList[("A",["0","+","-"])]))])
let strings = [|
    ("if x >= y -> z:=x [] y > x -> z:=y fi","AssignC(a,NUM 10)")
    //these are straight from the fm4fun website
     // ("y:=1; do x>0 -> y:=x*y; x:=x-1 od", "factorial function")
   // (" if x >= y -> z:=x [] y > x -> z:=y fi ","max")        //this one doesnt work because of the []
     // ("y:=1; if x>0 -> y:=x*y; x:=x-1 fi", "simple IF")
   // ("do x>=y -> z:=x [] y>x -> z:=y od", "testing do with []")    //this one doesn't work either
    //    ("i:=1; do i<n -> j:=i;  do (j>0)&&(A[j-1]>A[j]) -> t:=A[j]; A[j]:=A[j-1]; A[j-1]:=t; j:=j-1 od; i:=i+1 od", "insertion sort")
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
        //    printfn "evaluating the AST %A }" actualResult 
            printfn "evaluating the AST %A %A" actualResult  (edgeslist) 
                 // when printing out the graphviz, the text includes \ which needs to be removed...
        )
        strings
