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
        ("a:=10","NUM 10")
        ("b:=3+4", "PlusExpr (Num 3, Num 4)")
        ("skip","ff")
        ("c[4]:=4", "hof")    //  doesn't work
        ("if 4 != 3 -> skip fi", "fhf")     //doesn't work
        
        ("x:=3; y:=1; do x>0 -> y:=x*y; x:=x-1 od", "fj")
        |]
        
Array.map 
        (fun (toParse,expectedResult) -> 
            let actualResult = ((parse(toParse)))
            printfn "parsing %s gives the result:  %A  expected %A" toParse actualResult expectedResult 
        )
        strings

// Feel free to copy this example and write some more test cases.
// NB: currently newline character \n will not be formatted
