// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace MLParser
open Microsoft.FSharp.Text.Lexing
open ML_Lexer

module Main  =

   let readTokens lexbuf  = 
      let rec getTokens lexbuf tokens = 
         let tk = tokenize lexbuf
         match  tk  with
         | EOF -> List.rev (tk :: tokens)
         | _ ->  tk :: (getTokens lexbuf tokens)

      getTokens lexbuf []


   [<EntryPoint>]
   let main argv = 
       printfn "%A" argv
       let lexbuf = LexBuffer<char>.FromString ("let x y = 1 and do while z < 1.0 end\n f : int -> float")
       let tokens = readTokens lexbuf
       
       0 // return an integer exit code
