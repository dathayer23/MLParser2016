// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace MLParser
open Microsoft.FSharp.Text.Lexing
open ML_Lexer

module main =
   [<EntryPoint>]
   let main argv = 
       printfn "%A" argv
       let lexbuf = LexBuffer<char>.FromString ("let x y = 1")
       let tokens = tokenize lexbuf

       0 // return an integer exit code
