// Implementation file for parser generated by fsyacc
namespace MlParser2016
module MlParser =
   #nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
   open Microsoft.FSharp.Text.Lexing
   open Microsoft.FSharp.Text.Parsing.ParseHelpers
   # 1 "C:\Users\DThayer\Documents\GitHub\MLParser2016\MlParser\MlParser\ML_parse.fsy"


   open MLAst

   # 10 "C:\Users\DThayer\Documents\GitHub\MLParser2016\MlParser\MlParser\ML_parse.fs"
   // This type is the type of tokens accepted by the parser
   type token = 
     | EOF
     | HELLO
     | SINGLE_QUOTE
     | BACKSLASH
     | DBL_QUOTE
     | DOT
     | ARROW
     | STAR
     | ELLIPSIS
     | UNDERLINE
     | HASH
     | COMMA
     | BAR
     | RIGHT_BRACE
     | LEFT_BRACE
     | RIGHT_BRACKET
     | LEFT_BRACKET
     | RIGHT_PAREN
     | LEFT_PAREN
     | DBLARROR
     | COLON
     | SEMICOLON
     | EQ
     | TYPEVAR of (string)
     | FLOAT of (string)
     | HEX of (string)
     | INT of (string)
     | SYMB_ID of (string)
     | ID of (string)
     | END
     | OP
     | IN
     | AND
     | AS
     | OF
     | FN
     | CASE
     | DO
     | WHILE
     | ELSE
     | THEN
     | IF
     | LET
     | REC
     | FUN
     | RAISE
     | HANDLE
     | ORELSE
     | ANDALSO
     | WITH
     | EXCEPTION
     | INCLUDE
     | SHARING
     | DATATYPE
     | WITHTYPE
     | ABSTYPE
     | EQTYPE
     | VAL
     | TYPE
     | WHERE
     | NONFIX
     | INFIXR
     | INFIX
     | SIG
     | STRUCT
     | FUNCTOR
     | SIGNATURE
     | OPEN
     | LOCAL
     | STRUCTURE
   // This type is used to give symbolic names to token indexes, useful for error messages
   type tokenId = 
       | TOKEN_EOF
       | TOKEN_HELLO
       | TOKEN_SINGLE_QUOTE
       | TOKEN_BACKSLASH
       | TOKEN_DBL_QUOTE
       | TOKEN_DOT
       | TOKEN_ARROW
       | TOKEN_STAR
       | TOKEN_ELLIPSIS
       | TOKEN_UNDERLINE
       | TOKEN_HASH
       | TOKEN_COMMA
       | TOKEN_BAR
       | TOKEN_RIGHT_BRACE
       | TOKEN_LEFT_BRACE
       | TOKEN_RIGHT_BRACKET
       | TOKEN_LEFT_BRACKET
       | TOKEN_RIGHT_PAREN
       | TOKEN_LEFT_PAREN
       | TOKEN_DBLARROR
       | TOKEN_COLON
       | TOKEN_SEMICOLON
       | TOKEN_EQ
       | TOKEN_TYPEVAR
       | TOKEN_FLOAT
       | TOKEN_HEX
       | TOKEN_INT
       | TOKEN_SYMB_ID
       | TOKEN_ID
       | TOKEN_END
       | TOKEN_OP
       | TOKEN_IN
       | TOKEN_AND
       | TOKEN_AS
       | TOKEN_OF
       | TOKEN_FN
       | TOKEN_CASE
       | TOKEN_DO
       | TOKEN_WHILE
       | TOKEN_ELSE
       | TOKEN_THEN
       | TOKEN_IF
       | TOKEN_LET
       | TOKEN_REC
       | TOKEN_FUN
       | TOKEN_RAISE
       | TOKEN_HANDLE
       | TOKEN_ORELSE
       | TOKEN_ANDALSO
       | TOKEN_WITH
       | TOKEN_EXCEPTION
       | TOKEN_INCLUDE
       | TOKEN_SHARING
       | TOKEN_DATATYPE
       | TOKEN_WITHTYPE
       | TOKEN_ABSTYPE
       | TOKEN_EQTYPE
       | TOKEN_VAL
       | TOKEN_TYPE
       | TOKEN_WHERE
       | TOKEN_NONFIX
       | TOKEN_INFIXR
       | TOKEN_INFIX
       | TOKEN_SIG
       | TOKEN_STRUCT
       | TOKEN_FUNCTOR
       | TOKEN_SIGNATURE
       | TOKEN_OPEN
       | TOKEN_LOCAL
       | TOKEN_STRUCTURE
       | TOKEN_end_of_input
       | TOKEN_error
   // This type is used to give symbolic names to token indexes, useful for error messages
   type nonTerminalId = 
       | NONTERM__startstart
       | NONTERM_start
       | NONTERM_File

   // This function maps tokens to integer indexes
   let tagOfToken (t:token) = 
     match t with
     | EOF  -> 0 
     | HELLO  -> 1 
     | SINGLE_QUOTE  -> 2 
     | BACKSLASH  -> 3 
     | DBL_QUOTE  -> 4 
     | DOT  -> 5 
     | ARROW  -> 6 
     | STAR  -> 7 
     | ELLIPSIS  -> 8 
     | UNDERLINE  -> 9 
     | HASH  -> 10 
     | COMMA  -> 11 
     | BAR  -> 12 
     | RIGHT_BRACE  -> 13 
     | LEFT_BRACE  -> 14 
     | RIGHT_BRACKET  -> 15 
     | LEFT_BRACKET  -> 16 
     | RIGHT_PAREN  -> 17 
     | LEFT_PAREN  -> 18 
     | DBLARROR  -> 19 
     | COLON  -> 20 
     | SEMICOLON  -> 21 
     | EQ  -> 22 
     | TYPEVAR _ -> 23 
     | FLOAT _ -> 24 
     | HEX _ -> 25 
     | INT _ -> 26 
     | SYMB_ID _ -> 27 
     | ID _ -> 28 
     | END  -> 29 
     | OP  -> 30 
     | IN  -> 31 
     | AND  -> 32 
     | AS  -> 33 
     | OF  -> 34 
     | FN  -> 35 
     | CASE  -> 36 
     | DO  -> 37 
     | WHILE  -> 38 
     | ELSE  -> 39 
     | THEN  -> 40 
     | IF  -> 41 
     | LET  -> 42 
     | REC  -> 43 
     | FUN  -> 44 
     | RAISE  -> 45 
     | HANDLE  -> 46 
     | ORELSE  -> 47 
     | ANDALSO  -> 48 
     | WITH  -> 49 
     | EXCEPTION  -> 50 
     | INCLUDE  -> 51 
     | SHARING  -> 52 
     | DATATYPE  -> 53 
     | WITHTYPE  -> 54 
     | ABSTYPE  -> 55 
     | EQTYPE  -> 56 
     | VAL  -> 57 
     | TYPE  -> 58 
     | WHERE  -> 59 
     | NONFIX  -> 60 
     | INFIXR  -> 61 
     | INFIX  -> 62 
     | SIG  -> 63 
     | STRUCT  -> 64 
     | FUNCTOR  -> 65 
     | SIGNATURE  -> 66 
     | OPEN  -> 67 
     | LOCAL  -> 68 
     | STRUCTURE  -> 69 

   // This function maps integer indexes to symbolic token ids
   let tokenTagToTokenId (tokenIdx:int) = 
     match tokenIdx with
     | 0 -> TOKEN_EOF 
     | 1 -> TOKEN_HELLO 
     | 2 -> TOKEN_SINGLE_QUOTE 
     | 3 -> TOKEN_BACKSLASH 
     | 4 -> TOKEN_DBL_QUOTE 
     | 5 -> TOKEN_DOT 
     | 6 -> TOKEN_ARROW 
     | 7 -> TOKEN_STAR 
     | 8 -> TOKEN_ELLIPSIS 
     | 9 -> TOKEN_UNDERLINE 
     | 10 -> TOKEN_HASH 
     | 11 -> TOKEN_COMMA 
     | 12 -> TOKEN_BAR 
     | 13 -> TOKEN_RIGHT_BRACE 
     | 14 -> TOKEN_LEFT_BRACE 
     | 15 -> TOKEN_RIGHT_BRACKET 
     | 16 -> TOKEN_LEFT_BRACKET 
     | 17 -> TOKEN_RIGHT_PAREN 
     | 18 -> TOKEN_LEFT_PAREN 
     | 19 -> TOKEN_DBLARROR 
     | 20 -> TOKEN_COLON 
     | 21 -> TOKEN_SEMICOLON 
     | 22 -> TOKEN_EQ 
     | 23 -> TOKEN_TYPEVAR 
     | 24 -> TOKEN_FLOAT 
     | 25 -> TOKEN_HEX 
     | 26 -> TOKEN_INT 
     | 27 -> TOKEN_SYMB_ID 
     | 28 -> TOKEN_ID 
     | 29 -> TOKEN_END 
     | 30 -> TOKEN_OP 
     | 31 -> TOKEN_IN 
     | 32 -> TOKEN_AND 
     | 33 -> TOKEN_AS 
     | 34 -> TOKEN_OF 
     | 35 -> TOKEN_FN 
     | 36 -> TOKEN_CASE 
     | 37 -> TOKEN_DO 
     | 38 -> TOKEN_WHILE 
     | 39 -> TOKEN_ELSE 
     | 40 -> TOKEN_THEN 
     | 41 -> TOKEN_IF 
     | 42 -> TOKEN_LET 
     | 43 -> TOKEN_REC 
     | 44 -> TOKEN_FUN 
     | 45 -> TOKEN_RAISE 
     | 46 -> TOKEN_HANDLE 
     | 47 -> TOKEN_ORELSE 
     | 48 -> TOKEN_ANDALSO 
     | 49 -> TOKEN_WITH 
     | 50 -> TOKEN_EXCEPTION 
     | 51 -> TOKEN_INCLUDE 
     | 52 -> TOKEN_SHARING 
     | 53 -> TOKEN_DATATYPE 
     | 54 -> TOKEN_WITHTYPE 
     | 55 -> TOKEN_ABSTYPE 
     | 56 -> TOKEN_EQTYPE 
     | 57 -> TOKEN_VAL 
     | 58 -> TOKEN_TYPE 
     | 59 -> TOKEN_WHERE 
     | 60 -> TOKEN_NONFIX 
     | 61 -> TOKEN_INFIXR 
     | 62 -> TOKEN_INFIX 
     | 63 -> TOKEN_SIG 
     | 64 -> TOKEN_STRUCT 
     | 65 -> TOKEN_FUNCTOR 
     | 66 -> TOKEN_SIGNATURE 
     | 67 -> TOKEN_OPEN 
     | 68 -> TOKEN_LOCAL 
     | 69 -> TOKEN_STRUCTURE 
     | 72 -> TOKEN_end_of_input
     | 70 -> TOKEN_error
     | _ -> failwith "tokenTagToTokenId: bad token"

   /// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
   let prodIdxToNonTerminal (prodIdx:int) = 
     match prodIdx with
       | 0 -> NONTERM__startstart 
       | 1 -> NONTERM_start 
       | 2 -> NONTERM_File 
       | _ -> failwith "prodIdxToNonTerminal: bad production index"

   let _fsyacc_endOfInputTag = 72 
   let _fsyacc_tagOfErrorTerminal = 70

   // This function gets the name of a token as a string
   let token_to_string (t:token) = 
     match t with 
     | EOF  -> "EOF" 
     | HELLO  -> "HELLO" 
     | SINGLE_QUOTE  -> "SINGLE_QUOTE" 
     | BACKSLASH  -> "BACKSLASH" 
     | DBL_QUOTE  -> "DBL_QUOTE" 
     | DOT  -> "DOT" 
     | ARROW  -> "ARROW" 
     | STAR  -> "STAR" 
     | ELLIPSIS  -> "ELLIPSIS" 
     | UNDERLINE  -> "UNDERLINE" 
     | HASH  -> "HASH" 
     | COMMA  -> "COMMA" 
     | BAR  -> "BAR" 
     | RIGHT_BRACE  -> "RIGHT_BRACE" 
     | LEFT_BRACE  -> "LEFT_BRACE" 
     | RIGHT_BRACKET  -> "RIGHT_BRACKET" 
     | LEFT_BRACKET  -> "LEFT_BRACKET" 
     | RIGHT_PAREN  -> "RIGHT_PAREN" 
     | LEFT_PAREN  -> "LEFT_PAREN" 
     | DBLARROR  -> "DBLARROR" 
     | COLON  -> "COLON" 
     | SEMICOLON  -> "SEMICOLON" 
     | EQ  -> "EQ" 
     | TYPEVAR _ -> "TYPEVAR" 
     | FLOAT _ -> "FLOAT" 
     | HEX _ -> "HEX" 
     | INT _ -> "INT" 
     | SYMB_ID _ -> "SYMB_ID" 
     | ID _ -> "ID" 
     | END  -> "END" 
     | OP  -> "OP" 
     | IN  -> "IN" 
     | AND  -> "AND" 
     | AS  -> "AS" 
     | OF  -> "OF" 
     | FN  -> "FN" 
     | CASE  -> "CASE" 
     | DO  -> "DO" 
     | WHILE  -> "WHILE" 
     | ELSE  -> "ELSE" 
     | THEN  -> "THEN" 
     | IF  -> "IF" 
     | LET  -> "LET" 
     | REC  -> "REC" 
     | FUN  -> "FUN" 
     | RAISE  -> "RAISE" 
     | HANDLE  -> "HANDLE" 
     | ORELSE  -> "ORELSE" 
     | ANDALSO  -> "ANDALSO" 
     | WITH  -> "WITH" 
     | EXCEPTION  -> "EXCEPTION" 
     | INCLUDE  -> "INCLUDE" 
     | SHARING  -> "SHARING" 
     | DATATYPE  -> "DATATYPE" 
     | WITHTYPE  -> "WITHTYPE" 
     | ABSTYPE  -> "ABSTYPE" 
     | EQTYPE  -> "EQTYPE" 
     | VAL  -> "VAL" 
     | TYPE  -> "TYPE" 
     | WHERE  -> "WHERE" 
     | NONFIX  -> "NONFIX" 
     | INFIXR  -> "INFIXR" 
     | INFIX  -> "INFIX" 
     | SIG  -> "SIG" 
     | STRUCT  -> "STRUCT" 
     | FUNCTOR  -> "FUNCTOR" 
     | SIGNATURE  -> "SIGNATURE" 
     | OPEN  -> "OPEN" 
     | LOCAL  -> "LOCAL" 
     | STRUCTURE  -> "STRUCTURE" 

   // This function gets the data carried by a token as an object
   let _fsyacc_dataOfToken (t:token) = 
     match t with 
     | EOF  -> (null : System.Object) 
     | HELLO  -> (null : System.Object) 
     | SINGLE_QUOTE  -> (null : System.Object) 
     | BACKSLASH  -> (null : System.Object) 
     | DBL_QUOTE  -> (null : System.Object) 
     | DOT  -> (null : System.Object) 
     | ARROW  -> (null : System.Object) 
     | STAR  -> (null : System.Object) 
     | ELLIPSIS  -> (null : System.Object) 
     | UNDERLINE  -> (null : System.Object) 
     | HASH  -> (null : System.Object) 
     | COMMA  -> (null : System.Object) 
     | BAR  -> (null : System.Object) 
     | RIGHT_BRACE  -> (null : System.Object) 
     | LEFT_BRACE  -> (null : System.Object) 
     | RIGHT_BRACKET  -> (null : System.Object) 
     | LEFT_BRACKET  -> (null : System.Object) 
     | RIGHT_PAREN  -> (null : System.Object) 
     | LEFT_PAREN  -> (null : System.Object) 
     | DBLARROR  -> (null : System.Object) 
     | COLON  -> (null : System.Object) 
     | SEMICOLON  -> (null : System.Object) 
     | EQ  -> (null : System.Object) 
     | TYPEVAR _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
     | FLOAT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
     | HEX _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
     | INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
     | SYMB_ID _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
     | ID _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
     | END  -> (null : System.Object) 
     | OP  -> (null : System.Object) 
     | IN  -> (null : System.Object) 
     | AND  -> (null : System.Object) 
     | AS  -> (null : System.Object) 
     | OF  -> (null : System.Object) 
     | FN  -> (null : System.Object) 
     | CASE  -> (null : System.Object) 
     | DO  -> (null : System.Object) 
     | WHILE  -> (null : System.Object) 
     | ELSE  -> (null : System.Object) 
     | THEN  -> (null : System.Object) 
     | IF  -> (null : System.Object) 
     | LET  -> (null : System.Object) 
     | REC  -> (null : System.Object) 
     | FUN  -> (null : System.Object) 
     | RAISE  -> (null : System.Object) 
     | HANDLE  -> (null : System.Object) 
     | ORELSE  -> (null : System.Object) 
     | ANDALSO  -> (null : System.Object) 
     | WITH  -> (null : System.Object) 
     | EXCEPTION  -> (null : System.Object) 
     | INCLUDE  -> (null : System.Object) 
     | SHARING  -> (null : System.Object) 
     | DATATYPE  -> (null : System.Object) 
     | WITHTYPE  -> (null : System.Object) 
     | ABSTYPE  -> (null : System.Object) 
     | EQTYPE  -> (null : System.Object) 
     | VAL  -> (null : System.Object) 
     | TYPE  -> (null : System.Object) 
     | WHERE  -> (null : System.Object) 
     | NONFIX  -> (null : System.Object) 
     | INFIXR  -> (null : System.Object) 
     | INFIX  -> (null : System.Object) 
     | SIG  -> (null : System.Object) 
     | STRUCT  -> (null : System.Object) 
     | FUNCTOR  -> (null : System.Object) 
     | SIGNATURE  -> (null : System.Object) 
     | OPEN  -> (null : System.Object) 
     | LOCAL  -> (null : System.Object) 
     | STRUCTURE  -> (null : System.Object) 
   let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 1us; 65535us; 0us; 2us; |]
   let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; |]
   let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 1us; 2us; |]
   let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; |]
   let _fsyacc_action_rows = 5
   let _fsyacc_actionTableElements = [|1us; 32768us; 1us; 4us; 0us; 49152us; 1us; 32768us; 0us; 3us; 0us; 16385us; 0us; 16386us; |]
   let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; 5us; 6us; |]
   let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; |]
   let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; |]
   let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 16386us; |]
   let _fsyacc_reductions ()  =    [| 
   # 483 "C:\Users\DThayer\Documents\GitHub\MLParser2016\MlParser\MlParser\ML_parse.fs"
           (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
               let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : MLAst.program)) in
               Microsoft.FSharp.Core.Operators.box
                   (
                      (
                         raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                      )
                    : '_startstart));
   # 492 "C:\Users\DThayer\Documents\GitHub\MLParser2016\MlParser\MlParser\ML_parse.fs"
           (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
               let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'File)) in
               Microsoft.FSharp.Core.Operators.box
                   (
                      (
   # 86 "C:\Users\DThayer\Documents\GitHub\MLParser2016\MlParser\MlParser\ML_parse.fsy"
                                         %1
                      )
   # 86 "C:\Users\DThayer\Documents\GitHub\MLParser2016\MlParser\MlParser\ML_parse.fsy"
                    : MLAst.program));
   # 503 "C:\Users\DThayer\Documents\GitHub\MLParser2016\MlParser\MlParser\ML_parse.fs"
           (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
               Microsoft.FSharp.Core.Operators.box
                   (
                      (
   # 89 "C:\Users\DThayer\Documents\GitHub\MLParser2016\MlParser\MlParser\ML_parse.fsy"
                                           1 
                      )
   # 89 "C:\Users\DThayer\Documents\GitHub\MLParser2016\MlParser\MlParser\ML_parse.fsy"
                    : 'File));
   |]
   # 514 "C:\Users\DThayer\Documents\GitHub\MLParser2016\MlParser\MlParser\ML_parse.fs"
   let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
     { reductions= _fsyacc_reductions ();
       endOfInputTag = _fsyacc_endOfInputTag;
       tagOfToken = tagOfToken;
       dataOfToken = _fsyacc_dataOfToken; 
       actionTableElements = _fsyacc_actionTableElements;
       actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
       stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
       stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
       reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
       immediateActions = _fsyacc_immediateActions;
       gotos = _fsyacc_gotos;
       sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
       tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
       parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                                 match parse_error_rich with 
                                 | Some f -> f ctxt
                                 | None -> parse_error ctxt.Message);
       numTerminals = 73;
       productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
   let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
   let start lexer lexbuf : MLAst.program =
       Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))