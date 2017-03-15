module Json

open Parse
open System 
open Microsoft.FSharp.Core.Operators

type Json =
    | String of string
    | Int of int
    | Float of float
    | Object of Map<string,Json>
    | Array of Json list
    | Bool of bool
    | Null


let unescapedChar = Parse.satisfy (fun ch -> ch <> '\\' && ch <> '\"')


let escapedChar = 
    [ 
    // (stringToMatch, resultChar)
    ("\\\"",'\"')      // quote
    ("\\\\",'\\')      // reverse solidus 
    ("\\/",'/')        // solidus
    ("\\b",'\b')       // backspace
    ("\\f",'\f')       // formfeed
    ("\\n",'\n')       // newline
    ("\\r",'\r')       // cr
    ("\\t",'\t')       // tab
    ] 
    // convert each pair into a parser
    |> List.map (fun (toMatch,result) -> 
        Parse.string toMatch >>% result)
    // and combine them into one
    |> Parse.choice


/// Parse a unicode char
let unicodeChar = 
    
    // set up the "primitive" parsers        
    let backslash = Parse.char '\\'
    let uChar = Parse.char 'u'
    let hexdigit = anyOf (['0'..'9'] @ ['A'..'F'] @ ['a'..'f'])

    // convert the parser output (nested tuples)
    // to a char
    let convertToChar (((h1,h2),h3),h4) = 
        let str = sprintf "%c%c%c%c" h1 h2 h3 h4
        Int32.Parse(str,Globalization.NumberStyles.HexNumber) |> char

    // set up the main parser
    backslash  >>. uChar >>. hexdigit .>>. hexdigit .>>. hexdigit .>>. hexdigit
    |>> convertToChar 


let quotedString = 
    let quote = Parse.char '\"'
    let jchar = unescapedChar <|> escapedChar <|> unicodeChar 

    // set up the main parser
    quote >>. Parse.zeroOrMoreChars jchar .>> quote 
