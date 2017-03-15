module Parse

open Result
open System


type Parser<'a> = Parser of (string -> Result<string, ('a * string)>)


let char expectedChar =
    let innerFn input =
        if System.String.IsNullOrEmpty input then Err "End of input"
        else if input.[0] = expectedChar then Ok (expectedChar, input.[1..])
        else sprintf "Expected %A, but got %A" expectedChar input.[0]
                |> Err
    Parser innerFn

let run (Parser func) input =
    func input


let andThen parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input
        match result1 with
            | Err msg -> Err msg
            | Ok (object1, tail) ->
                let result2 = run parser2 tail
                match result2 with
                    | Err msg -> Err msg
                    | Ok (object2, tail) ->
                        Ok ((object1, object2), tail)
    Parser innerFn

let (.>>.) = andThen



let orElse parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input

        match result1 with
            | Err err ->
                let result2 = run parser2 input

                match result2 with
                    | Err err -> Err err
                    | Ok (object', tail) -> curry Ok object' tail

            | Ok (object', tail) -> curry Ok object' tail

    Parser innerFn


let (<|>) = orElse


let map func parser =
    let innerFn input =
        let result = run parser input

        match result with
            | Err err -> Err err
            | Ok (object', tail) -> func object' => tail |> Ok // curry Ok (func object) tail

    Parser innerFn


let (|>>) parser func = map func parser


let (.>>) parser1 parser2 = parser1 .>>. parser2 |>> fst


let (>>.) parser1 parser2 = parser1 .>>. parser2 |>> snd

// let (|>) x f = f x
// let (=>) x y = x, y
// parser1 |> map Ok
// parser1 |>> Ok


let sequence listOfParsers =
    let concatResults p1 p2 =
        p1 .>>. p2 |>> uncurry (@)
    
    listOfParsers
        |> Seq.map (fun parser -> parser |>> List.singleton)
        |> Seq.reduce concatResults


/// Convert a list of chars to a string
let charListToStr charList = List.toArray charList |> System.String


let string str =
    str
        // convert to list of char
        |> Seq.map char
        // convert to Parser<char list>
        |> sequence
        // convert Parser<char list> to Parser<string>
        |> map charListToStr


// let aaa = "aaaa" |> Seq.map (sprintf "%c!")


let choice listOfParsers =
    listOfParsers |> List.reduce (<|>)


let anyOf listOfChars =
    listOfChars
        |> List.map char
        |> choice

// apply : Parser<'a -> 'b> -> Parser<'a> -> Parser<'b>
// bind : ('a -> Parser<'b>) -> Parser<'a> -> Parser<'b>


// return : 'a -> Parser<'a>  // succeed | result
let succeed x =
    let innerFn input =
        // ignore the input and return x
        Ok (x,input)
    // return the inner function
    Parser innerFn


let bind func parser =
    let innerFn input =
        let result1 = run parser input
        
        match result1 with
            | Err msg -> Err msg
            | Ok (value, tail) -> run (func value) tail

    Parser innerFn


let (>>=) parser func = bind func parser

/// (helper) match zero or more occurences of the specified parser
let rec parseZeroOrMore parser input =
    // run parser with the input
    let firstResult = run parser input 
    // test the result for Failure/Success
    match firstResult with
    | Err err -> 
        // if parse fails, return empty list
        ([],input)  
    | Ok (firstValue, inputAfterFirstParse) -> 
        // if parse succeeds, call recursively
        // to get the subsequent values
        let (subsequentValues,remainingInput) = 
            parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue::subsequentValues
        (values,remainingInput)  

/// matches zero or more occurences of the specified parser
let zeroOrMore parser = 
    let innerFn input =
        // parse the input -- wrap in Success as it always succeeds
        parseZeroOrMore parser input |> Ok

    Parser innerFn

let oneOrMore parser =
    parser
        >>= fun head ->
            zeroOrMore parser >>= fun tail -> succeed (head::tail)
                        
            
let satisfy predicate =
    let innerFn input =
        if String.IsNullOrEmpty(input) then
            Err "No more input"
        else
            let first = input.[0] 
            if predicate first then      // <====== use predicate here
                let remainingInput = input.[1..]
                Ok (first,remainingInput)
            else
                let err = sprintf "Unexpected '%c'" first
                Err err
    // return the parser
    Parser innerFn


let (>>%) p x = p |>> (fun _ -> x)


let zeroOrMoreChars cp =
    zeroOrMore cp |>> charListToStr