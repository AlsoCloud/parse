#load "Basics.fs"
#load "Result.fs"
#load "Parse.fs"

open Parse

let input = "\"akBba\""
let objectOpen = Parse.char '{'
let stringQuote = Parse.char '"'
let lowerCaseLetters = Parse.anyOf ['a'..'Z']
let string = Parse.zeroOrMore lowerCaseLetters
let quotedString = stringQuote >>. string .>> stringQuote |>> Parse.charListToStr
let parserObjectEnd = Parse.char '}'



Parse.run quotedString input
t