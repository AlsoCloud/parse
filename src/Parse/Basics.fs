[<AutoOpen>]
module Basics


let (=>) x y = x, y


let flip func x y = func y x


let uncurry func (x, y) = func x y


let curry func a b = a => b |> func