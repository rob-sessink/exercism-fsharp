module Accumulate

// based upon https://blog.ploeh.dk/2015/12/22/tail-recurse/

let accumulate' (func: 'a -> 'b) (input: 'a list): 'b list =
    let cons x input = x :: input

    let rec map func acc =
        function
        | [] -> acc []
        | head :: tail -> map func (acc << (cons (func head))) tail

    map func id input

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list =

    let rec map input func acc =
        match input with
        | [] -> acc
        | head :: tail -> map tail func ((func head) :: acc)

    map input func [] |> List.rev
