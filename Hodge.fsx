#load "Math7.fsx"
open Math7

let vec = Term.vec "∧"
let str (a, e) = Term.str2 vec (a, e)
let strs = Term.strs2 vec

let bsort xs =
    let rec bswap n = function
        | []    -> n, []
        | x::xs ->
            match bswap n xs with
            | n, [] -> n, [x]
            | n, y::ys when x > y -> n + 1, y::x::ys
            | n, y::ys            -> n    , x::y::ys
    let rec bsort n xs =
        match bswap n xs with
        | n, []    -> n, []
        | n, y::ys -> let n, ys = bsort n ys
                      n, y::ys
    bsort 0 xs

let sort (t:term) =
    let n, e = bsort t.E
    let s = if n % 2 = 0 then 1 else -1
    term(s * t.N, t.A, e)

let rmPair (t:term) =
    let e =
        t.E
        |> Seq.groupBy id
        |> Seq.filter (snd >> Seq.length >> (fun x -> x % 2 = 1))
        |> Seq.map fst
        |> Seq.toList
    term(t.N, t.A, e)

let simplify (t:term) =
    let len = Seq.length (rmPair t).E
    if Seq.length t.E = len then sort t else term.Zero

let simplifyE e = term(1, [], e) |> simplify

let hodge n (t:term) =
    term(t.N, t.A, List.append (List.rev t.E) [1..n]) |> sort |> rmPair
let hodges n = List.map (fun (e, al) -> hodge n e, al)

let n = 4
printfn @"\begin{align}"
printfn @"&\vec{a}∧(\star\vec{b}) \\"
let a = Term.splits [for i in [1..n] -> term(1, [sprintf "a_%d" i], [i])]
let b = Term.splits [for i in [1..n] -> term(1, [sprintf "b_%d" i], [i])]
let sa, sb = strs a |> Term.bracket, strs b |> Term.bracket
printfn @"&=%s∧\star%s \\" sa sb
let hb = b |> List.map (fun (a, e) -> (a, hodge n e))
printfn @"&=%s∧\star%s \\" sa (strs hb |> Term.bracket)
let _, d = Term.prodTerms1 "∧" vec simplifyE id a hb
let pt = Term.prodTerms2 vec Term.byIndexSign ((=) 1)
pt "" d
hodges n d |> pt "\star"
printfn @"&=\star(\vec{a}\cdot\vec{b})"
printfn @"\end{align}"
