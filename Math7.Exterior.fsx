module Math7.Exterior

#load "Math7.Term.fsx"

let tests = testList()

let op   = "∧"
let vec  = Term.vec  op
let str  = Term.str  vec
let strs = Term.strs vec

tests.Add <| fun _ ->
    let e = [3; 1; 2]
    Math7.test "vec" @"\vec{e_3}∧\vec{e_1}∧\vec{e_2}"
    <| vec e
    let t = Term.fromE e
    Math7.test "str" @"\vec{e_3}∧\vec{e_1}∧\vec{e_2}"
    <| str t
    Math7.test "strs" @"\vec{e_3}∧\vec{e_1}∧\vec{e_2}+\vec{e_3}∧\vec{e_1}∧\vec{e_2}"
    <| strs [t; t]

let bsortE xs =
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

tests.Add <| fun _ ->
    let e = [3; 2; 1]
    let n, es = bsortE e
    Math7.test "bsortE" @"\vec{e_3}∧\vec{e_2}∧\vec{e_1} \to 3, \vec{e_1}∧\vec{e_2}∧\vec{e_3}"
    <| sprintf @"%s \to %d, %s" (vec e) n (vec es)
    let e = [3; 2; 3; 1]
    let n, es = bsortE e
    Math7.test "bsortE" @"\vec{e_3}∧\vec{e_2}∧\vec{e_3}∧\vec{e_1} \to 4, \vec{e_1}∧\vec{e_2}∧\vec{e_3}∧\vec{e_3}"
    <| sprintf @"%s \to %d, %s" (vec e) n (vec es)

let sort (t:term) =
    let n, e = bsortE t.E
    let s = if n % 2 = 0 then 1 else -1
    term(s * t.N, t.A, e)

tests.Add <| fun _ ->
    let t = Term.fromE [3; 2; 1]
    Math7.test "sort" @"\vec{e_3}∧\vec{e_2}∧\vec{e_1} \to -\vec{e_1}∧\vec{e_2}∧\vec{e_3}"
    <| sprintf @"%s \to %s" (str t) (str (sort t))
    let t = Term.fromE [3; 2; 3; 1]
    Math7.test "sort" @"\vec{e_3}∧\vec{e_2}∧\vec{e_3}∧\vec{e_1} \to \vec{e_1}∧\vec{e_2}∧\vec{e_3}∧\vec{e_3}"
    <| sprintf @"%s \to %s" (str t) (str (sort t))

let rec rmPairE = function
| [] -> []
| x::y::xs when x = y -> rmPairE xs
| x::xs -> x::rmPairE xs

tests.Add <| fun _ ->
    let e = [1; 1; 1; 2; 3; 3; 4]
    let vecg = Term.vec ""  // 幾何学積
    Math7.test "rmPairE" @"\vec{e_1}\vec{e_1}\vec{e_1}\vec{e_2}\vec{e_3}\vec{e_3}\vec{e_4} \to \vec{e_1}\vec{e_2}\vec{e_4}"
    <| sprintf @"%s \to %s" (vecg e) (vecg (rmPairE e))

let simplify (t:term) =
    let t = sort t
    if t.E.Length = (rmPairE t.E).Length then t else term.Zero

tests.Add <| fun _ ->
    let t = Term.fromE [3; 2; 1]
    Math7.test "simplify" @"\vec{e_3}∧\vec{e_2}∧\vec{e_1} \to -\vec{e_1}∧\vec{e_2}∧\vec{e_3}"
    <| sprintf @"%s \to %s" (str t) (str (simplify t))
    let t = Term.fromE [3; 2; 3; 1]
    Math7.test "simplify" @"\vec{e_3}∧\vec{e_2}∧\vec{e_3}∧\vec{e_1} \to 0"
    <| sprintf @"%s \to %s" (str t) (str (simplify t))

let hodge n (t:term) =
    let e = List.append (List.rev t.E) [1..n]
    let t = term(t.N, t.A, e) |> sort
    term(t.N, t.A, rmPairE t.E)

tests.Add <| fun _ ->
    let t = Term.fromE [1]
    Math7.test "hodge" @"2,\vec{e_1} \to \vec{e_2}"
    <| sprintf @"2,%s \to %s" (str t) (str (hodge 2 t))
    let t = Term.fromE [2]
    Math7.test "hodge" @"2,\vec{e_2} \to -\vec{e_1}"
    <| sprintf @"2,%s \to %s" (str t) (str (hodge 2 t))

let testHodge n =
    Math7.prologue (sprintf "## %d次元" n)
    for i = 0 to n do
        for e in Math7.combination n i do
            let t = Term.fromE e
            printfn @"\star(%s)&=%s \\" (str t) (hodge n t |> str)
    Math7.epilogue()

let showProd title n =
    Math7.prologue title
    printfn @"&\vec{a}∧\star\vec{b} \\"
    let a = [for i in [1..n] -> term(1, [sprintf "a_%d" i], [i])]
    let b = [for i in [1..n] -> term(1, [sprintf "b_%d" i], [i])]
    let sa, sb = strs a |> Term.bracket, strs b |> Term.bracket
    printfn @"&=%s∧\star%s \\" sa sb
    let al = Term.splits a
    let bl = Term.splits b |> List.map (fun (a, e) -> (a, hodge n e))
    printfn @"&=%s∧%s \\" sa (Term.strs2 vec bl |> Term.bracket)
    Term.showProd1 op vec al bl
    let d = Term.showProd2 op vec (Term.fromE >> simplify) al bl
         |> Term.simplify
         |> Term.sort id Term.byIndexSign
    let sp4 = Term.showProd3 vec ((=) 1)
    sp4 d
    d
    |> List.map (fun (e, al) ->
        let h = hodge n e
        term(h.N, @"\star"::h.A, h.E), al)
    |> sp4
    printfn @"&=\star(\vec{a}\cdot\vec{b})"
    Math7.epilogue()

if Math7.isMain() then Math7.tests "## Math7.Exterior tests" tests
