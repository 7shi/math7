#load "Math7.Term.fsx"
#load "Math7.Octonion.fsx"
#load "Math7.Exterior.fsx"
open Math7

let showProd op f g sort1 sort2 filter nl a b =
    let sa = Term.strs f a |> Term.bracket
    let sb = Term.strs f b |> Term.bracket
    printfn @"&%s%s%s \\" sa op sb
    Term.prods g a b
    |> Term.simplify
    |> Term.sort sort1 sort2
    |> List.filter filter
    |> Term.showProd3 f nl

for n = 1 to 7 do
    Math7.prologue (sprintf "## %d次元" n)
    let a = [for i in [1..n] -> term(1, [sprintf "a_%d" i], [i])]
    let b = [for i in [1..n] -> term(1, [sprintf "b_%d" i], [i])]
    showProd "∧" (Term.vec "∧") (Term.fromE >> Exterior.simplify)
        id Term.byIndexSign (fun _ -> true) (fun x -> x % 3 = 0) a b
    showProd "×" (Term.vec "×") Octonion.prod id Term.byIndexSign
        (fun (e, _) -> e.E <> []) (fun _ -> true) a b
    Math7.epilogue()
