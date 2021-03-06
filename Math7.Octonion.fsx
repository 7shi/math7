namespace Math7

#load "Math7.Term.fsx"

type octonion =
    E | I | J | K | H of octonion

    override x.ToString() =
        match x with
        |   E -> "1" | I -> "i" | J -> "j" | K -> "k"
        | H E -> "h"
        | H x -> string x + "_h"

    member x.N =
        match x with
        | E -> 0 | I -> 1 | J -> 2 | K -> 3
        | H x -> x.N + 4

    member x.Next =
        match x with
        |   I -> J
        |   J -> K
        |   K -> I
        | H x -> H x.Next
        |   x -> x

    static member (*)(a:octonion, b:octonion) =
        match a, b with
        |   x,   E            ->  1,   x  // i1 = i
        |   E,   y            ->  1,   y  // 1i = i
        |   x,   y when x = y -> -1,   E  // ii = -1
        | H x, H E            -> -1,   x  // i_hh = -i
        | H x, H y            -> y * x    // i_hj_h = ji
        |   x, H E            ->  1, H x  // ih = i_h
        |   x, H y when x = y -> -1, H E  // ii_h = -h
        |   x, H y -> let c, z = y * x    // ij_h = (ji)h
                      c, H z
        |   x,   y when x.Next = y        // ij = k
                   -> 1, y.Next
        |   x,   y -> let c, z = y * x    // ji = -ij
                      -c, z

module Octonion =
    let tests = testList()
    
    let octs = [| E; I; J; K; H E; H I; H J; H K |]
    let oct x = octs.[x]
    let ijkh = function
    | 0 -> ""
    | x -> string (oct x)

    let str es = es |> Seq.map ijkh |> Seq.filter ((<>) "") |> Term.strPower

    let prod es =
        let n, e = es |> Seq.fold (fun (n, x) y ->
            let m, z = oct x * oct y
            n * m, z.N) (1, E.N)
        term(n, [], if e = E.N then [] else [e])

    let conj = List.map <| fun (t:term) ->
        if t.E.IsEmpty then t else -1 * t

    let showProd title =
        Term.showProd title "" str prod id Term.byIndexSign

    tests.Add <| fun _ ->
        let str2 = Term.str2 str
        let e = Term.fromE [E.N]
        let a = Term.split e
        Math7.test "Term.strProd" @"(1)^2 \to 1"
        <| sprintf @"(%s)^2 \to %s" (str2 a) (Term.strProd "" str prod a a |> fst)
        let i = Term.fromE [I.N]
        let a = Term.split i
        Math7.test "Term.strProd" @"(i)^2 \to \underbrace{i^2}_{-1}"
        <| sprintf @"(%s)^2 \to %s" (str2 a) (Term.strProd "" str prod a a |> fst)
        let a0 = term(1, ["a_0"], [])
        let a = a0, e
        Math7.test "Term.strProd" @"(a_0)^2 \to a_0^2"
        <| sprintf @"(%s)^2 \to %s" (str2 a) (Term.strProd "" str prod a a |> fst)
        let a0i = Term.split (term(1, ["a_0"], [I.N]))
        let a = a0, i
        Math7.test "Term.strProd" @"(a_0i)^2 \to a_0^2\underbrace{i^2}_{-1}"
        <| sprintf @"(%s)^2 \to %s" (str2 a) (Term.strProd "" str prod a a |> fst)
        Math7.test "Term.str3" @"(a_0+a_0)"
        <| sprintf @"%s" (Term.str3 str (e, [a0; a0]))
        Math7.test "Term.str3" @"(a_0+a_0)i"
        <| sprintf @"%s" (Term.str3 str (i, [a0; a0]))

    if Math7.isMain() then Math7.tests "## Math7.Octonion tests" tests
