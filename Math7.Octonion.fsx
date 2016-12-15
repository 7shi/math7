namespace Math7

#load "Math7.fsx"

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
    let octs = [| E; I; J; K; H E; H I; H J; H K |]
    let oct x = octs.[x]
    let ijkh = function
    | 0 -> ""
    | x -> string (oct x)

    let str es = es |> Seq.map ijkh |> Term.strPower

    let prod es =
        let n, e = es |> Seq.fold (fun (n, x) y ->
            let m, z = oct x * oct y
            n * m, z.N) (1, E.N)
        term(n, [], if e = E.N then [] else [e])

    let conj = List.map <| fun (t:term) ->
        if t.E.IsEmpty then t else -1 * t
