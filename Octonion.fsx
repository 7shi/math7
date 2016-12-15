type Octonion =
    E | I | J | K | H of Octonion

    member x.Next =
        match x with
        |   I -> J
        |   J -> K
        |   K -> I
        | H x -> H x.Next
        |   x -> x

    override x.ToString() =
        match x with
        |   E -> "1" | I -> "i" | J -> "j" | K -> "k"
        | H E -> "h"
        | H x -> string x + "_h"

    static member (*) (a:Octonion, b:Octonion) =
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

let tostr (a, z) =
    let s = if z = H E then "  h" else string z
    sprintf "%s%-3s" (if a = -1 then "-" else " ") s

let elems = [| I; J; K; H E; H I; H J; H K |]

printf "    "
for x in elems do
    printf "|%s" (tostr (E * x))
printfn ""
printfn "%s" (String.replicate (4 * 8 + 7) "-")
for x in elems do
    printf "%s" (tostr (E * x))
    for y in elems do
        printf "|%s" (tostr (x * y))
    printfn ""
