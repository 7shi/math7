type Octonion =
    E | I | J | K | H of Octonion

    override x.ToString() =
        match x with
        |   E -> "1" | I -> "i" | J -> "j" | K -> "k"
        | H E -> "h"
        | H x -> string x + "h"

    member x.Next =
        match x with
        |   I -> J
        |   J -> K
        |   K -> I
        | H x -> H x.Next
        |   x -> x

    static member (*)(a:Octonion, b:Octonion) =
        match a, b with
        |   x,   E            ->  1,   x  // i1 = i
        |   E,   y            ->  1,   y  // 1i = i
        |   x,   y when x = y -> -1,   E  // ii = -1
        | H x, H E            -> -1,   x  // (ih)h = -i
        | H x, H y            -> y * x    // (ih)(jh) = ji
        |   x, H E            ->  1, H x  // ih = (ih)
        |   x, H y when x = y -> -1, H E  // i(ih) = -h
        |   x, H y -> let c, z = y * x    // i(jh) = (ji)h
                      c, H z
        |   x,   y when x.Next = y        // ij = k
                   -> 1, y.Next
        |   x,   y -> let c, z = y * x    // ji = -ij
                      -c, z

let str (a, z) =
    (if a = -1 then "-" else " ") + string z
    |> sprintf "%-3s"

let elems = [E; I; J; K; H E; H I; H J; H K]

for x in elems do
    elems
    |> List.map ((*) x >> str)
    |> String.concat " "
    |> printfn "%s"
