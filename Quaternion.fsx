type Quaternion =
    E | I | J | K

    override x.ToString() =
        match x with
        | E -> "1" | I -> "i" | J -> "j" | K -> "k"

    member x.Next =
        match x with
        | E -> E | I -> J | J -> K | K -> I

    static member (*)(a:Quaternion, b:Quaternion) =
        match a, b with
        | x, E            ->  1, x  // i1 = i
        | E, y            ->  1, y  // 1i = i
        | x, y when x = y -> -1, E  // ii = -1
        | x, y when x.Next = y      // ij = k
               -> 1, y.Next
        | x, y -> let c, z = y * x  // ji = -ij
                  -c, z

let str (a, z) =
    (if a = -1 then "-" else " ") + string z

let elems = [E; I; J; K]

for x in elems do
    elems
    |> List.map ((*) x >> str)
    |> String.concat " "
    |> printfn "%s"
