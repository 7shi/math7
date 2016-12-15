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

let tostr (a, z) =
    (if a = -1 then "-" else " ") + string z

let elems = [I; J; K]

printf "  "
for x in elems do
    printf "|%s" (tostr (E * x))
printfn ""
printfn "%s" (String.replicate (2 * 4 + 3) "-")
for x in elems do
    printf "%s" (tostr (E * x))
    for y in elems do
        printf "|%s" (tostr (x * y))
    printfn ""
