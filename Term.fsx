let str (a, x) =
    if x = "" then a.ToString() else
    let sa = match a with 1 -> "" | -1 -> "-" | _ -> a.ToString()
    x.ToCharArray()
    |> Seq.groupBy id
    |> Seq.map (fun (e, xs) ->
        match Seq.length xs with
        | 1 -> e.ToString()
        | n -> sprintf "%c^%d" e n)
    |> String.concat ""
    |> (+) sa

printfn "%s" (str ( 3,"" ))
printfn "%s" (str ( 1,"x"))
printfn "%s" (str ( 2,"y"))
printfn "%s" (str (-1,"y"))
printfn "%s" (str (-2,"z"))
printfn "%s" (str (1,"xx"))

let strs ts =
    ts
    |> List.mapi (fun i (a, _ as t) ->
        if i > 0 && a >= 0 then "+" + str t else str t)
    |> String.concat ""

printfn "%s" (strs [2,"x"; 3,"y"; -1, "z"])

let prod (ts:(int * string) list) =
    ts |> Seq.reduce (fun (a, x) (b, y) -> a * b, x + y)

printfn "%s" (prod [2,"x"; 3,"y"] |> str)

let prods ts =
    ts
    |> Seq.reduce (fun al bl ->
        [for a in al do for b in bl -> prod [a; b]])

let a, b = [1,"x"; 2,"" ], [1,"x"; 3,"" ]
let c, d = [1,"x"; 2,"y"], [1,"x"; 3,"y"]
printfn "(%s)(%s)=%s" (strs a) (strs b) (prods [a; b] |> strs)
printfn "(%s)(%s)=%s" (strs c) (strs d) (prods [c; d] |> strs)

let simplify (ts:(int * string) list) =
    ts
    |> Seq.groupBy (fun (a, x) -> System.String(x.ToCharArray() |> Array.sort))
    |> Seq.map (fun (a, xl) -> xl |> Seq.map fst |> Seq.sum, a)
    |> Seq.toList

printfn "(%s)(%s)=%s" (strs a) (strs b) (prods [a; b] |> simplify |> strs)
printfn "(%s)(%s)=%s" (strs c) (strs d) (prods [c; d] |> simplify |> strs)

printfn ""

printfn @"\begin{align}"
let f = [1,"x"; 1,"y"]
for n = 2 to 8 do
    let ff = List.replicate n f |> prods |> simplify
    printfn @"(%s)^%d&=%s \\" (strs f) n (strs ff)
printfn @"\end{align}"
