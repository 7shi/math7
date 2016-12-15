open System

let matrix m =
    printfn @"\left(\begin{matrix}"
    m |> Seq.map (String.concat " & ")
      |> String.concat (@" \\" + Environment.NewLine)
      |> printfn "%s"
    printfn @"\end{matrix}\right)"

let v = [["a"]; ["b"]; ["c"]]
let w = [["d" ;  "e" ;  "f"]]

matrix v
matrix w
printfn "="
v
|> Seq.mapi (fun i r ->
    w.[0] |> Seq.mapi (fun j c ->
        if i = j then "内" else "外"
        |> sprintf @"\underbrace{%s%s}_{%s}" r.[0] c))
|> matrix
