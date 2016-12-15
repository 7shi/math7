#load "Math7.fsx"
open Math7

Term.prodTerms "## 代数式" ""
    (fun es -> es |> Seq.map (fun e -> [""; "x"; "y"; "z"].[e]) |> Term.strPower)
    (fun xs ->
        match xs with
        | [2; 1] | [3; 2] | [1; 3] -> term(List.rev xs)
        | _ -> term xs)
    (fun xs ->
        match xs with
        | [x; y] when x = y -> (-2, xs)
        | _                 -> (-1, xs))
    Term.byIndexSign ((=) 3)
    [for i in [1..3] -> term([sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term([sprintf "b_%d" i], [i])]

Term.prodTerms "## テンソル積" @"\otimes "
    (Term.vec @"\otimes ")
    (fun xs -> term xs)
    id Term.byIndexSign (fun _ -> false)
    [for i in [1..3] -> term([sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term([sprintf "b_%d" i], [i])]

Term.prodTerms "## ウェッジ積" "∧"
    (Term.vec "∧")
    (fun xs ->
        match xs with
        | [x; y] when x = y -> term.Zero
        | [2; 1] | [3; 2] | [1; 3] -> term(-1, List.rev xs)
        | _ -> term xs)
    id Term.byIndexSign (fun _ -> false)
    [for i in [1..3] -> term([sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term([sprintf "b_%d" i], [i])]

Term.prodTerms "## ベクトル積" "×"
    (Term.vec "×")
    (fun xs ->
        match xs with
        | [x; y] when x = y -> term.Zero
        | [1; 2] -> term( 1, [3]) | [2; 3] -> term( 1, [1]) | [3; 1] -> term( 1, [2])
        | [2; 1] -> term(-1, [3]) | [3; 2] -> term(-1, [1]) | [1; 3] -> term(-1, [2])
        | _ -> term xs)
    id Term.byIndexSign (fun _ -> false)
    [for i in [1..3] -> term([sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term([sprintf "b_%d" i], [i])]

Term.prodTerms "## 幾何学積" ""
    (Term.vec "")
    (fun xs ->
        match xs with
        | [x; y] when x = y -> term.One
        | [2; 1] | [3; 2] | [1; 3] -> term(-1, List.rev xs)
        | _ -> term xs)
    id Term.byIndexSign ((=) 1)
    [for i in [1..3] -> term([sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term([sprintf "b_%d" i], [i])]

Term.prodTerms "## 四元数" ""
    (fun es -> es |> Seq.map (fun e -> [""; "i"; "j"; "k"].[e]) |> Term.strPower)
    (fun xs ->
        match xs with
        | [x; y] when x = y -> term -1
        | [1; 2] -> term( 1, [3]) | [2; 3] -> term( 1, [1]) | [3; 1] -> term( 1, [2])
        | [2; 1] -> term(-1, [3]) | [3; 2] -> term(-1, [1]) | [1; 3] -> term(-1, [2])
        | _ -> term xs)
    id Term.byIndexSign ((=) 1)
    [for i in [1..3] -> term([sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term([sprintf "b_%d" i], [i])]
