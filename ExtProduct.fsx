#load "Math7.Term.fsx"
open Math7

Term.showProd "## 代数式" ""
    (fun es -> es |> Seq.map (fun e -> [""; "x"; "y"; "z"].[e]) |> Term.strPower)
    (fun es ->
        match es with
        | [2; 1] | [3; 2] | [1; 3] -> term(1, [], List.rev es)
        | _ -> term(1, [], es))
    (fun xs ->
        match xs with
        | [x; y] when x = y -> (-2, xs)
        | _                 -> (-1, xs))
    Term.byIndexSign ((=) 3)
    [for i in [1..3] -> term(1, [sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term(1, [sprintf "b_%d" i], [i])]

Term.showProd "## テンソル積" @"\otimes "
    (Term.vec @"\otimes ")
    (fun es -> term(1, [], es))
    id Term.byIndexSign (fun _ -> false)
    [for i in [1..3] -> term(1, [sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term(1, [sprintf "b_%d" i], [i])]

Term.showProd "## ウェッジ積" "∧"
    (Term.vec "∧")
    (fun es ->
        match es with
        | [x; y] when x = y -> term.Zero
        | [2; 1] | [3; 2] | [1; 3] -> term(-1, [], List.rev es)
        | _ -> term(1, [], es))
    id Term.byIndexSign (fun _ -> false)
    [for i in [1..3] -> term(1, [sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term(1, [sprintf "b_%d" i], [i])]

Term.showProd "## ベクトル積" "×"
    (Term.vec "×")
    (fun es ->
        match es with
        | [x; y] when x = y -> term.Zero
        | [1; 2] -> term( 1, [], [3]) | [2; 3] -> term( 1, [], [1]) | [3; 1] -> term( 1, [], [2])
        | [2; 1] -> term(-1, [], [3]) | [3; 2] -> term(-1, [], [1]) | [1; 3] -> term(-1, [], [2])
        | _ -> term(1, [], es))
    id Term.byIndexSign (fun _ -> false)
    [for i in [1..3] -> term(1, [sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term(1, [sprintf "b_%d" i], [i])]

Term.showProd "## 幾何学積" ""
    (Term.vec "")
    (fun es ->
        match es with
        | [x; y] when x = y -> term.One
        | [2; 1] | [3; 2] | [1; 3] -> term(-1, [], List.rev es)
        | _ -> term(1, [], es))
    id Term.byIndexSign ((=) 1)
    [for i in [1..3] -> term(1, [sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term(1, [sprintf "b_%d" i], [i])]

Term.showProd "## 四元数" ""
    (fun es -> es |> Seq.map (fun e -> [""; "i"; "j"; "k"].[e]) |> Term.strPower)
    (fun es ->
        match es with
        | [x; y] when x = y -> term -1
        | [1; 2] -> term( 1, [], [3]) | [2; 3] -> term( 1, [], [1]) | [3; 1] -> term( 1, [], [2])
        | [2; 1] -> term(-1, [], [3]) | [3; 2] -> term(-1, [], [1]) | [1; 3] -> term(-1, [], [2])
        | _ -> term(1, [], es))
    id Term.byIndexSign ((=) 1)
    [for i in [1..3] -> term(1, [sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term(1, [sprintf "b_%d" i], [i])]
