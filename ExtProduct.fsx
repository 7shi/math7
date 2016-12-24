#load "Math7.Term.fsx"
open Math7

Term.showProd "## 代数式" ""
    (Seq.map (fun e -> [""; "x"; "y"; "z"].[e]) >> Term.strPower)
    (function
    | [x; y] when y % 3 + 1 = x -> Term.fromE [y; x]
    | es -> Term.fromE es)
    (function
    | [x; y] as xs when x = y -> (-2, xs)
    |           xs            -> (-1, xs))
    Term.byIndexSign ((=) 3)
    [for i in [1..3] -> term(1, [sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term(1, [sprintf "b_%d" i], [i])]

Term.showProd "## テンソル積" @"\otimes "
    (Term.vec @"\otimes ")
    Term.fromE id Term.byIndexSign (fun _ -> false)
    [for i in [1..3] -> term(1, [sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term(1, [sprintf "b_%d" i], [i])]

Term.showProd "## ウェッジ積" "∧"
    (Term.vec "∧")
    (function
    | [x; y] when x = y -> term.Zero
    | [x; y] when y % 3 + 1 = x -> term(-1, [], [y; x])
    | es -> Term.fromE es)
    id Term.byIndexSign (fun _ -> false)
    [for i in [1..3] -> term(1, [sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term(1, [sprintf "b_%d" i], [i])]

Term.showProd "## ベクトル積" "×"
    (Term.vec "×")
    (function
    | [x; y] when x = y -> term.Zero
    | [x; y] when x % 3 + 1 = y -> term( 1, [], [y % 3 + 1])
    | [x; y] when y % 3 + 1 = x -> term(-1, [], [x % 3 + 1])
    | es -> Term.fromE es)
    id Term.byIndexSign (fun _ -> false)
    [for i in [1..3] -> term(1, [sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term(1, [sprintf "b_%d" i], [i])]

Term.showProd "## 幾何学積" ""
    (Term.vec "")
    (function
    | [x; y] when x = y -> term.One
    | [x; y] when y % 3 + 1 = x -> term(-1, [], [y; x])
    | es -> Term.fromE es)
    id Term.byIndexSign ((=) 1)
    [for i in [1..3] -> term(1, [sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term(1, [sprintf "b_%d" i], [i])]

Term.showProd "## 四元数" ""
    (Seq.map (fun e -> [""; "i"; "j"; "k"].[e]) >> Term.strPower)
    (function
    | [x; y] when x = y -> term -1
    | [x; y] when x % 3 + 1 = y -> term( 1, [], [y % 3 + 1])
    | [y; x] when x % 3 + 1 = y -> term(-1, [], [y % 3 + 1])
    | es -> Term.fromE es)
    id Term.byIndexSign ((=) 1)
    [for i in [1..3] -> term(1, [sprintf "a_%d" i], [i])]
    [for i in [1..3] -> term(1, [sprintf "b_%d" i], [i])]
