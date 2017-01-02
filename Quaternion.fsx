#load "Math7.Term.fsx"
open Math7

printfn "# 複素数"
let compStr = Seq.map (fun e -> [| ""; "i" |].[e]) >> Term.strPower
let compProd title =
    Term.showProd title "" compStr
        (function
        | [x; y] when x = y -> term -1
        | es -> Term.fromE es)
        id Term.byIndexSign (fun _ -> true)
let ca = [term(1, ["a_0"], []); term(1, ["a_1"], [1])]
let cb = [term(1, ["b_0"], []); term(1, ["b_1"], [1])]
compProd "## 積" ca cb
Math7.prologue "## 共役"
let conj (ts:term list) =
    ts |> List.map (fun t -> if t.E = [] then t else -1 * t)
printfn "(%s)^*&=%s" (Term.strs compStr ca) (conj ca |> Term.strs compStr)
Math7.epilogue()
compProd "## ノルム" (conj ca) ca

printfn ""
printfn "# 三元数"
Term.showProd "## 積" ""
    (Seq.map (fun e -> [""; "i"; "j"].[e]) >> Term.strPower)
    (function
    | [x; y] when x = y -> term -1
    | [2; 1] -> term(1, [], [1; 2])
    | es -> Term.fromE es)
    id Term.byIndexSign (fun _ -> true)
    (term(1, ["a_0"], [])::[for i in [1..2] -> term(1, [sprintf "a_%d" i], [i])])
    (term(1, ["b_0"], [])::[for i in [1..2] -> term(1, [sprintf "b_%d" i], [i])])

printfn ""
printfn "# 双複素数"
let ijk = Seq.map (fun e -> [""; "i"; "j"; "k"].[e]) >> Term.strPower
let dcompProdE = function
| [1; 1] | [2; 2] -> term -1            // ii=jj=-1
| [3; 3] -> term.One                    // kk=1
| [1; 2] | [2; 1] -> term( 1, [], [3])  // ij=ji=k
| [2; 3] | [3; 2] -> term(-1, [], [1])  // jk=kj=-i
| [3; 1] | [1; 3] -> term(-1, [], [2])  // ki=ik=-j
| es -> Term.fromE es
let dcompProd title =
    Term.showProd title "" ijk dcompProdE id Term.byIndexSign (fun _ -> true)
let qa = term(1, ["a_0"], [])::[for i in [1..3] -> term(1, [sprintf "a_%d" i], [i])]
let qb = term(1, ["b_0"], [])::[for i in [1..3] -> term(1, [sprintf "b_%d" i], [i])]
dcompProd "## 積" qa qb
Math7.prologue "## 共役"
let dcompConj (ts:term list) =
    ts |> List.map (fun t -> match t.E with [2] | [3] -> -1 * t | _ -> t)
printfn "(%s)^*&=%s" (Term.strs ijk qa) (dcompConj qa |> Term.strs ijk)
Math7.epilogue()
dcompProd "## ノルム" (dcompConj qa) qa
Math7.prologue "### 確認"
let dcompProd2 a b =
    let sa = Term.strs ijk a |> Term.bracket
    let sb = Term.strs ijk b |> Term.bracket
    if sa = sb then printf @"&%s^2=" sa else printf @"&%s%s=" sa sb
    Term.prods dcompProdE a b
    |> Term.simplify
    |> Term.sort id Term.byIndexSign
    |> List.iteri (fun i d1 ->
        let s = Term.str3 ijk d1
        printf "%s" <| if i = 0 then s else Term.addSign s)
    printfn @" \\"
for x in [1; -1] do
    for y in [1; -1] do
        for z in [1; -1] do
            let a = [term(1, ["a_0"], [ ])
                     term(x, ["a_1"], [1])
                     term(y, ["a_2"], [2])
                     term(z, ["a_3"], [3])]
            dcompProd2 a qa
Math7.epilogue()

printfn ""
printfn "# テッサリン"
let tessProdE = function
| [1; 1] | [3; 3] -> term -1            // ii=kk=-1
| [2; 2] -> term.One                    // jj=1
| [1; 2] | [2; 1] -> term( 1, [], [3])  // ij=ji=k
| [2; 3] | [3; 2] -> term( 1, [], [1])  // jk=kj=i
| [3; 1] | [1; 3] -> term(-1, [], [2])  // ki=ik=-j
| es -> Term.fromE es
let tessProd title =
    Term.showProd title "" ijk tessProdE id Term.byIndexSign (fun _ -> true)
tessProd "## 積" qa qb
tessProd "## ノルム" (dcompConj qa) qa

printfn ""
printfn "# 分解型複素数"
let spca = [term(1, ["a_0"], []); term(1, ["a_1"], [2])]
let spcb = [term(1, ["b_0"], []); term(1, ["b_1"], [2])]
tessProd "## 積" spca spcb
tessProd "## ノルム" (dcompConj spca) spca

printfn ""
printfn "# 四元数"
printfn ""
printfn "## 乗積表"
printfn ""
let quatProdE n = function
| [x; y] when x = y -> term n
| [x; y] when x % 3 + 1 = y -> term( 1, [], [y % 3 + 1])
| [y; x] when x % 3 + 1 = y -> term(-1, [], [y % 3 + 1])
| es -> Term.fromE es
for x in {0..3} do
    seq {for y in {0..3} -> [x; y] |> List.filter ((<>) 0)}
    |> Seq.map (quatProdE -1 >> Term.str ijk >> sprintf "%2s")
    |> String.concat " "
    |> printfn "%s"
let quatProd title =
    Term.showProd title "" ijk (quatProdE -1) id Term.byIndexSign (fun _ -> true)
quatProd "## 積（ベクトル）" (List.tail qa) (List.tail qb)
quatProd "## 積" qa qb
quatProd "## ノルム" (conj qa) qa

printfn ""
printfn "# 双曲四元数"
let hquatProd title =
    Term.showProd title "" ijk (quatProdE 1) id Term.byIndexSign (fun _ -> true)
hquatProd "## 積" qa qb
hquatProd "## ノルム" (conj qa) qa
