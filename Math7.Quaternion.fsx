module Math7.Quaternion

#load "Math7.Term.fsx"
open Math7

let compStr = Seq.map (fun e -> [| ""; "i" |].[e]) >> Term.strPower
let compProd title =
    Term.showProd title "" compStr
        (function
        | [x; y] when x = y -> term -1
        | es -> Term.fromE es)
        id Term.byIndexSign (fun _ -> true)
let ca = [term(1, ["a_0"], []); term(1, ["a_1"], [1])]
let cb = [term(1, ["b_0"], []); term(1, ["b_1"], [1])]
let conj (ts:term list) =
    ts |> List.map (fun t -> if t.E = [] then t else -1 * t)
if Math7.isMain() then
    printfn "# 複素数"
    compProd "## 積" ca cb
    Math7.prologue "## 共役"
    printfn "(%s)^*&=%s" (Term.strs compStr ca) (conj ca |> Term.strs compStr)
    Math7.epilogue()
    compProd "## ノルム" (conj ca) ca

let triStr = Seq.map (fun e -> [""; "i"; "j"].[e]) >> Term.strPower
let triProd title =
    Term.showProd title "" triStr
        (function
        | [x; y] when x = y -> term -1
        | [2; 1] -> term(1, [], [1; 2])
        | es -> Term.fromE es)
        id Term.byIndexSign (fun _ -> true)
let ta = (term(1, ["a_0"], [])::[for i in [1..2] -> term(1, [sprintf "a_%d" i], [i])])
let tb = (term(1, ["b_0"], [])::[for i in [1..2] -> term(1, [sprintf "b_%d" i], [i])])
if Math7.isMain() then
    printfn ""
    printfn "# 三元数"
    triProd "## 積" ta tb

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
let dcompConj (ts:term list) =
    ts |> List.map (fun t -> match t.E with [1] | [2] -> -1 * t | _ -> t)
let showProd f g a b =
    let sa = Term.strs f a |> Term.bracket
    let sb = Term.strs f b |> Term.bracket
    if sa = sb then printf @"&%s^2=" sa else printf @"&%s%s=" sa sb
    let d =
        Term.prods g a b
        |> Term.simplify
        |> Term.sort id Term.byIndexSign
    d |> List.iteri (fun i d1 ->
        let s = Term.str3 f d1
        let s = if d.Length = 1 then Term.unbracket s else s
        printf "%s" <| if i = 0 then s else Term.addSign s)
    printfn @" \\"
if Math7.isMain() then
    printfn ""
    printfn "# 双複素数"
    dcompProd "## 積" qa qb
    Math7.prologue "## 共役"
    printfn "(%s)^*&=%s" (Term.strs ijk qa) (dcompConj qa |> Term.strs ijk)
    Math7.epilogue()
    dcompProd "## ノルム" (dcompConj qa) qa
    Math7.prologue "### 確認"
    for x in [1; -1] do
        for y in [1; -1] do
            for z in [1; -1] do
                let a = [term(1, ["a_0"], [ ])
                         term(x, ["a_1"], [1])
                         term(y, ["a_2"], [2])
                         term(z, ["a_3"], [3])]
                showProd ijk dcompProdE a qa
    Math7.epilogue()

let tessProdE = function
| [1; 1] | [3; 3] -> term -1            // ii=kk=-1
| [2; 2] -> term.One                    // jj=1
| [1; 2] | [2; 1] -> term( 1, [], [3])  // ij=ji=k
| [2; 3] | [3; 2] -> term( 1, [], [1])  // jk=kj=i
| [3; 1] | [1; 3] -> term(-1, [], [2])  // ki=ik=-j
| es -> Term.fromE es
let tessProd title =
    Term.showProd title "" ijk tessProdE id Term.byIndexSign (fun _ -> true)
let tessConj (ts:term list) =
    ts |> List.map (fun t -> match t.E with [1] | [3] -> -1 * t | _ -> t)
if Math7.isMain() then
    printfn ""
    printfn "# テッサリン"
    tessProd "## 積" qa qb
    tessProd "## ノルム" (tessConj qa) qa

let spca = [term(1, ["a_0"], []); term(1, ["a_1"], [2])]
let spcb = [term(1, ["b_0"], []); term(1, ["b_1"], [2])]
if Math7.isMain() then
    printfn ""
    printfn "# 分解型複素数"
    tessProd "## 積" spca spcb
    tessProd "## ノルム" (dcompConj spca) spca

let quatProdE n = function
| [x; y] when x = y -> term n
| [x; y] when x % 3 + 1 = y -> term( 1, [], [y % 3 + 1])
| [y; x] when x % 3 + 1 = y -> term(-1, [], [y % 3 + 1])
| es -> Term.fromE es
let quatProd title =
    Term.showProd title "" ijk (quatProdE -1) id Term.byIndexSign (fun _ -> true)
if Math7.isMain() then
    printfn ""
    printfn "# 四元数"
    printfn ""
    printfn "## 乗積表"
    printfn ""
    for x in {0..3} do
        seq {for y in {0..3} -> [x; y] |> List.filter ((<>) 0)}
        |> Seq.map (quatProdE -1 >> Term.str ijk >> sprintf "%2s")
        |> String.concat " "
        |> printfn "%s"
    quatProd "## 積（ベクトル）" (List.tail qa) (List.tail qb)
    quatProd "## 積" qa qb
    quatProd "## ノルム" (conj qa) qa

let hquatProd title =
    Term.showProd title "" ijk (quatProdE 1) id Term.byIndexSign (fun _ -> true)
if Math7.isMain() then
    printfn ""
    printfn "# 双曲四元数"
    hquatProd "## 積" qa qb
    hquatProd "## ノルム" (conj qa) qa

let quat a b c d = [term(a, [], []); term(b, [], [1]); term(c, [], [2]); term(d, [], [3])]
if Math7.isMain() then
    printfn ""
    printfn "# 共役との積"
    let qa, qb = quat 1 2 3 4, quat 5 6 7 8
    quatProd "## 四元数" (conj qa) qb
    dcompProd "## 双複素数" (dcompConj qa) qb
    tessProd "## テッサリン" (tessConj qa) qb

    let q1, q2, q3, q4 = quat 1 2 3 4, quat 5 6 7 8, quat 9 10 11 12, quat 13 14 15 16
    quatProd "## 四元数1" (conj q1) q3
    quatProd "## 四元数2" (conj q2) q3
    quatProd "## 四元数3" (conj q1) q4
    quatProd "## 四元数4" (conj q2) q4
