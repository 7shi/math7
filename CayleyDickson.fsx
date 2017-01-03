#load "Math7.Term.fsx"
#load "Math7.Quaternion.fsx"
#load "Math7.Octonion.fsx"
open Math7

let t a e = term(1, [a], e)
let qProdE = Quaternion.quatProdE -1

Math7.prologue "# 複素数"
Quaternion.showProd Quaternion.compStr qProdE
    [t"a"[];t"b"[1]] [t"c"[];t"d"[1]]
Math7.epilogue()

let show pfx h ts =
    printf "%s" pfx
    Term.simplify ts
    |> Term.sort id (fun t -> String.concat "" t.A)
    |> Term.showProd3Raw Quaternion.ijk h
let showEq nl = show "&=" (fun _ -> nl)
let str = Seq.map Octonion.ijkh >> Term.strPower
let strs = Term.strs str
let dcompProds = Term.prods Quaternion.dcompProdE
let classify:term list -> unit =
    Seq.groupBy (fun t ->
        t.A |> Seq.map (fun s -> s.[..0]) |> String.concat "")
    >> Seq.iter (fun (t, ts) ->
        show (t + "&:") (fun _ -> false) ts)

Math7.prologue "# 双複素数"
printfn @"&(a+bj)(c+dj) \\"
let a = [t"a_0"[];t"a_1"[1]]
let b = [t"b_0"[];t"b_1"[1]]
let c = [t"c_0"[];t"c_1"[1]]
let d = [t"d_0"[];t"d_1"[1]]
printfn @"&=\{(%s)+(%s)j\}\{(%s)+(%s)j\} \\"
    (strs a) (strs b) (strs c) (strs d)
let j = Term.fromE [2]
let q1 = List.append a (dcompProds b [j])
let q2 = List.append c (dcompProds d [j])
printfn @"&=(%s)(%s) \\" (strs q1) (strs q2)
let q3dc = dcompProds q1 q2 in showEq true q3dc
Math7.epilogue()
Math7.prologue "## 分類"
classify q3dc
Math7.epilogue()
Math7.prologue "## 比較"
dcompProds a c |> show "ac&=" (fun _ -> false)
dcompProds a d |> show "ad&=" (fun _ -> false)
dcompProds b c |> show "bc&=" (fun _ -> false)
dcompProds b d |> show "bd&=" (fun _ -> false)
Math7.epilogue()

let prods = Term.prods Octonion.prod
let tstr =
    Term.simplify
    >> Seq.mapi (fun i dl ->
        let s = Term.str3 str dl
        if i = 0 then s else Term.addSign s)
    >> String.concat ""
    >> function "" -> "0" | s -> s
let ijkstr z (re, im) =
    List.append re (prods im [z]) |> tstr
let tupleProd z ((a, b as x), (c, d as y)) =
    let sa, sb, sc, sd = strs a, strs b, strs c, strs d
    printf @"%s%s&=(%s,%s)(%s,%s)"
        (ijkstr z x) (ijkstr z y) sa sb sc sd
    printf @"=(%s%s-%s^* %s,%s%s+%s%s^* )"
        sa sc sd sb sd sa sb sc
    let re =
        List.append (prods a c)
            (prods [term -1] (prods (Quaternion.conj d) b))
    let im =
        List.append (prods d a) (prods b (Quaternion.conj c))
    let result = List.append re (prods im [z])
    printfn @"=(%s,%s)=%s \\" (tstr re) (tstr im) (tstr result)

Math7.prologue "# 四元数"
printfn @"&(a+bj)(c+dj) \\"
printfn @"&=\{(%s)+(%s)j\}\{(%s)+(%s)j\} \\"
    (strs a) (strs b) (strs c) (strs d)
printfn @"&=(%s)(%s) \\" (strs q1) (strs q2)
let q3q = prods q1 q2 in showEq true q3q
Math7.epilogue()
Math7.prologue "## 分類"
classify q3q
Math7.epilogue()
Math7.prologue "## 確認"
let i, _0, _1 = Term.fromE [1], [term.Zero], [term.One]
let ijk = [[i], _0; _0, _1; _0, [i]]
for x in ijk do for y in ijk do tupleProd j (x, y)
Math7.epilogue()

Math7.prologue "# 八元数"
let k, h = Term.fromE [3], Term.fromE [4]
let ijkh = [[i], _0; [j], _0; [k], _0; _0, _1
            _0, [i]; _0, [j]; _0, [k]]
let ocomb = [for x in ijkh do for y in ijkh -> x, y]
ocomb |> List.iter (tupleProd h)
Math7.epilogue()
printfn ""
printfn "## 確認"
let check f =
    let g: term list * term list -> bool = function
    | [x], [y] ->
        match x.E, y.E with
        | [a], [b] -> a > 0 && b > 0 && a <> b
        | _ -> false
    | _ -> false
    ocomb |> List.filter (f >> g) |> List.iter (tupleProd h)
Math7.prologue "### db"
check <| fun ((_, b), (_, d)) -> b, d
Math7.epilogue()
Math7.prologue "### ad"
check <| fun ((a, _), (_, d)) -> a, d
Math7.epilogue()
Math7.prologue "### bc"
check <| fun ((_, b), (c, _)) -> b, c
Math7.epilogue()
