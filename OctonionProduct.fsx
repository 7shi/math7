#load "Math7.Octonion.fsx"
open Math7

let show title =
    Term.prodTerms title "" Octonion.str Octonion.prod id Term.byIndexSign

let a = [for i in [1..7] -> term([sprintf "a_%d" i], [i])]
let b = [for i in [1..7] -> term([sprintf "b_%d" i], [i])]
let aa = term(["a_0"])::a
let bb = term(["b_0"])::b
show "## 二乗（虚部）" (fun _ -> true) a a
show "## 二乗（実部＋虚部）" ((=) 1) aa aa
show "## 積（虚部）" (fun _ -> true) a b
show "## 積（実部＋虚部）" (fun _ -> true) aa bb
show "## ノルムの二乗" (fun _ -> true) (Octonion.conj aa) aa
