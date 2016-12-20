#load "Math7.Exterior.fsx"
open Math7

Math7.prologue "## 虚数の二乗"
for n = 2 to 10 do
    let e1 = [1..n]
    let e2 = List.append e1 e1 |> Term.fromE
    let e3 = Exterior.sort e2
    let e4 = term(e3.N, e3.A, Exterior.rmPairE e3.E)
    printfn @"i_{%d}^2&=%s \\" n (Term.str (Term.vec "") e4)
Math7.epilogue()

Exterior.testHodge 2
Exterior.testHodge 3
Exterior.testHodge 4

Exterior.showProd "## 2次元" 2
Exterior.showProd "## 3次元" 3
Exterior.showProd "## 4次元" 4
