#load "Math7.Term.fsx"
#load "Math7.Exterior.fsx"
open Math7

let bs = [[1;2];[1;3];[1;4];[2;3];[3;4];[4;2]]

Term.showProd "## 幾何学積 (4次元)" ""
    (Term.vec "")
    (fun e1 ->
        let e2 = e1 |> Term.fromE |> Exterior.sort
        term(e2.N, [], Exterior.rmPairE e2.E))
    id Term.byIndexSign ((=) 1)
    [for i in [1..6] -> term(1, [sprintf "a_%d" i], bs.[i-1])]
    [for i in [1..6] -> term(1, [sprintf "a_%d" i], bs.[i-1])]
