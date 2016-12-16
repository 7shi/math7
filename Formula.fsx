// https://gist.github.com/7shi/c0a6f489fbe22b98b015

type Expr =
| Num of int
| Add of Expr * Expr
| Mul of Expr * Expr
| Div of Expr * Expr
| Var of string * int
    static member (+)(x, y) = Add(x, Num y)
    static member (+)(x, y) = Add(Num x, y)
    static member (+)(x, y) = Add(x, y)
    static member (*)(x, y) = Mul(x, y)
    static member (/)(x, y) = Div(x, y)
    static member Pow(x, n) =
        match x with
        | Var(name, xn) -> Var(name, xn * n)
        | _ -> failwith "not supported"

let x = Var("x", 1)

let rec tostr e =
    let tostr2 = function
    | Add _ as e -> "(" + (tostr e) + ")"
    | e          -> (tostr e)
    match e with
    | Num n        -> n.ToString()
    | Add(x, y)    -> (tostr  x) + "+" + (tostr  y)
    | Mul(x, y)    -> (tostr2 x) + "*" + (tostr2 y)
    | Div(x, y)    -> (tostr2 x) + "/" + (tostr2 y)
    | Var(name, 1) -> name
    | Var(name, n) -> sprintf "%s^%d" name n

let rec differentiate = function
| Add(x, y)   -> (differentiate x) + (differentiate y)
| Mul(x, y)   -> x * (differentiate y)
| Div(x, y)   -> (differentiate x) / y
| Var("x", 1) -> Num 1
| Var("x", n) -> (Num n) * (x ** (n - 1))
| _           -> Num 0

let rec integrate = function
| Add(x, y)   -> (integrate x) + (integrate y)
| Mul(x, y)   -> x * (integrate y)
| Div(x, y)   -> (integrate x) / y
| Var("x", n) -> (x ** (n + 1)) / (Num(n + 1))
| Num 1       -> x
| e           -> e * x

let indefIntegrate e = (integrate e) + Var("C", 1)

let rec eval = function
| Num n     -> float n
| Add(x, y) -> (eval x) + (eval y)
| Mul(x, y) -> (eval x) * (eval y)
| Div(x, y) -> (eval x) / (eval y)
| _         -> nan

let f = x**3 + x**2 + x + 1
let f1 = differentiate  f
let f2 = indefIntegrate f
let f3 = differentiate  f2

printfn "f1: d/dx %s = %s" (tostr f) (tostr f1)
printfn "f2: ∫ %s dx = %s" (tostr f) (tostr f2)
printfn "f3: d/dx f2 = %s" (tostr f3)
