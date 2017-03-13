type Sedenion =
    E | I | J | K | H of Sedenion | L of Sedenion

    override x.ToString() =
        match x with
        |   E -> "1" | I -> "i" | J -> "j" | K -> "k"
        | H E -> "h"
        | H x -> string x + "_h"
        | L E -> @"{\ell}"
        | L (H E) -> @"h_{\ell}"
        | L (H x) -> string x + @"_{h\ell}"
        | L x -> string x + @"_{\ell}"

    member x.Next =
        match x with
        |   I -> J
        |   J -> K
        |   K -> I
        | H x -> H x.Next
        | L x -> L x.Next
        |   x -> x

let right (s:string) = s.Substring(s.LastIndexOf '=' + 1)

let rec prod sgn x y =
    let sx, sy = string x, string y
    let left = sgn + sx + sy + if sgn = "" then "&=" else "="
    let inv = if sgn = "-" then "" else "-"
    match x, y with
    | _, E -> sx
    | E, _ -> sy
    | x, y when x = y -> sx + "^2&=-1"
    | L x, L E ->
        // (xℓ)ℓ=-x
        left + sgn + "(" + string x + @"\ell)\ell=" + inv + string x
    | L E, L _ -> left + prod inv y x
    | L x, L y ->
        // (xℓ)(yℓ)=-(xℓ)(ℓy)=((xℓ)ℓ)y=-xy
        let sx, sy = string x, string y
        let result = prod sgn y x |> right
        sprintf "%s%s(%s\ell)(%s\ell)=%s(%s\ell)({\ell}%s)=%s((%s\ell)\ell)%s=%s%s%s=%s"
                left  sgn sx sy  inv sx sy  sgn sx sy  inv sx sy  result
    | L _, H _ -> left + prod inv y x
    | H E, L (H E) -> left + sgn + @"h(h\ell)=" + inv + @"\ell"  // h(hℓ)=-ℓ
    | H E, L (H y) ->
        // h((yh)ℓ)=-(h(yh))ℓ=((yh)h)ℓ=-yℓ
        let sy = string y
        sprintf @"%s%sh((%sh)\ell)=%s(h(%sh))\ell=%s((%sh)h)\ell=%s%s\ell=%s%s_{\ell}"
                left  sgn sy  inv sy  sgn sy  inv sy  inv sy
    | H E, L E -> left + sgn + @"h_{\ell}"  // hℓ
    | H E, L y ->
        // h(yℓ)=-(hy)ℓ=(yh)ℓ
        let sy = string y
        sprintf @"%s%sh(%s\ell)=%s(h%s)\ell=%s(%sh)\ell=%s%s_{h\ell}"
                left  sgn sy  inv sy  sgn sy  sgn sy
    | H x, L (H E) ->
        // (xh)(hℓ)=-((xh)h)ℓ=xℓ
        let sx = string x
        sprintf @"%s%s(%sh)(h\ell)=%s((%sh)h)\ell=%s%s\ell=%s%s_{\ell}"
                left  sgn sx  inv sx  sgn sx  sgn sx
    | H x, L (H y) when x = y ->
        // (xh)((xh)ℓ)=-ℓ
        left + sgn + sx + "(" + string (H y) + @"\ell)=" + inv + @"\ell"
    | H x, L (H y) ->
        // (xh)((yh)ℓ)=-((xh)(yh))ℓ=-(yx)ℓ
        let sgn2, yx = if y.Next = x then inv, x.Next else sgn, y.Next
        let sy, syx = string (H y), string yx
        sprintf @"%s%s%s(%s\ell)=%s(%s%s)\ell=%s(%s)\ell=%s%s_{\ell}"
                left  sgn sx sy  inv sx sy  sgn2 syx  sgn2 syx
    | H x, L E -> left + sgn + (string x) + "_{h\ell}"  // (xh)ℓ
    | H x, L y when x = y ->
        // (xh)(xℓ)=-((xh)x)ℓ=(x(xh))ℓ=-hℓ
        let sx2 = string x
        sprintf @"%s%s%s(%s\ell)=%s(%s%s)\ell=%s(%s%s)\ell=%sh_{\ell}"
                left  sgn sx sx2  inv sx sx2  sgn sx2 sx  inv
    | H x, L y ->
        // (xh)(yℓ)=-((xh)y)ℓ=-((yx)h)ℓ
        let sgn2, yx = if y.Next = x then inv, x.Next else sgn, y.Next
        let sy, syx = string y, string yx
        sprintf @"%s%s%s(%s\ell)=%s(%s%s)\ell=%s(%sh)\ell=%s%s_{h\ell}"
                left  sgn sx sy  inv sx sy  sgn2 syx  sgn2 syx
    | _, L (H E) ->
        sprintf @"%s%s%s(h\ell)=%s(%sh)\ell=%s%s"
                left  sgn sx  inv sx  inv (string (L (H x)))
    | x, L (H y) when x = y ->
        // i((ih)ℓ)=-(i(ih))ℓ=hℓ
        sprintf @"%s%s%s(%s_h\ell)=%s(%s%s_h)\ell=%sh\ell=%sh_{\ell}"
                left  sgn sx sx  inv sx sx  sgn  sgn
    | x, L (H y) ->
        // i((jh)ℓ)=-(i(jh))ℓ=((ij)h)ℓ=(kh)ℓ
        let sy = string (H y)
        let sgn2, xy = if x.Next = y then sgn, y.Next else inv, x.Next
        sprintf @"%s%s%s(%s\ell)=%s(%s%s)\ell=%s%s\ell=%s%s"
                left  sgn sx sy  inv sx sy  sgn2 (string (H xy))  sgn2 (string (L (H xy)))
    | x, L E -> left + sgn + sx + @"_{\ell}"  // xℓ
    | x, L y when x = y ->
        left + sgn + sx + "(" + sx + @"\ell)=" + inv + @"\ell"  // x(xℓ)=-ℓ
    | x, L y ->
        // x(yℓ)=-(xy)ℓ
        let sgn2, xy = if x.Next = y then inv, y.Next else sgn, x.Next
        let sy, sxy = string y, string xy
        sprintf @"%s%s%s(%s\ell)=%s(%s%s)\ell=%s%s\ell=%s%s_{\ell}"
                left  sgn sx sy  inv sx sy  sgn2 sxy  sgn2 sxy
    | H x, H E -> left + sgn + "(" + string x + "h)h=" + inv + string x
    | H E, H _ -> left + prod inv y x
    | H x, H y ->
        // (xh)(yh)=-(xh)(hy)=((xh)h)y=-xy
        let sx, sy = string x, string y
        let result = prod sgn y x |> right
        sprintf "%s%s(%sh)(%sh)=%s(%sh)(h%s)=%s((%sh)h)%s=%s%s%s=%s"
                left  sgn sx sy  inv sx sy  sgn sx sy  inv sx sy  result
    | x, H E -> left + sgn + string (H x)
    | x, H y when x = y ->
        left + sgn + sx + "(" + sx + "h)=" + inv + "h"  // x(yh)=-(xy)h
    | x, H y ->
        // x(yh)=-(xy)h
        let sgn2, xy = if x.Next = y then inv, y.Next else sgn, x.Next
        let sy, sxy = string y, string xy
        sprintf "%s%s%s(%sh)=%s(%s%s)h=%s%sh=%s%s_h"
                left  sgn sx sy  inv sx sy  sgn2 sxy  sgn2 sxy
    | x, y when x.Next = y -> left + sgn + (string y.Next)
    | x, y ->
        if sgn <> "" then "??? " + sx + sy else
        left + prod inv y x

printfn @"\begin{align}"
let elems = [[I; J; K]
             [H E; H I; H J; H K]
             [L E; L I; L J; L K]
             [L (H E); L (H I); L (H J); L (H K)]]
elems |> List.iteri (fun i xs ->
    elems |> List.iteri (fun j ys ->
        for x in xs do
            for y in ys do
                printfn @"%s \\" (prod "" x y)
        if i + j < 6 then printfn @"\hline"))
printfn @"\end{align}"
