namespace Math7

#load "Math7.fsx"

type term(n:int, a:string list, e:int list) =
    new n = term(n, [], [])
    member x.N = n
    member x.A = a
    member x.E = e
    static member Zero = term 0
    static member One  = term 1
    static member (*)(n:int, t:term) =
        term(n * t.N, t.A, t.E)
    override x.ToString() = "term" + (n, a, e).ToString()

module Term =
    let fromE  e       = term(1  , [] , e )
    let dupNA (t:term) = term(t.N, t.A, [])
    let dupE  (t:term) = fromE t.E
    let addSign (v:string) =
        if v.StartsWith "-" then v else "+" + v
    let fix = function "" -> "1" | "-" -> "-1" | s -> s

    let split  t  = dupNA t, dupE t
    let splits ts = ts |> List.map split

    let bracket (s:string) =
        if   s.Contains "\{" then  "[" + s +  "]"
        elif s.Contains  "(" then "\{" + s + "\}"
        else                       "(" + s +  ")"

    let startEnd (str:string) s e =
        str.StartsWith s && str.EndsWith e

    let unbracket (s:string) =
        if startEnd s "(" ")" || startEnd s "[" "]" then
            s.[1 .. s.Length - 2]
        elif startEnd s "\{" "\}" then
            s.[2 .. s.Length - 3]
        else s

    let strPower (ss:string seq) =
        ss
        |> Seq.groupBy id
        |> Seq.map (fun (a, l) ->
            let len = Seq.length l
            if len = 1 then a else a + "^" + (string len))
        |> String.concat ""

    let strA (t:term) =
        match t.N with
        |  0 -> "0"
        |  1 ->            strPower t.A
        | -1 ->      "-" + strPower t.A
        |  n -> string n + strPower t.A

    let str f t = strA t + f t.E |> fix

    let strs f ts =
        if Seq.length ts = 0 then "0" else
        ts
        |> Seq.mapi (fun i t ->
            let t = str f t
            if i = 0 then t else addSign t)
        |> String.concat ""

    let strE f e =
        let s = strA e + f e.E
        if e.N < 0 then fix s |> bracket else s

    let str2 f (a, e) = strA a + strE f e |> fix

    let strs2 f ts =
        ts
        |> Seq.mapi (fun i t ->
            let s = str2 f t
            if i = 0 then s else addSign s)
        |> String.concat ""

    let str3 f (e, al) =
        let sea = strA e
        let sal = strs f al
        let see = f e.E
        let sal = if Seq.length al = 1 then sal else bracket sal
        sea + sal + see |> fix

    let allNeg (ts:term list) =
        let minus = ts |> Seq.filter (fun t -> t.N < 0)
        ts.Length = Seq.length minus

    let neg (ts:term list) =
        ts |> List.map (fun t -> -1 * t)

    let gcd x y =
        let rec f x = function
        | 0 -> x
        | y -> f y (x % y)
        let m = x < 0 && y < 0
        let r = f (abs x) (abs y)
        if m then -r else r

    let gcdN:term list -> int = function
    | [] -> 0
    | ts -> ts |> Seq.map (fun t -> t.N) |> Seq.reduce gcd

    let prodA (t1:term) (t2:term) =
        term(t1.N * t2.N, List.append t1.A t2.A, [])

    let prod (t1:term) (t2:term) =
        let a = prodA t1 t2
        term(a.N, a.A, List.append t1.E t2.E)

    let strProd op f (g:int list -> term) (aa, ae) (ba, be) =
        let pa = prod aa ba
        let pe = prod ae be
        let ge = pe.N * g pe.E
        let sp =
            let se1 =
                if op = "" then strE f pe else
                strE f ae + op + strE f be
            let se2 = strA ge + f ge.E
            if se1 = se2 then strA pa + se1 |> fix else
            sprintf @"%s\underbrace{%s}_{%s}" (strA pa) se1 (fix se2)
        sp, prod pa ge

    let simplifyA (terms:term seq) =
        seq {
            for a, ts in terms |> Seq.groupBy (fun t -> List.sort t.A) ->
            match Seq.toList ts with
            | [ ] -> term.Zero
            | [x] -> x
            | x::xs ->
                let n = ts |> Seq.map (fun t -> t.N) |> Seq.sum
                let c = xs |> Seq.filter (fun t -> x.A = t.A) |> Seq.length
                term(n, (if c = xs.Length then x.A else a), [])}
        |> Seq.filter (fun t -> t.N <> 0)
        |> Seq.toList

    let simplify (terms:term seq) =
        terms
        |> Seq.groupBy (fun t -> t.E)
        |> Seq.map (fun (e, al) ->
            let e, al = fromE e, al |> Seq.map dupNA |> simplifyA
            let n = gcdN al
            if n = 1 then e, al else
            n * e, al |> List.map (fun a -> term (a.N / n, a.A, a.E)))
        |> Seq.filter (fun (_, ts) -> not <| ts.IsEmpty)
        |> Seq.toList

    let sort sort1 sort2 (eals:(term * term list) list) =
        eals
        |> List.sortBy (fun (e, _) -> sort1 e.E)
        |> List.map (fun (e, al) -> e, List.sortBy sort2 al)

    let byIndexSign (t:term) =
        t.A
        |> Seq.map (fun s ->
            let i = s.IndexOf '_'
            if i < 0 then s else s.[i + 1 ..])
        |> Seq.sort
        |> String.concat ""
        |> (fun s -> s + if t.N < 0 then "-" else "+")

    let vec op es =
        es
        |> Seq.map (sprintf @"\vec{e_%d}")
        |> String.concat op

    let showProd1 op f al bl =
        let sa = strs2 f al |> bracket
        let sb = strs2 f bl |> bracket
        al |> List.iteri (fun i a ->
            let s = str2 f a
            if i = 0 then
                printfn @"&=%s%s%s \\" s op sb
            else
                printfn @"&\quad %s%s%s \\" (addSign s) op sb)

    let showProd2 op f g al bl =
        al |> List.mapi (fun i a ->
            let list = bl |> List.mapi (fun j b ->
                let sp, p = strProd op f g a b
                match i, j with
                | 0, 0 -> printf @"&=%s" sp
                | _, 0 -> printf @"&\quad %s" (addSign sp)
                | _, _ -> printf @"%s" (addSign sp)
                p)
            printfn @" \\"
            list)
        |> List.concat

    let showProd3 f nl (d:(term * term list) list) =
        if d.Length = 0 then printfn @"&=0 \\" else
        d |> List.iteri (fun i d1 ->
            let s = str3 f d1
            let s = if d.Length = 1 then unbracket s else s
            if i = 0 then
                printf @"&=%s" s
            else
                if nl i then
                    printfn @" \\"
                    printf @"&\quad "
                printf "%s" (addSign s))
        printfn @" \\"

    let showProd title op f g sort1 sort2 nl a b =
        Math7.prologue title
        let sa, sb = strs f a |> bracket, strs f b |> bracket
        if op = "" && sa = sb then
            printfn @"&%s^2 \\" sa
        else
            printfn @"&%s%s%s \\" sa op sb
        let al, bl = splits a, splits b
        showProd1 op f al bl
        let c = showProd2 op f g al bl
        let d = c |> simplify |> sort sort1 sort2
        if d.Length < c.Length then showProd3 f nl d
        Math7.epilogue()
