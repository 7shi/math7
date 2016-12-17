namespace Math7

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

type testf = (string -> string -> string -> unit) -> unit

module Term =
    let prologue title =
        printfn @""
        printfn "%s" title
        printfn @""
        printfn "```math"
        printfn @"\begin{align}"
    let epilogue() =
        printfn @"\end{align}"
        printfn "```"

    let testPrint tag expected result =
        if expected = result then
            printfn @"%s\ [OK]&\ %s \\" tag result
        else
            printfn @"%s\ [NG]&\ %s \\" tag result
            printfn @"expected:&\ %s \\" expected
    let test title tests =
        prologue title
        for t in tests do t testPrint
        epilogue()

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
        let s = str f e
        if e.N < 0 then bracket s else s

    let str2 f (a, e) =
        match strA a, strE f e with
        | "", "1" -> "1"
        | a , "1" -> a
        | a , e   -> a + e

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
        let s = sea + sal + see
        if s = "" then "1" else s

    let prodA (t1:term) (t2:term) =
        term(t1.N * t2.N, List.append t1.A t2.A, [])

    let prod (t1:term) (t2:term) =
        let a = prodA t1 t2
        term(a.N, a.A, List.append t1.E t2.E)

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
        |> Seq.map (fun (e, ts) -> term(1, [], e), simplifyA ts)
        |> Seq.filter (fun (_, ts) -> not <| ts.IsEmpty)
        |> Seq.toList

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

    let allNeg (ts:term list) =
        let minus = ts |> Seq.filter (fun t -> t.N < 0)
        ts.Length = Seq.length minus

    let neg (ts:term list) =
        ts |> List.map (fun t -> -1 * t)

    let showProd1 op f al bl =
        let sa = strs2 f al |> bracket
        let sb = strs2 f bl |> bracket
        al |> List.iteri (fun i a ->
            let s = str2 f a
            if i = 0 then
                printfn @"&=%s%s%s \\" s op sb
            else
                printfn @"&\quad %s%s%s \\" (addSign s) op sb)

    let showProd2 f (g:int list -> term) al bl =
        al |> List.mapi (fun i (aa, ae) ->
            let list = bl |> List.mapi (fun j (ba, be) ->
                let pa = prod aa ba
                let pe = prod ae be
                let ge = pe.N * g pe.E
                let sp =
                    let se = strE f pe
                    if se = strE f ge then str2 f (pa, pe) else
                    sprintf @"%s\underbrace{%s}_{%s}" (strA pa) se (str f ge)
                match i, j with
                | 0, 0 -> printf @"&=%s" sp
                | _, 0 -> printf @"&\quad %s" (addSign sp)
                | _, _ -> printf @"%s" (addSign sp)
                prod pa ge)
            printfn @" \\"
            list)
        |> List.concat

    let showProd3 f sort1 c =
        c
        |> simplify
        |> List.sortBy (fun (e, _) -> e.E |> sort1)

    let showProd4 f sort2 h (d:(term * term list) list) =
        if d.Length = 0 then printfn @"&=0 \\" else
        d |> List.iteri (fun i (e, al) ->
            let e, al = if allNeg al then -1 * e, neg al else e, al
            let s = str3 f (e, al |> Seq.map dupNA |> Seq.sortBy sort2)
            let s = if d.Length = 1 then unbracket s else s
            if i = 0 then
                printf @"&=%s" s
            else
                if h i then
                    printfn @" \\"
                    printf @"&\quad "
                printf "%s" (addSign s))
        printfn @" \\"

    let showProd title op f g sort1 sort2 h a b =
        prologue title
        let sa, sb = strs f a |> bracket, strs f b |> bracket
        if op = "" && sa = sb then
            printfn @"&%s^2 \\" sa
        else
            printfn @"&%s%s%s \\" sa op sb
        let al, bl = splits a, splits b
        showProd1 op f al bl
        let c = showProd2 f g al bl
        let d = showProd3 f sort1 c
        if d.Length < c.Length then
            showProd4 f sort2 h d
        epilogue()
