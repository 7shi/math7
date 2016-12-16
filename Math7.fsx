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

module Term =
    let dupNA (t:term) = term(t.N, t.A, [] )
    let dupE  (t:term) = term(1  , [] , t.E)
    let addSign (v:string) =
        if v.StartsWith "-" then v else "+" + v

    let split  t  = dupNA t, dupE t
    let splits ts = ts |> List.map split

    let bracket (s:string) =
        if   s.Contains "\{" then  "[" + s +  "]"
        elif s.Contains  "(" then "\{" + s + "\}"
        else                       "(" + s +  ")"

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

    let str f t =
        strA t + f t.E
        |> function "" -> "1" | "-" -> "-1" | s -> s

    let strs f ts =
        ts
        |> Seq.mapi (fun i t ->
            let t = str f t
            if i = 0 then t else addSign t)
        |> String.concat ""

    let str2E f e =
        let s = str f e
        if e.N < 0 then bracket s else s

    let str2 f (a, e) =
        match strA a, str2E f e with
        | "", "1" -> "1"
        | a , "1" -> a
        | a , e   -> a + e

    let strs2 f ts =
        ts
        |> Seq.mapi (fun i t ->
            let s = str2 f t
            if i = 0 then s else addSign s)
        |> String.concat ""

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
        |> Seq.map (fun (e, ts) -> e, simplifyA ts)
        |> Seq.filter (fun (_, ts) -> not <| ts.IsEmpty)
        |> Seq.toList

    let byIndexSign (n, a:string list) =
        a
        |> Seq.map (fun s ->
            let i = s.IndexOf '_'
            if i < 0 then s else s.[i + 1 ..])
        |> Seq.sort
        |> String.concat ""
        |> (fun s -> s + if n = 1 then "+" else "-")

    let vec op es =
        es
        |> Seq.map (sprintf @"\vec{e_%d}")
        |> String.concat op

    let prodTermsCore op f (g:int list -> term) sort1 sort2 h al bl =
        let sa = strs2 f al |> bracket
        let sb = strs2 f bl |> bracket
        al |> Seq.iteri (fun i a ->
            let s = str2 f a
            if i = 0 then
                printfn @"&=%s%s%s \\" s op sb
            else
                printfn @"&\quad %s%s%s \\" (addSign s) op sb)
        let c =
            al |> List.mapi (fun i (aa, ae) ->
                let list = bl |> List.mapi (fun j (ba, be) ->
                    let pa = prod aa ba
                    let pe = prod ae be
                    let ge = pe.N * g pe.E
                    let sp =
                        let se = str2E f pe
                        if se = str2E f ge then str2 f (pa, pe) else
                        sprintf @"%s\underbrace{%s}_{%s}" (strA pa) se (str f ge)
                    match i, j with
                    | 0, 0 -> printf @"&=%s" sp
                    | _, 0 -> printf @"&\quad %s" (addSign sp)
                    | _, _ -> printf @"%s" (addSign sp)
                    prod pa ge)
                printfn @" \\"
                list)
            |> List.concat
        let d = c |> simplify |> List.sortBy (fst >> sort1)
        if d.Length < c.Length then
            d |> List.iteri (fun i (e, al) ->
                let sign, slist =
                    let slist =
                        al
                        |> List.sortBy (fun t -> sort2 (t.N, t.A))
                        |> List.map (strA >> addSign)
                    let m =
                        slist
                        |> Seq.filter (fun (x:string) -> x.StartsWith "-")
                        |> Seq.length
                    if m < List.length slist then "", slist else
                    "-", slist |> List.map (fun (x:string) -> "+" + x.[1..])
                let sal =
                    let sal =
                        slist
                        |> String.concat ""
                        |> (fun x -> if x.StartsWith "+" then x.[1..] else x)
                    if (sign = "" && d.Length = 1) then sal else
                    sign + if al.Length = 1 then sal else "(" + sal + ")"
                let se = f e
                if i = 0 then
                    printf @"&=%s%s" sal se
                else
                    if h i then
                        printfn @" \\"
                        printf @"&\quad "
                    printf "%s%s" (addSign sal) se)
            printfn @" \\"

    let prodTerms title op f g sort1 sort2 h a b =
        printfn @""
        printfn "%s" title
        printfn @""
        printfn "```math"
        printfn @"\begin{align}"
        let sa, sb = strs f a |> bracket, strs f b |> bracket
        if op = "" && sa = sb then
            printfn @"&%s^2 \\" sa
        else
            printfn @"&%s%s%s \\" sa op sb
        prodTermsCore op f g sort1 sort2 h (splits a) (splits b)
        printfn @"\end{align}"
        printfn "```"
