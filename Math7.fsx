namespace Math7

type testList = System.Collections.Generic.List<unit -> unit>

module Math7 =
    let isMain() =
        System.Diagnostics.StackTrace().GetFrame(1).GetMethod().Name = "main@"

    let prologue title =
        printfn @""
        printfn "%s" title
        printfn @""
        printfn "```math"
        printfn @"\begin{align}"
    let epilogue() =
        printfn @"\end{align}"
        printfn "```"

    let test tag expected result =
        if expected = result then
            printfn @"%s\ [OK]&\ %s \\" tag result
        else
            printfn @"%s\ [NG]&\ %s \\" tag result
            printfn @"expected:&\ %s \\" expected
    let tests title ts =
        prologue title
        for t in ts do t()
        epilogue()

    let combination n k =
        let rec f i = function
        | [] -> []
        | x::xs ->
            match f (i + 1) xs with
            | [] -> if x + k - i > n then [] else [x + 1 .. x + k - i]
            | ys -> x::ys
        let rec g cur =
            match f 0 cur with
            | [] -> [cur]
            | next -> cur :: g next
        g [1..k]

    if isMain() then
        printfn "combination 4 2 -> %A" (combination 4 2)
