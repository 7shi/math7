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

    let combination n i =
        let rec f j = function
        | [] -> []
        | s::ss ->
            match f (j + 1) ss with
            | [] -> if s + 1 > j + n - i then [] else [s + 1 .. s + 1 + i - j]
            | ss -> s::ss
        let rec g list =
            match f 1 list with
            | [] -> [list]
            | next -> list :: g next
        g [1..i]
