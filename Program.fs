open System
open System.Text

type Editor = {
    cpos: int * int
    mode: string
    lines: list<StringBuilder>
}

let rec zeroesBeforeNumber (e: Editor) (istr: string) =
    if istr.Length < ((e.lines.Length) - 1).ToString().Length then
        let istr = istr.Insert (0, "0")

        zeroesBeforeNumber e istr
    else
        istr

let move (e: Editor) (cpos: int * int) =
    if fst cpos > 0 && fst cpos < e.lines[snd e.cpos].Length then
        { e with cpos = (fst e.cpos + fst cpos, snd e.cpos) }
    elif snd cpos > 0 && snd cpos < e.lines.Length then
        if fst cpos > e.lines[snd e.cpos + 1].Length then
            { e with cpos = (0, snd e.cpos + snd cpos) }
        else
            { e with cpos = (fst e.cpos, snd e.cpos + snd cpos) }
    else e

let rec render (e: Editor) (i: int) =
    if i = 0 then Console.Clear ()

    printfn $"{ zeroesBeforeNumber (e) ((i + 1).ToString ()) } { e.lines[i] }"

    if i < e.lines.Length - 1 then render (e) (i + 1)
    else ()

let processKey (e: Editor) (k: ConsoleKeyInfo) =
    if e.mode = "normal" then
        match k.Key with
        | ConsoleKey.H -> move (e) (-1, 0)
        | ConsoleKey.J -> move (e) (0, -1)
        | ConsoleKey.K -> move (e) (0, 1)
        | ConsoleKey.L -> move (e) (1, 0)
        | ConsoleKey.LeftArrow -> move (e) (-1, 0)
        | ConsoleKey.DownArrow -> move (e) (0, -1)
        | ConsoleKey.UpArrow -> move (e) (0, 1)
        | ConsoleKey.RightArrow -> move (e) (1, 0)
        | ConsoleKey.I ->
            { e with mode = "insert" }
        | ConsoleKey.Enter ->
            { e with lines = List.insertAt (snd e.cpos + 1) (StringBuilder "") (e.lines); cpos = (0, snd e.cpos + 1) }
        | _ -> e
    elif e.mode = "insert" then
        match k.Key with
        | ConsoleKey.LeftArrow -> move (e) (-1, 0)
        | ConsoleKey.DownArrow -> move (e) (0, -1)
        | ConsoleKey.UpArrow -> move (e) (0, 1)
        | ConsoleKey.RightArrow -> move (e) (1, 0)
        | ConsoleKey.Escape ->
            { e with mode = "normal" }
        | ConsoleKey.Enter ->
            { e with lines = List.insertAt (snd e.cpos + 1) (StringBuilder "") (e.lines); cpos = (0, snd e.cpos + 1) }
        | ConsoleKey.Backspace ->
            let line = e.lines[snd e.cpos]
            line.Insert(fst e.cpos, "\b") |> ignore

            move (e) (-1, 0)
        | ConsoleKey.Tab ->
            let line = e.lines[snd e.cpos]
            line.Insert (fst e.cpos, "    ") |> ignore

            move (e) (4, 0)
        | _ ->
            let line = e.lines[snd e.cpos]
            line.Insert(fst e.cpos, k.KeyChar.ToString()) |> ignore

            move (e) (1, 0)
    else e

[<EntryPoint>]
let main argv =
    Console.CursorVisible <- false

    let mutable e: Editor = {
        cpos = (0, 0)
        mode = "normal"
        lines = [
            // Placeholder for loaded file or written text
            StringBuilder().Append "open System"
            StringBuilder().Append ""
            StringBuilder().Append "let mutable i: int = 0"
            StringBuilder().Append ""
            StringBuilder().Append "[<EntryPoint>]"
            StringBuilder().Append "let main argv: string ="
            StringBuilder().Append "    let str: string = \"returned string\""
            StringBuilder().Append ""
            StringBuilder().Append "    printfn \"Hello world\""
            StringBuilder().Append ""
            StringBuilder().Append "    i <- i + 1"
            StringBuilder().Append "    printfn \"i is a mutable value\""
            StringBuilder().Append "    str"
        ]
    }

    render e 0

    let rec loop (e: Editor) =
        let k = (Console.ReadKey true)

        let e = processKey e k
        render e 0
        
        loop e

    loop e

    Console.CursorVisible <- true

    0
