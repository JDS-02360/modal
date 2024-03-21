open System
open System.Text

type Editor = {
    cpos: int * int
    mode: string
    lines: list<StringBuilder>
}

let listReplaceAt (i: int) (s: StringBuilder) (l: list<StringBuilder>) =
    let removedL = List.removeAt i l
    List.insertAt i s removedL

let rec render (e: Editor) (i: int) =
    if i = 0 then Console.Clear ()

    printfn $"{ e.lines[i] }"

    if i < e.lines.Length - 1 then render (e) (i + 1)
    else ()

let processKey (e: Editor) (k: ConsoleKeyInfo) =
    if e.mode = "normal" then
        match k.Key with
        | ConsoleKey.H ->
            if fst e.cpos > 0 then
                { e with cpos = (fst e.cpos - 1, snd e.cpos) }
            else e
        | ConsoleKey.J ->
            if snd e.cpos < e.lines.Length then
                if fst e.cpos > e.lines[snd e.cpos + 1].Length then
                    { e with cpos = (0, snd e.cpos + 1) }
                else
                    { e with cpos = (fst e.cpos, snd e.cpos + 1) }
            else e
        | ConsoleKey.K ->
            if snd e.cpos > 0 then
                if fst e.cpos > e.lines[snd e.cpos - 1].Length then
                    { e with cpos = (0, snd e.cpos - 1) }
                else
                    { e with cpos = (fst e.cpos, snd e.cpos - 1) }
            else e
        | ConsoleKey.L ->
            if fst e.cpos < e.lines[snd e.cpos].Length then
                { e with cpos = (fst e.cpos + 1, snd e.cpos) }
            else e
        | ConsoleKey.LeftArrow ->
            if fst e.cpos > 0 then
                { e with cpos = (fst e.cpos - 1, snd e.cpos) }
            else e
        | ConsoleKey.DownArrow ->
            if snd e.cpos < e.lines.Length then
                if fst e.cpos > e.lines[snd e.cpos + 1].Length then
                    { e with cpos = (0, snd e.cpos + 1) }
                else
                    { e with cpos = (fst e.cpos, snd e.cpos + 1) }
            else e
        | ConsoleKey.UpArrow ->
            if snd e.cpos > 0 then
                if fst e.cpos > e.lines[snd e.cpos - 1].Length then
                    { e with cpos = (0, snd e.cpos - 1) }
                else
                { e with cpos = (fst e.cpos, snd e.cpos - 1) }
            else e
        | ConsoleKey.RightArrow ->
            if fst e.cpos < e.lines[snd e.cpos].Length then
                { e with cpos = (fst e.cpos + 1, snd e.cpos) }
            else e
        | ConsoleKey.I ->
            { e with mode = "insert" }
        | ConsoleKey.Enter ->
            { e with lines = List.insertAt (snd e.cpos + 1) (StringBuilder "") (e.lines); cpos = (0, snd e.cpos + 1) }
        | _ -> e
    elif e.mode = "insert" then
        match k.Key with
        | ConsoleKey.LeftArrow ->
            if fst e.cpos > 0 then
                { e with cpos = (fst e.cpos - 1, snd e.cpos) }
            else e
        | ConsoleKey.DownArrow ->
            if snd e.cpos < e.lines.Length then
                if fst e.cpos > e.lines[snd e.cpos + 1].Length then
                    { e with cpos = (0, snd e.cpos + 1) }
                else
                    { e with cpos = (fst e.cpos, snd e.cpos + 1) }
            else e
        | ConsoleKey.UpArrow ->
            if snd e.cpos > 0 then
                if fst e.cpos > e.lines[snd e.cpos - 1].Length then
                    { e with cpos = (0, snd e.cpos - 1) }
                else
                    { e with cpos = (fst e.cpos, snd e.cpos - 1) }
            else e
        | ConsoleKey.RightArrow ->
            if fst e.cpos < e.lines[snd e.cpos].Length then
                { e with cpos = (fst e.cpos + 1, snd e.cpos) }
            else e
        | ConsoleKey.Escape ->
            { e with mode = "normal" }
        | ConsoleKey.Enter ->
            { e with lines = List.insertAt (snd e.cpos + 1) (StringBuilder "") (e.lines); cpos = (0, snd e.cpos + 1) }
        | ConsoleKey.Backspace ->
            let line = e.lines[snd e.cpos]

            if fst e.cpos > 0 then
                line.Remove((fst e.cpos) - 1, 1) |> ignore

                { e with lines = listReplaceAt (snd e.cpos) line e.lines; cpos = ((fst e.cpos) - 1, snd e.cpos) }
            else e
        | ConsoleKey.Tab ->
            let line = e.lines[snd e.cpos]
            line.Insert(fst e.cpos, "    ") |> ignore

            { e with lines = listReplaceAt (snd e.cpos) line e.lines; cpos = ((fst e.cpos) + 4, snd e.cpos) }
        | _ ->
            let line = e.lines[snd e.cpos]
            line.Insert(fst e.cpos, k.KeyChar.ToString()) |> ignore

            { e with lines = listReplaceAt (snd e.cpos) line e.lines; cpos = ((fst e.cpos) + 1, snd e.cpos) }
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

        let processedE = processKey e k
        render processedE 0
        
        loop processedE

    loop e

    Console.CursorVisible <- true

    0
