open System
open System.Text

type Editor = {
    cpos: int * int
    mode: string
    lines: list<StringBuilder>
}

let zeroesBeforeNumber (e: Editor) (i: int) =
    i.ToString ()

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

    printfn $"{ zeroesBeforeNumber (e) (i + 1) } { e.lines[i] }"

    if i < e.lines.Length - 1 then render (e) (i + 1)
    else ()

let processKey (e: Editor) (k: ConsoleKey) =
    if e.mode = "normal" then
        match k with
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
            { e with lines = List.insertAt (snd e.cpos + 1) (StringBuilder("")) (e.lines); cpos = (0, snd e.cpos + 1) }
        | _ -> e
    elif e.mode = "insert" then
        match k with
        | ConsoleKey.LeftArrow -> move (e) (-1, 0)
        | ConsoleKey.DownArrow -> move (e) (0, -1)
        | ConsoleKey.UpArrow -> move (e) (0, 1)
        | ConsoleKey.RightArrow -> move (e) (1, 0)
        | ConsoleKey.Escape ->
            { e with mode = "normal" }
        | ConsoleKey.Enter ->
            { e with lines = List.insertAt (snd e.cpos + 1) (StringBuilder "") (e.lines); cpos = (0, snd e.cpos + 1) }
        | ConsoleKey.Backspace ->
            { e with lines = List.mapi (fun (i: int) (line: StringBuilder) -> if i = snd e.cpos (* && fst e.cpos > 0 *) then line.Insert (fst e.cpos, "\b") else line) e.lines; cpos = (* if fst e.cpos > 0 then *) (fst e.cpos - 1, snd e.cpos) (* else e.cpos *) }
        | ConsoleKey.Tab ->
            { e with lines = List.mapi (fun (i: int) (line: StringBuilder) -> if i = snd e.cpos then line.Insert (fst e.cpos, "    ") else line) e.lines; cpos = (fst e.cpos + 4, snd e.cpos) }
        | _ ->
            { e with lines = List.mapi (fun (i: int) (line: StringBuilder) -> if i = snd e.cpos then line.Insert (fst e.cpos, k) else line) e.lines; cpos = (fst e.cpos + 1, snd e.cpos) }
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
            StringBuilder().Append "let main argv ="
            StringBuilder().Append "    printfn \"Hello world\""
            StringBuilder().Append ""
            StringBuilder().Append "    i <- i + 1"
            StringBuilder().Append "    printfn \"i is a mutable value\""
        ]
    }

    render e 0

    let rec loop (e: Editor) =
        let k = (Console.ReadKey true).Key

        let e = processKey e k
        render e 0
        
        loop e

    loop e

    Console.CursorVisible <- true

    0
