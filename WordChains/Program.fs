//Kata from http://codekata.com/kata/kata19-word-chains/

open System
open System.Diagnostics
open System.IO

let readDictionary filename =
    File.ReadAllLines filename
    |> Set.ofArray

let measuredAction (f:unit -> 'a) =
    let stopwatch = new Stopwatch()
    stopwatch.Start()
    let result = f()
    stopwatch.Stop()
    (result, stopwatch.Elapsed)

let runWordChains filename firstWord targetWord =
    let dictionary = filename |> readDictionary
    dictionary |> Set.count |> printfn "Read %d words"

    printfn "Trying to find chain from '%s' to '%s'" firstWord targetWord
    let (result,time) = (fun () -> WordChains.getFirstShortestPath dictionary firstWord targetWord) |> measuredAction

    printfn "%A" result
    printfn "Chain length: %d" (result |> List.length)
    printfn "Time: %A" time

[<EntryPoint>]
let main argv = 
    if(argv |> Array.length < 3) then
        printfn "Usage: WordChains.exe dictionaryFilePath firstWord targetWord"
        printfn ""
        printfn "Note: firstWord and targetWord need to be of equal lengths"
        printfn "Example: WordChains.exe wordlist.txt lead gold"
        1
    else
        let firstWord = argv.[1]
        let targetWord = argv.[2]

        if(firstWord.Length = targetWord.Length) then
            runWordChains argv.[0] firstWord targetWord
            //printfn "Press any key to continue"
            //Console.ReadKey() |> ignore
            0
        else
            printfn "Both words need to be of equal lengths. Exiting"
            2    