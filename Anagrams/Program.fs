//Kata from http://codekata.com/kata/kata06-anagrams/

open System.IO

let findAnagrams (dictionary:string seq) =
    let toCanonicalForm (s:string) = 
        //'canonical' form of an anagram has all characters sorted
        let canonicalized  = s.ToCharArray() |> Array.sort
        System.String(canonicalized) |> string

    dictionary 
        |> Seq.groupBy toCanonicalForm
     
let readDictionary filename =
    File.ReadAllLines filename 
    |> Seq.distinctBy (fun x -> x.ToLowerInvariant()) 
    |> Array.ofSeq

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let filename = argv.[0]
    let dictionary = filename |> readDictionary
    dictionary |> Array.length |> printfn "Read %d words"

    let allAnagramGroups = dictionary |> findAnagrams |> Seq.cache
    let topN = 10
    printfn "Top %d longest anagrams: " topN
    allAnagramGroups 
    |> Seq.filter ((snd >> Seq.length) >> ( (<) 1) )
    |> Seq.sortBy (fst >> String.length >> (~-)) 
    |> Seq.take topN 
    |> Seq.iter( snd >> printfn "%A")

    printfn ""
    printfn "Top %d most numerous sets of anagrams: " topN
    allAnagramGroups 
    //|> Seq.filter ((fst >> Seq.length) >> ( (<) 8) ) //extra filter on min word length
    |> Seq.sortBy (snd >> Seq.length >> (~-)) 
    |> Seq.take topN 
    |> Seq.iter( fun x -> printfn "Count %d %A" (x |> snd |> Seq.length) (x |> snd |> List.ofSeq))

    printfn "Press any key to continue"
    System.Console.ReadKey() |> ignore
    0
