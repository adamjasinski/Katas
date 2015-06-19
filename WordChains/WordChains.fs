module WordChains

let replaceNthChar n c (str:string) =
        let chars = str.ToCharArray()
        chars.[n]  <- c
        System.String(chars)

let generateNextCandidates (input:string) (target:string) (pathSoFar:string list) dictionary =
    let isValidChoice candidate =
        dictionary |> Set.contains candidate 
        && pathSoFar |> List.tryFind (fun x -> x = candidate) |> Option.isNone

    let candidatesBringingUsCloserToTarget =
        seq{
            for i=0 to input.Length-1 do
                if(input.[i] <> target.[i]) then
                    let candidate = input |> replaceNthChar i target.[i]
                    if(isValidChoice candidate) then
                            yield candidate
    }
    let candidatesAtSameDistanceToTarget =    
        seq {
            for i=0 to input.Length-1 do
                if(input.[i] <> target.[i]) then
                    let nextLettersToTry = [|'a'..'z'|] |> Array.filter (fun x -> x <> target.[i] && x <> input.[i])
                    for k=0 to nextLettersToTry.Length-1 do
                        let candidate = input |> replaceNthChar i nextLettersToTry.[k]
                        if(isValidChoice candidate) then
                            yield candidate
    }

    Seq.append 
        candidatesBringingUsCloserToTarget 
        candidatesAtSameDistanceToTarget

let firstNonEmptyMappedSeqOrEmptyIfNone (mapping:'a -> seq<'b>) (source:seq<'a>) : seq<'b> = 
    let allMapped = source |> Seq.map mapping
    let firstNonEmpty = allMapped |> Seq.tryPick( fun col -> if(col |> Seq.isEmpty) then None else Some(col))
    match firstNonEmpty with
        |Some x -> x
        |_ -> Seq.empty

let getFirstShortestPath  dictionary (input:string) (target:string) =
    if(input.Length <> target.Length) then 
        invalidArg("input") "Word lengths don't match"

    if(dictionary |> Set.contains target |> not) then
        invalidArg("target") "Target word not isn't present in the dictionary"

    //TODO - remove mutable state and make this an 'accumulator' parameter
    let deadEndWords:Set<string> ref = ref Set.empty

    let rec getFirstShortestPathInner (pathSoFar:string list) (input:string) =
            if(!deadEndWords |> Set.contains input) then
                printfn "%s: Recognizing dead end, skipping" input
                List.empty
            else
                printfn "(Length %d) %A" (pathSoFar |> List.length) pathSoFar
                if(input = target) then
                    pathSoFar
                else
                    let candidates = dictionary |> generateNextCandidates input target pathSoFar
                    let firstMatchingPathOrEmpty = 
                        candidates 
                        |> firstNonEmptyMappedSeqOrEmptyIfNone ( fun c -> getFirstShortestPathInner (c :: pathSoFar) c |> Seq.ofList)
                    if(firstMatchingPathOrEmpty |> Seq.isEmpty) then
                        deadEndWords := (!deadEndWords + set(candidates))
                    firstMatchingPathOrEmpty |> List.ofSeq

    getFirstShortestPathInner [input] input
    |> List.rev
