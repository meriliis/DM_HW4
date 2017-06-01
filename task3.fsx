let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"input.txt")
let input = System.IO.File.ReadAllLines path 
            |> List.ofArray
            |> List.map (fun (x : string) -> x.Split() 
                                                |> List.ofArray)

let letters = Set.ofList(["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"])

// Finding all possible combinations of length n or less (except empty set) using items in set 'input'; returns list of string lists sorted by string list length
let findPossibleCombinations (n : int) (input : Set<string>) =
    let rec worker (input : Set<string>) = seq {
        if Set.count input <= n && Set.count input > 0 then 
            yield Set.toList input
        for s in input do
            yield! worker(Set.remove s input)
    }

    input 
        |> worker 
        |> Set.ofSeq 
        |> Set.toList 
        |> List.sortBy (fun x -> x.Length)

// This function finds the result by using several helper functions defined inside
let freqMap (support : int) (input : string list list) (n : int) (letters : Set<string>) (output : Map<string list, int>) = 
    let possibleCombinations = findPossibleCombinations n letters

    // Counting how many times combination 'comb' appears in 'input'; returns an updated map of frequencies
    let rec countFreqs (input : string list list) (output : Map<string list, int>) (comb : string list) = 
        match input with
        | []            -> output
        | head :: tail  -> if List.forall (fun x -> List.contains x head) comb then
                                if output.ContainsKey(comb) then
                                    let oldCount = output.Item(comb)
                                    countFreqs tail (output.Add(comb, oldCount + 1)) comb
                                else
                                    countFreqs tail (output.Add(comb, 1)) comb
                           else
                                countFreqs tail output comb

    // Excluding the infrequent combinations and their supersets; returns the updated list of combinations 
    let dropInfrequent (support : int) (output : Map<string list, int>) (combs : string list list) = 
        // Infrequent combinations in map
        let infrequentCombs = output 
                                 |> Map.filter (fun k v -> v < support)
                                 |> Map.toList 
                                 |> List.map fst
        
        // Drops the combination from list if it is a superset of infrequent combination 'infreq'; returns the updated list
        let rec dropCombs (infreq : string list) (combs : string list list) = 
            match combs with
            | []           -> []
            | head :: tail -> if List.forall (fun x -> List.contains x head) infreq then
                                dropCombs infreq tail
                              else 
                                head :: (dropCombs infreq tail)

        // Uses the function dropCombs on every infrequent combination; returns the final updated combination list
        let rec worker (infrequentCombs : string list list) (combs : string list list) = 
            match infrequentCombs with
            | []           -> combs
            | head :: tail -> let newCombs = dropCombs head combs
                              worker tail newCombs

        worker infrequentCombs combs

    // Uses the functions defined before to return the map of frequencies
    let rec worker (support : int) (input : string list list) (combs : string list list) (output : Map<string list, int>) =
        match combs with
        | []             -> output
        | head :: tail -> let newOutput = head |> countFreqs input output
                          let prunedCombs = tail |> dropInfrequent support newOutput
                          worker support input prunedCombs newOutput

    
    // Final result + filtering and sorting
    worker support input possibleCombinations output
        |> Map.filter (fun k v -> v >= support)
        |> Map.toList
        |> List.sortBy (fun (k, _) -> k.Length)
    

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let map = freqMap 5 input 4 letters Map.empty 
          
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

for (k, v) in map do
    printfn "%A %d" k v



                      

