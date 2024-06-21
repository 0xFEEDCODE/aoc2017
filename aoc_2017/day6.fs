module aoc_2017.day6

open System
open System.Collections.Generic
open helpers


let solve() =
    let inp = "14	0	15	12	11	11	3	5	1	6	8	4	9	1	8	4"
    
    let mutable data = inp.Split "\t" |> Seq.where(fun x -> x.Length > 0) |> Seq.map(fun x -> x |> int) |> Seq.toArray
    
    let get_highest_block bl = bl |> smapi |> Seq.sortByDescending(snd)  |> Seq.head
    
    let seen = Dictionary()
    
    let generate_blocks_hash bl = bl |> Seq.fold(fun acc x -> HashCode.Combine(acc, x)) 0
    
    let q = Queue()
    q.Enqueue (data, 0)
    
    let mutable seen_once = false
    
    while (q.Count > 0) do
        let (blocks, steps) = q.Dequeue()
        let hash = generate_blocks_hash blocks
        
        if seen.ContainsKey(hash) then
            if seen_once = false then
                seen.Clear()
                q.Enqueue((blocks, 0))
                
            if seen_once = true then
                q.Clear()
                
            seen_once <- true
            
            printfn $"%A{steps}"
            
        else
            seen.Add (hash, 0)
            
            let (hbl_i, hbl_v) = get_highest_block blocks
            blocks[hbl_i] <- 0
            
            for i = 1 to hbl_v do
                let index = (hbl_i + i) % blocks.Length
                let entry = blocks[(hbl_i + i) % blocks.Length]
                blocks[index] <- entry + 1
                
            q.Enqueue((blocks, steps+1))
    0

