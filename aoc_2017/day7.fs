module aoc_2017.day7

open System.Collections.Generic
open System.IO

open helpers

let solve() =
    let inp = File.ReadLines "day7.txt"
    
    let programs = Dictionary()
    
    inp |> Seq.iter(fun line ->
        let spl = line.Split "->"
        let connections = (spl.Length > 1) $ (lazy (spl[1].Split "," |> Seq.map(fun name -> name[1..])), lazy Seq.empty)
        let name = spl[0].Split " " |> Seq.head
        let weight = (spl[0].Split " " |> Seq.item 1) |> Seq.where(fun x -> x >= '0' && x <= '9') |> Seq.fold(fun acc x -> acc + x.ToString()) "" |> int
        programs.Add(name, (weight, connections))
        )
    
    let rec get_root el =
        let parent = programs |> Seq.tryFind(fun kv ->
            let _, connections = kv.Value
            connections |> Seq.contains el)
        
        match parent with
        | Some(parent) ->
            get_root parent.Key
        | None ->
            el
            
    let rec get_weight el =
        let (w, children) = programs[el]
        if (children |> Seq.isEmpty) then
           w 
        else
            w + (children |> Seq.sumBy(get_weight))
            
    let rec get_level el level = 
        let parent = programs |> Seq.tryFind(fun kv ->
            let _, connections = kv.Value
            connections |> Seq.contains el)
        
        match parent with
        | Some(parent) ->
            get_level parent.Key (level+1)
        | None ->
            level
        
    //printfn "%A" (get_root (programs.Keys |> Seq.head))
    let balance =
     programs
     |> Seq.where(fun x -> not ((snd x.Value) |> Seq.isEmpty))
     |> Seq.map(fun x -> (snd x.Value) |> Seq.map(fun y -> ((x.Key, (fst x.Value), y, (fst programs[y]))), (get_weight y)) )
     
    let unbalanced = balance |> Seq.where(fun x -> (x |> Seq.distinctBy(snd)  |> Seq.length) > 1)
                     |> Seq.sortBy(fun x ->
                         let (name, _, _, _) = fst (x |> Seq.head)
                         let level = get_level name 0
                         level
                         )
                     
    unbalanced |> Seq.iter(fun seq ->
        printfn "\n"
        seq |> Seq.iter(printf "%A "))
    printfn ""
    0
