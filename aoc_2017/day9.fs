module aoc_2017.day9

open System.Collections.Generic
open System.Text
open helpers
open System.IO

let determine_garbage_len str start_idx = 
    let rec determine_garbage_len' (str: string) (start_idx: int) (len: int) (len_for_part2: int) =
        if str[start_idx + len] = '>' then
            (len + 1, len_for_part2)
        else
            let updated_len = (str[start_idx+len] = '!') ? (len + 2, len + 1)
            determine_garbage_len' str start_idx updated_len (str[start_idx+len] = '!') ? (len_for_part2,len_for_part2+1)
            
    determine_garbage_len' str start_idx 0 -1

let cleanup_garbage (str: string) =
    let mutable sb = StringBuilder()
    
    let mutable removed_garbage_len = 0
    
    let mutable i = 0
    while i < str.Length do
        let ch = str[i]
        if ch = '<' then
            let (garbage_len, l) = determine_garbage_len str i
            removed_garbage_len <- removed_garbage_len + l
            i <- i + garbage_len
            if (i < str.Length && str[i] = ',') then
                i <- i+1
        else
            sb <- sb.Append(ch)
            i <- i + 1
    printfn "%A" removed_garbage_len
    sb.ToString()
    
let solve() =
    let inp = File.ReadAllText "day9.txt"
    
    let levels = Dictionary()
    let stack = Stack()
    let clean_string = cleanup_garbage inp
    
    let mutable level = 0
    for ch in clean_string do
        match ch with
        | '{' ->
            level <- level + 1
            stack.Push level
        | '}' ->
            level <- stack.Pop()
            
            if not (levels.ContainsKey level) then
                levels[level] <- 0
            levels[level] <- levels[level] + 1
            
        | ',' ->
            level <- level - 1
        
    let score = levels |> Seq.sumBy(fun kv -> kv.Key * kv.Value)
    printfn "%A" levels
    printfn "%A" score
    0

