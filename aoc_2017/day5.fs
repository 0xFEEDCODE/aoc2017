module aoc_2017.day5
open System.Collections.Generic
open System.IO

open helpers

let solve_pt1() =
    let inp = File.ReadAllLines "day5.txt" |> Seq.map(fun x -> x |> int)
    let dict = Dictionary()
    inp |> Seq.mapi(fun i x -> (i,x)) |> Seq.iter(fun (i, x) -> dict[i] <- x)
    
    let rec process_loop i steps =
        if not (dict.ContainsKey i) then
            steps
        else
            let jump = dict[i]
            dict[i] <- jump + 1
            
            process_loop (i + jump) (steps+1)
            
    printfn $"%A{process_loop 0 0}"
    
let solve_pt2() =
    let inp = File.ReadAllLines "day5.txt" |> Seq.map(fun x -> x |> int)
    let dict = Dictionary()
    inp |> Seq.mapi(fun i x -> (i,x)) |> Seq.iter(fun (i, x) -> dict[i] <- x)
    
    let rec process_loop i steps =
        if not (dict.ContainsKey i) then
            steps
        else
            let jump = dict[i]
            dict[i] <- jump + ((jump >= 3) ? (-1, +1))
            
            process_loop (i + jump) (steps+1)
            
    printfn $"%A{process_loop 0 0}"
    

let solve() =
    solve_pt1()
    solve_pt2()

