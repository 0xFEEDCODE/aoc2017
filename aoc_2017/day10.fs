module aoc_2017.day10

open System.Collections.Generic
open helpers

let inp = "14,58,0,116,179,16,1,104,2,254,167,86,255,55,122,244"

let rev_section start_idx len (data: 'a seq) =
    let mutable data = data |> Seq.toArray
    
    for i = 0 to ((len-1)/2) do
        let left_idx = (start_idx + i) % (data |> Seq.length)
        let right_idx = ((start_idx+(len-1)) - i) % (data |> Seq.length)
        
        let temp = data[right_idx]
        data[right_idx] <- data[left_idx]
        data[left_idx] <- temp
        
    data
    
let part1() =
    let lengths = inp.Split "," |> Seq.map(fun x -> x |> int) |> Seq.toArray
    let processed = Dictionary()
    lengths |> Seq.iter(fun x -> processed.Add(x, false))
    
    let data = seq {for i in 0..255 -> i} |> Seq.toArray
    
    let knot_hash data =
            lengths |> Seq.fold(fun (pos, skip_size, data) length ->
            let updated_data = rev_section pos length data
            let updated_pos = pos + length + skip_size
            let updated_skip_size = skip_size + 1
            (updated_pos, updated_skip_size, updated_data)
            ) (0, 0, data)
    
    let ans1 =
        let _, _, result = knot_hash data
        result |> Seq.take 2 |> Seq.reduce((*))
    ans1
    
let part2() =
    let lengths = ((inp |> Seq.fold(fun acc x -> acc + (acc = "") ? ("", ",") + (int x).ToString()) "").Split "," |> Seq.map(int),
                   [17;31;73;47;23]) ||> Seq.append
    
    let data = seq {for i in 0..255 -> i} |> Seq.toArray
    
    let knot_hash pos skip_size data =
            lengths |> Seq.fold(fun (pos, skip_size, data) length ->
            let updated_data = rev_section pos length data
            let updated_pos = pos + length + skip_size
            let updated_skip_size = skip_size + 1
            (updated_pos, updated_skip_size, updated_data)) (pos, skip_size, data)
                  
                  
    let _, _, sparse_hash = seq {for i in 0..63 -> i} |> Seq.fold(fun (pos, skip_size, data) _ -> knot_hash pos skip_size data) (0, 0, data)
    let dense_hash = sparse_hash |> Array.splitInto 16 |> Seq.map(fun arr -> arr |> Seq.reduce((^^^)))
    let result = dense_hash |> Seq.map(fun x -> System.String.Format("{0:X2}", x).ToLower()) |> Seq.reduce((+))
    result

let solve() =
    printfn $"%A{part1()}"
    printfn $"%A{part2()}"
    
    0
