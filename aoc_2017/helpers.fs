module aoc_2017.helpers

open System.Text

let smapi x = x |> Seq.mapi(fun i x -> (i,x))

let lw (a,b) = (lazy a, lazy b)
let (?) is_true (a, b) = if is_true then a else b
let ($) is_true (a : Lazy<'a>, b : Lazy<'a>) =
    if is_true then a.Force() else b.Force()

let perm_string (data: string) =
    let swap (i: int) (j: int) (data: StringBuilder) =
        let temp = data[i]
        data[i] <- data[j]
        data[j] <- temp
        
    let mutable acc = List.empty
    
    let rec perm k (dsb: StringBuilder) =
        if k = 1 then
            acc <- dsb.ToString() :: acc
        else
            let sb_copy = StringBuilder().Append(dsb)
            perm (k-1) sb_copy
            
            for i = 0 to (k-1) do
                swap ((k%2 = 0) ? (i, 0)) (k-1) dsb
                    
                perm (k-1) dsb
            
    perm data.Length (StringBuilder(data))
    acc
    
let perm (data: 'a seq) =
    let swap (i: int) (j: int) (data: 'a array) =
        let temp = data[i]
        data[i] <- data[j]
        data[j] <- temp
        
    let mutable acc = List.empty
    
    let rec perm k data =
        if k = 1 then
            acc <- data :: acc
        else
            perm (k-1) (data |> Seq.toArray)
            
            for i = 0 to (k-1) do
                swap ((k%2 = 0) ? (i, 0)) (k-1) data
                    
                perm (k-1) data
            
    perm (data |> Seq.length) (data |> Seq.toArray)
    acc |> List.toSeq
