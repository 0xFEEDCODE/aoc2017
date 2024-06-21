module aoc_2017.day4
open helpers

open System.Collections.Generic
open System.IO

let solve () =

    let inp = File.ReadAllLines "day4.txt"
                  
    let rec distribute item = function
        | [] -> [[item]]
        | head::tail as lst -> (item::lst) :: [for tail' in distribute item tail -> head::tail']
        
    let rec permute = function
        | [] -> [[]]
        | e::xs -> List.collect (distribute e) (permute xs)
        
    let valid_count =
        inp
        |> Seq.where (fun phrase ->
            let dict = Dictionary()

            phrase.Split " "
            |> Seq.iter (fun word ->
                if not (dict.ContainsKey word) then
                    dict[word] <- 0
                dict[word] <- dict[word] + 1
                
                let anagrams = perm_string word
                for anagram in anagrams do
                    if word <> anagram then
                        if (dict.ContainsKey anagram) then
                            dict[anagram] <- dict[anagram] + 1
               )

            seq { for v in dict.Values -> v } |> Seq.forall (fun v -> v = 1))
        |> Seq.length

    printfn $"%A{valid_count}"

    0
