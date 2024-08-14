module aoc_2017.day12

open System
open System.Collections.Generic
open System.IO

let solve () =
    let inp = File.ReadAllLines "day12.txt"


    let cMap = Dictionary<'a, 'a seq>()

    inp
    |> Seq.iter (fun x ->
        let split = x.Split "<->"
        cMap[(int split[0])] <- split[1].Split "," |> Seq.map Int32.Parse)

    let getProgramsInGroup groupId =
        id
        |> Seq.initInfinite
        |> Seq.scan
            (fun ((_, visited), toVisit) _ ->
                let visitedNew = toVisit |> Seq.fold (fun (acc: Set<'a>) -> acc.Add) visited
                let toVisitNew =
                    toVisit
                    |> Seq.map (fun x -> cMap[x])
                    |> Seq.collect id
                    |> Seq.where (fun x -> not (visited.Contains(x)))

                ((visited.Count, visitedNew), toVisitNew))
            ((0, Set [ groupId ]), cMap[groupId])
        |> Seq.takeWhile (fun ((prevVisitedCount, visited), _) -> prevVisitedCount <> visited.Count)
        |> Seq.last
        |> fst
        |> snd

    let getGroupCount groupIds =
        groupIds
        |> Seq.map getProgramsInGroup
        |> Seq.map (fun x -> x |> Seq.sort |> Seq.toArray)
        |> Seq.distinct
        |> Seq.length

    printfn "%A" (getProgramsInGroup 0 |> Seq.length)
    printfn "%A" (getGroupCount cMap.Keys)
