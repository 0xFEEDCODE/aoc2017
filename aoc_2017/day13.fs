module aoc_2017.day13

open System
open System.IO
open System.Linq
open helpers

type Dir =
    | Up
    | Down

[<CustomEquality; NoComparison>]
type State =
    { Range: int
      ScannerPos: int
      Direction: Dir }

    override this.Equals other =
        match other with
        | :? State as other -> this.Range = other.Range && this.ScannerPos = other.ScannerPos
        | _ -> false
        
    member this.GetScannerPos time =
        // "Sawtooth wave" modulo
        // Ex: range 4 -> 0,1,2,3,2,1,0 ...
        let range = this.Range - 1
        let cycleLen = range * 2
        let pos = time % cycleLen
        (pos <= range) ? (pos, cycleLen - pos)

let solve () =
    let inp = File.ReadAllLines "day13.txt"
    
    let firewall =
        inp
        |> Seq.fold
            (fun map line ->
                let split = line.Split ":" |> Seq.map Int32.Parse |> Seq.toArray

                map
                |> Map.add
                    (split[0])
                    { Range = split[1]
                      ScannerPos = 0
                      Direction = Down })
            Map.empty

    let traverse delay =
        Enumerable.Range(firewall.Keys.Min(), firewall.Keys.Max() + 1)
        |> Seq.fold
            (fun penaltySoFar depth ->
                let time = delay + depth
                let maybePenalty =
                    match (firewall |> Map.tryFind depth) with
                    | Some(state) ->
                        if ((state.GetScannerPos time) = 0) then
                            Some(depth * state.Range)
                        else
                            None
                    | None -> None

                let updatedPenaltyScore =
                    match (penaltySoFar, maybePenalty) with
                    | Some p1, Some p2 -> Some(p1 + p2)
                    | None, Some(p2) -> Some(p2)
                    | Some p1, None -> Some(p1)
                    | _ -> None

                updatedPenaltyScore)
            None

    printfn $"%A{traverse 0}\n"
    
    id
    |> Seq.initInfinite
    |> Seq.map traverse
    |> Seq.mapi (fun i x -> (i, x))
    |> Seq.takeWhile (fun (_, x) -> x.IsSome)
    |> Seq.last
    |> fst
    |> (+) 1
    |> printfn "%A"
