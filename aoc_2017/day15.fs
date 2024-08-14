module aoc_2017.day15

open System
open System.Net.Http.Headers

let solve () =
    let vA, vB = (783UL, 325UL)
    let fA, fB = (16807UL, 48271UL)

    let divideBy = 2147483647UL

    let genValue (x: uint64) (f: uint64) = (x * f) % divideBy

    let isValidA (x: uint64) = (x % 4UL) = 0UL
    let isValidB (x: uint64) = (x % 8UL) = 0UL

    let firstTwoToTuple (x: 'a seq) =
        (x |> Seq.head, x |> Seq.tail |> Seq.head)

    let compare (a: uint64) (b: uint64) =
        let last16A, last16B =
            (seq {a;b}
             |> Seq.map (fun x ->
                 let binaryFormat = Convert.ToString(int64 x, 2)
                 binaryFormat |> Seq.skip (binaryFormat.Length - 16) |> Seq.toArray))
            |> firstTwoToTuple

        seq { 0..15 } |> Seq.forall (fun i -> last16A[i] = last16B[i])

    let nPairs = 40000000
    let ans1 =
        seq { 0..nPairs }
        |> Seq.fold
            (fun (a, b, count) _ ->
                let newA = (genValue a fA)
                let newB = (genValue b fB)
                let newCount = count + (if compare a b then 1 else 0)
                (newA, newB, newCount))
            (vA, vB, 0)

    printfn $"Pt1 %A{ans1}"

    let nPairs = 5000000
    let generate startingValue factor validationFn =
        id
        |> Seq.initInfinite
        |> Seq.scan (fun acc _ -> (genValue acc factor)) startingValue
        |> Seq.where validationFn
        |> Seq.take nPairs
        |> Seq.toArray

    let cA = generate vA fA isValidA
    let cB = generate vB fB isValidB

    let ans2 =
        seq { 0 .. nPairs - 1 }
        |> Seq.where (fun i -> compare cA[i] cB[i])
        |> Seq.map (fun i -> (cA[i], cB[i]))
        |> Seq.length

    printfn $"%A{ans2}"
    0
