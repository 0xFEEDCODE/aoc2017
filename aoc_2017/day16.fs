module aoc_2017.day16

open System.IO

type Instruction =
    | Spin of int
    | Exchange of int * int
    | Partner of char * char

let solve () =
    let inp = File.ReadAllText "day16.txt"

    let shiftArr (arr: 'a array) shift =
        let h1 = arr[.. arr.Length - shift - 1]
        let h2 = arr[arr.Length - shift ..]
        (h1 |> Array.append h2)

    let swap (arr: 'a array) p1 p2 =
        let newArr = arr |> Seq.toArray
        newArr[p1] <- arr[p2]
        newArr[p2] <- arr[p1]
        newArr

    let swapLetter arr l1 l2 =
        let p1 = array.FindIndex(arr, (fun l -> l = l1))
        let p2 = array.FindIndex(arr, (fun l -> l = l2))
        swap arr p1 p2

    let comp (a: 'a array) (b: 'a array) =
        a.Length = b.Length
        && seq { 0 .. (a.Length - 1) } |> Seq.forall (fun i -> a[i] = b[i])

    let programsState = { 'a' .. 'p' } |> Seq.toArray

    let instructions =
        inp.Split ","
        |> Seq.map (fun x ->
            match x[0] with
            | 's' -> Spin(int x[1..])
            | 'x' ->
                let spl = x[1..].Split "/"
                Exchange(int spl[0], int spl[1])
            | 'p' ->
                let spl = x[1..].Split "/"
                Partner(char spl[0], char spl[1]))

    let cycle =
        (fun _ -> instructions)
        |> Seq.initInfinite
        |> Seq.scan
            (fun acc instructionSet ->
                instructionSet
                |> Seq.fold
                    (fun localAcc instruction ->
                        match instruction with
                        | Spin(size) -> shiftArr localAcc size
                        | Exchange(n1, n2) -> swap localAcc n1 n2
                        | Partner(l1, l2) -> swapLetter localAcc l1 l2)
                    acc)
            programsState
        |> Seq.mapi(fun i x -> (i,x))
        |> Seq.takeWhile (fun (i, x) -> i = 0 || not (comp x programsState))

    let cycleLen = cycle |> Seq.length
    let oneB = 1000000000
    
    let cycleStart = cycleLen * (oneB / cycleLen)
    let danceN = oneB - cycleStart
    let result = (cycle |> Seq.toArray)[danceN] |> snd |> Seq.fold(fun acc x -> acc + (string x)) ""
    printfn "%A" result
