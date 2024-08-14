module aoc_2017.day14

open System
open helpers

type Coord = { Row: int; Col: int }

let getGridItemAtCoord (grid: 'a array array) (coord: Coord) =
    let gridRowLen = grid |> Seq.length
    let gridColLen = grid[0] |> Seq.length

    match coord with
    | { Row = r; Col = c } when r >= 0 && c >= 0 && r < gridRowLen && c < gridColLen -> Some(grid[r][c])
    | _ -> None

let solve () =
    let inp = "ljoxqyyw"

    let grid =
        seq { 0..127 }
        |> Seq.map (fun i ->
            (day10.generateKnotHashPt2 $"{inp}-{i}")
            |> Seq.map (fun x -> Convert.ToString(Convert.ToInt64(string x, 16), 2).PadLeft(4, '0'))
            |> Seq.collect id
            |> Seq.toArray)
        |> Seq.toArray

    let ans1 =
        grid
        |> Seq.sumBy (fun row -> row |> Seq.sumBy (fun item -> (item = '1')?(1, 0)))

    let getAdjacentGroup (coord: Coord) =
        let getNeighPos c =
            seq {
                { Row = c.Row - 1; Col = c.Col }
                { Row = c.Row + 1; Col = c.Col }
                { Row = c.Row; Col = c.Col - 1 }
                { Row = c.Row; Col = c.Col + 1 }
            }

        let rec findAdjacent coords visited =
            let visited =
                coords
                |> Seq.fold (fun (acc: Map<Coord, int>) coord -> acc.Add(coord, 0)) visited

            let adjacent =
                coords
                |> Seq.map (fun coord -> (getNeighPos coord) |> Seq.map (fun p -> (p, getGridItemAtCoord grid p)))
                |> Seq.collect id
                |> Seq.where (fun (p, item) ->
                    (not (visited |> Map.containsKey p))
                    && match item with
                       | Some(item) -> item = '1'
                       | _ -> false)

            if (adjacent |> Seq.isEmpty) then
                visited
            else
                findAdjacent (adjacent |> Seq.map fst) visited

        findAdjacent (seq { coord }) Map.empty

    let nGroups =
        seq { 0 .. grid.Length - 1 }
        |> Seq.map (fun row ->
            (seq { 0 .. grid[row].Length - 1 })
            |> Seq.map (fun col -> { Row = row; Col = col }))
        |> Seq.collect id
        |> Seq.fold
            (fun (visited: Map<Coord, int>, nGroups) coord ->
                if visited.ContainsKey coord then
                    (visited, nGroups)
                else if ((getGridItemAtCoord grid coord) = Some('1')) then
                    ((getAdjacentGroup coord)
                     |> Seq.fold (fun acc2 x2 -> acc2.Add(x2.Key, 0)) visited,
                     nGroups + 1)
                else
                    (visited, nGroups))
            (Map.empty, 0)
        |> snd

    printfn $"%A{ans1}"
    printfn $"%A{nGroups}"
