module aoc_2017.day17

open System.Collections.Generic
open System.Linq

[<Literal>]
let nSpins = 50000000

[<Literal>]
let inp = 337


let solve () =

    let ll = LinkedList()
    let head = ll.AddFirst 0
    
    let mutable llCount = ll.Count
    
    let insertNode2 (valueToInsert: int) (currNodePos : int) =
        let insertPos = (inp + currNodePos) % llCount
        let insertedNodePos = insertPos + 1
        
        llCount <- llCount + 1
        
        if insertedNodePos = 1 then
            let insertedNode = ll.AddAfter(head, valueToInsert)
            (insertedNode, insertedNodePos)
        else
            (head, insertedNodePos)
    
    let insertNode (valueToInsert: int) (currNode : LinkedListNode<int>) (currNodePos : int) =
        let insertPos = (inp + currNodePos) % ll.Count
        
        let offsetFromCurrNode = (currNode, (insertPos - currNodePos))
        let offsetStart = (ll.First, insertPos)
        let offsetEnd = (ll.Last, insertPos - ll.Count)
        
        let closestOffsetPivotNode, closestOffsetDistance = [offsetFromCurrNode; offsetStart; offsetEnd] |> Seq.minBy (fun x -> abs (snd x))
        
        let insertPoint =
            if closestOffsetDistance = 0 then
                closestOffsetPivotNode
            else
                Enumerable.Range(0, abs closestOffsetDistance)
                |> Seq.fold
                    (fun (node: LinkedListNode<int>) _ ->
                        if closestOffsetDistance > 0 then
                            if node.Next <> null then node.Next else ll.First
                        else
                            if node.Previous <> null then node.Previous else ll.Last)
                    closestOffsetPivotNode
                    
        let insertedNode = ll.AddAfter(insertPoint, valueToInsert)
        let insertedNodePos = insertPos + 1
            
        (insertedNode, insertedNodePos)
        
    let mutable pc = 0
    
    let ans1 =
        (seq { 0..(2017-1) }
            |> Seq.fold(fun (currNode: LinkedListNode<int>, currNodePos) x -> insertNode (x+1) currNode currNodePos) (head, 0)
            |> fst
        ).Next.Value
        
    
    let ll = LinkedList()
    let head = ll.AddFirst 0
    
    let mutable llCount = ll.Count
    
    // Inserts node only if the node position is 1
    let insertNodePart2 (valueToInsert: int) (currNode : LinkedListNode<int>) (currNodePos : int) =
        let insertPos = (inp + currNodePos) % llCount
        let insertedNodePos = insertPos + 1
        
        llCount <- llCount + 1
        
        if insertedNodePos = 1 then
            let insertedNode = ll.AddAfter(currNode, valueToInsert)
            (insertedNode, insertedNodePos)
        else
            (currNode, insertedNodePos)
    
    let ans2 =
        (seq { 0..(50000000-1) }
        |> Seq.fold(fun (currNode: LinkedListNode<int>, currNodePos) x -> (insertNodePart2 (x+1) currNode currNodePos)) (head, 0)
        |> fst).Value
        
    printfn $"%A{ans1}, %A{ans2}"

    ()
