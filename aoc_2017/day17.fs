module aoc_2017.day17

open System
open System.Collections.Generic
open System.Linq

[<Literal>]
let nSpins = 50000000

[<Literal>]
let inp = 337

let solve () =

    let ll = LinkedList()
    let head = ll.AddFirst 0
    
    //1,2,4,7,16,33,146,483
    //1,2,3,9,17,113
    let mutable px = 0
    let mutable pc = 0

    let r =
        seq { 0..10000000 }
        |> Seq.scan
            (fun (currNode: LinkedListNode<int>, currNodePos) x ->
                let insertPos = (inp + currNodePos) % ll.Count
                let offset = (insertPos - currNodePos) 

                let insertPoint =
                    if offset = 0 then
                        currNode
                    else
                        Enumerable.Range(0, abs offset)
                        |> Seq.fold
                            (fun (node: LinkedListNode<int>) _ ->
                                if offset > 0 then
                                    if node.Next <> null then node.Next else ll.First
                                else
                                    if node.Previous <> null then node.Previous else ll.Last)
                            currNode
                            
                let valueToInsert = x + 1
                let insertedNodePos = insertPos + 1
                let insertedNode = ll.AddAfter(insertPoint, valueToInsert)
                
                pc <- pc+1
                if (insertedNodePos = 1) then
                    printfn $"IA: {pc}, V: {valueToInsert}"
                    pc <- 0
                    
                
                (*
                if(ll.Count > 1) then
                    if((ll.Count - ll.First.Next.Value) = 1) then
                        printfn $"%A{(ll.Count, ll.Last.Value)}"
                        *)
                
                (*
                printfn "____"
                let mutable it = ll.First

                let mutable c = 0
                pc <- pc+1
                while (it <> null) do
                    printf "%A " it.Value
                    c <- c+1
                    it <- it.Next
                    (*
                    if(c = 1) then
                        if px <> it.Value then
                            px <- it.Value
                            //printfn $"Change %A{(pc, ll.First.Next.Value, ll.Count)}"
                            pc <- 0
                            *)

                *)
                (insertedNode, insertedNodePos))
            (head, 0)
        |> Seq.toArray

    //printfn "%A" (r |> Seq.iter(fun (x,y) -> printfn "%A" x.Value))
    
    printfn "%A" (ll.First.Next.Value)

    ()
