module aoc_2017.day8

open System
open helpers
open System.IO
open System.Collections.Generic

type Condition =
    | LT of int
    | LTE of int
    | EQ of int
    | NE of int
    | GT of int
    | GTE of int
    
    member this.Execute x =
        match this with
        | LT(y)-> x < y
        | LTE(y) -> x <= y
        | EQ(y) -> x = y
        | NE(y) -> x <> y
        | GT(y) -> x > y
        | GTE(y) -> x >= y
        
    static member ParseCondition x =
        match x with
        | "<" -> LT
        | "<=" -> LTE
        | "==" -> EQ
        | "!=" -> NE
        | ">" -> GT
        | ">=" -> GTE
        
type Register = string

type Operation =
    | INC of int
    | DEC of int
    
    member this.Execute x =
        match this with
        | INC(y) -> x + y
        | DEC(y) -> x - y
    
type Instruction = Operation * Register * Condition

let solve() =
    let inp = File.ReadLines "day8.txt"
    
    let registers = Dictionary()
    let instructions = Queue()
    
    inp |> Seq.iter(fun line ->
        let spl = line.Split " "
        let reg = Register(spl[0])
        
        let operation_value = spl[2] |> int
        let operation = match spl[1] with | "inc" -> INC(operation_value) | "dec" -> DEC(operation_value) | _ -> failwith "fu"
        
        let cond_op = spl[5] |> Condition.ParseCondition
        let cond_val = spl[6] |> int
        
        let cond_reg = Register(spl[4])
        let cond = cond_op cond_val
        
        instructions.Enqueue(reg, Instruction(operation, cond_reg, cond))
        if ( not (registers.ContainsKey(reg))) then
            registers[reg] <- 0
        )
    
    let mutable highest_value_registered = Int32.MinValue
    while instructions.Count > 0 do
        let reg, instruction = instructions.Dequeue()
        let op, condition_register, condition = instruction
        if condition.Execute(registers[condition_register]) then
            registers[reg] <- op.Execute registers[reg]
            highest_value_registered <- max (registers[reg]) highest_value_registered
    
    printfn "%A" (registers.Values |> Seq.max)
    printfn "%A" (highest_value_registered)
    0