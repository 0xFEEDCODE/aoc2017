module aoc_2017.day18

open System
open System.Collections.Generic
open System.IO
open System.Linq

type Int64 with
    member x.modulo m = if x < 0 then (x%m + m) % m else x % m
    
type Register = string
type RegisterOrValue =
    | Register of Register
    | Value of int64
    
    static member Cast (x: string) =
        match Int64.TryParse(x) with
        | true, n -> Value(n)
        | false, _ -> Register(x)
    
type RegistryModifier = Register * RegisterOrValue

type Instruction =
    | Snd of Register
    | Rcv of Register
    | Set of RegistryModifier
    | Add of RegistryModifier
    | Mul of RegistryModifier
    | Mod of RegistryModifier
    | Jgz of RegisterOrValue * RegisterOrValue 
    | SndPt2 of RegisterOrValue
    | RcvPt2 of Register
    
type Program(instructions : Instruction array) as this =
    let instructions = instructions
    [<DefaultValue>] val mutable Registers : Dictionary<Register, int64>
    do
        this.Registers <- Dictionary<Register, int64>()
        
    let mutable pc : int = 0
    let incrementPcBy x = pc <- pc + x
    let incrementPc() = pc <- pc + 1
    member this.GetProgramCounter
        with get() = pc
        
    let mutable isHalted = false
    let halt() =
        isHalted <- true
        pc <- -1
        
    member this.IsHalted
        with get() = isHalted
        
    let mutable lastSoundPlayed = 0L
    member this.GetLastSoundPlayed
        with get() = lastSoundPlayed
    
    let mutable receivedMessages: int64 array = [||]
    member this.GetReceivedMessage
        with get() = receivedMessages
        
    let queue = Queue()
    member this.AddToQueue x =
        queue.Enqueue x
        
    member this.GetFromQueue
        with get() = queue.Dequeue()
        
    member this.IsQueueEmpty
        with get() = not (queue.Any())
    
    let mutable messageSentCount = 0
    member this.GetMessageSentCount
        with get() = messageSentCount
        
    let mutable isWaitingForMessage = false
    member this.IsWaitingForMessage
        with get() = isWaitingForMessage
        
    let getRegister x =
        this.Registers[x]
        
    let setRegister x y = this.Registers[x] <- y
    
    let getValue regOrValue =
        match regOrValue with
        | Value v -> v
        | Register r -> getRegister r
    
    member this.ProcessInstructionAtProgramCounterPt1()  =
        match instructions[pc] with
        | Snd x -> lastSoundPlayed <- (getRegister x); incrementPc()
        | Rcv x ->
            if(getRegister x <> 0L) then
                halt()
            else
                incrementPc()
        | Set (x, y) -> setRegister x (getValue y); incrementPc() 
        | Add (x, y) -> setRegister x ((getRegister x) + getValue y); incrementPc()
        | Mul (x, y) -> setRegister x ((getRegister x) * getValue y); incrementPc()
        | Mod (x, y) -> setRegister x ((getRegister x).modulo(getValue y)); incrementPc()
        | Jgz (x, y) ->
            if ((getValue x) > 0L) then
                incrementPcBy ((getValue y) |> int)
            else
                incrementPc()
        | _ -> failwith "wtf"
            
    member this.ProcessInstructionAtProgramCounterPt2 (otherProgram: Program) =
        match instructions[pc] with
        | SndPt2 x ->
            messageSentCount <- messageSentCount + 1
            let value = getValue x
            otherProgram.AddToQueue value
            incrementPc()
        | RcvPt2 x ->
            isWaitingForMessage <- this.IsQueueEmpty
            
            // Waiting for message - PC not incremented until other program has message
            if isWaitingForMessage then
                if otherProgram.IsWaitingForMessage && otherProgram.IsQueueEmpty then
                    halt()
                ()
            else
                let value = this.GetFromQueue
                setRegister x value
                incrementPc()
        | Set (x, y) -> setRegister x (getValue y); incrementPc()
        | Add (x, y) -> setRegister x ((getRegister x) + getValue y); incrementPc()
        | Mul (x, y) -> setRegister x ((getRegister x) * getValue y); incrementPc()
        | Mod (x, y) -> setRegister x ((getRegister x).modulo(getValue y)); incrementPc()
        | Jgz (x, y) ->
            if ((getValue x) > 0L) then
                incrementPcBy ((getValue y) |> int)
            else
                incrementPc()
        | _ -> failwith "wtf"

let solve () =
    let inp = File.ReadAllLines "day18.txt"
    let instructions = 
        inp |> Seq.map(fun line ->
            let spl = line.Split " "
            let instrName = spl[0] 
            let arg1 = spl[1] 
            let arg2 = RegisterOrValue.Cast(if spl.Length > 2 then spl[2] else "")
            match instrName with
            | "snd" -> Snd arg1
            | "rcv" -> Rcv arg1
            | "set" -> Set (RegistryModifier(arg1, arg2))
            | "add" -> Add (RegistryModifier(arg1, arg2))
            | "mul" -> Mul (RegistryModifier(arg1, arg2))
            | "mod" -> Mod (RegistryModifier(arg1, arg2))
            | "jgz" -> Jgz ((RegisterOrValue.Cast(arg1), arg2))
            )
        |> Seq.toArray
        
    let program = Program(instructions)
    
    while not program.IsHalted do
        program.ProcessInstructionAtProgramCounterPt1()
    //pt1 ans
    printfn $"%A{program.GetLastSoundPlayed}"
        
    //pt2 start
    let instructions = 
        inp |> Seq.map(fun line ->
            let spl = line.Split " "
            let instrName = spl[0] 
            let arg1 = spl[1] 
            let arg2 = RegisterOrValue.Cast(if spl.Length > 2 then spl[2] else "")
            match instrName with
            | "snd" -> SndPt2 (RegisterOrValue.Cast(arg1))
            | "rcv" -> RcvPt2 arg1
            | "set" -> Set (RegistryModifier(arg1, arg2))
            | "add" -> Add (RegistryModifier(arg1, arg2))
            | "mul" -> Mul (RegistryModifier(arg1, arg2))
            | "mod" -> Mod (RegistryModifier(arg1, arg2))
            | "jgz" -> Jgz ((RegisterOrValue.Cast(arg1), arg2))
            )
        |> Seq.toArray
        
    let p0 = Program(instructions)
    let p1 = Program(instructions)
    p0.Registers.Add("p", 0L)
    p1.Registers.Add("p", 1L)
    
    let progs = [|p0; p1|]
    
    let rec processUntilDeadlock (n: int) =
        let currProg = progs[n]
        let otherProg = progs[(n+1)%progs.Length]
        currProg.ProcessInstructionAtProgramCounterPt2 otherProg
                
        if(currProg.IsHalted && otherProg.IsHalted) then
            ()
        else if (not otherProg.IsHalted) && currProg.IsWaitingForMessage then
            processUntilDeadlock((n+1)%progs.Length)
        else
            processUntilDeadlock(n)
    
    processUntilDeadlock(0)
        
    printfn $"%A{p0.GetMessageSentCount}"
    printfn $"%A{p1.GetMessageSentCount}"
    ()
