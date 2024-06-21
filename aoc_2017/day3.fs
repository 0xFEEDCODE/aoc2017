module aoc_2017.day3

let inp = 368078

type Point = int * int

type Corner =
    | Right
    | Top
    | Left
    | Bottom
    | Next

let solve() =
    let get_mhd ((y1, x1): Point) ((y2, x2): Point) = abs (x1 - x2) + abs (y1 - y2)
    
    let exp_int x exp = ((x |> float) ** exp) |> int
    
    //let rec calc_pattern x = if x > 2 then (exp_int 2 x) + (calc_pattern (x - 1)) else x
    let rec calc_pattern x = if x <= 0 then 2 else (8 * x) + calc_pattern (x-1)
    let determine_corner l h x =
        match (floor ( ((x - l) / ((h - l) / 4)) |> float) |> int) with
        | 0 -> Right | 1 -> Top | 2 -> Left | 3 -> Bottom | 4 -> Next
                                                          | _ -> failwith "fu"
                                                          
    let get_pos l h x =
        let corner_len = (h - l) / 4
        match determine_corner l h x with
        | Right ->
            (l - x, 0)
        | Top ->
            ((corner_len * -1) + 1, (corner_len - (x-l))-1)
        | Left ->
            let y = (x - (l + (corner_len*3)-2))
            (y, (corner_len * -1))
        | Bottom ->
            (1, (x - (l+(corner_len*4)-1)))
        | Next -> (1, 1)
    
    (*
    printfn "%A" (calc_pattern 4)
    
    let l = calc_pattern 3
    let h = calc_pattern 4
    
    for i in l..h do
        let (y,x) = get_pos l h i
        if i = 23 then
            ()
        if 1 <> 1 then
            printfn "%A" ((i, (y,x)))
        else
            Console.SetCursorPosition (10 + 2 + x, 10 + 3 + y)
            printf "x"
            Threading.Thread.Sleep 100
        //printfn "%A" (i, (determine_corner l h i), get_pos l h i)
    
    exit(0)
    *)
    
    
    let inp = 368078
    let r = Seq.initInfinite id
            |> Seq.map(fun i -> (i, calc_pattern i))
            |> Seq.takeWhile(fun (_, x) -> x < inp) |> Seq.last
            
    (*
    Console.SetBufferSize (1024, 1024)
    Console.Clear()
    *)
    let l = snd r
    let h = calc_pattern (fst r + 1)
    let pos = get_pos l h inp
            
    (*
    
    let r = Seq.initInfinite id
            |> Seq.map(fun i -> (i, calc_pattern i))
            |> Seq.takeWhile(fun (_, x) -> x < inp)
    r |> Seq.iter(printfn "%A") 
    exit(0)
    *)
            
    (*
    for i in 0..6 do
        let rfst = fst (r |> Seq.item i)
        let l = snd (r |> Seq.item i)
        let h = calc_pattern (fst (r |> Seq.item i) + 1)
        //printfn "%A" (l, h)
        
        for i in l..h do
            let y,x = get_pos l h i
            let bound = 200
            let (sy, sx) = (-1 + rfst+1, -1 + rfst+2)
            Console.SetCursorPosition (sx + bound + x, sy + bound + y)
            printf "x"
            Threading.Thread.Sleep 50
            *)
    
    
    (*
    let start_pos = (-1 + ((fst r)+1), -1 + ((fst r)+2))
    
    let target_pos = ((fst start_pos + fst pos), (snd start_pos + snd pos))
    printfn "%A" (l, h, start_pos, pos, target_pos, (get_mhd (0,0) target_pos))
    
    *)
    let a2 = Array2D.zeroCreate 1024 1024
    
    let sy, sx = ((a2 |> Array2D.length1) / 2, (a2 |> Array2D.length2) / 2)
    a2[sy,sx] <- 1
    
    let get_neigh_values (y: int) (x: int) = seq {(y-1, x-1); (y+1, x+1); (y-1, x+1); (y+1, x-1); (y+1, x); (y-1, x); (y, x+1); (y, x-1); }
                                             |> Seq.where(fun (y, x) -> y > 0 && x > 0
                                                                        && y < (a2 |> Array2D.length1)
                                                                        && x < (a2 |> Array2D.length2) && a2[y, x] <> 0)
                                             |> Seq.map(fun (y, x) -> a2[y,x])
                                             
    let patt = Seq.initInfinite id
            |> Seq.map(fun i -> (i, calc_pattern i))
            |> Seq.takeWhile(fun (i, x) -> i < 5 && x < inp)
    
    for (i, low_bound) in patt do
        
        //printfn "%A" l_boundary
        
        let high_bound = calc_pattern (i+1)
        let start_pos = ((i+ sy), (i+ sx) + 1)
        
        for j in low_bound..high_bound do
            let pos = get_pos low_bound high_bound j
            let x, y = (fst start_pos + fst pos, snd start_pos + snd pos)
            a2[y,x] <- get_neigh_values y x |> Seq.sum
            if (a2[y,x] > inp )  then printfn "%A" (a2[y,x]) 
            //printfn $"%A{a2[y, x]}"
            
            (*
            Console.SetCursorPosition (x-300, y-300)
            printf "x"
            Threading.Thread.Sleep 50
            *)
            
            
        
    
    
    
    0
    

