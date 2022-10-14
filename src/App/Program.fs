open System
open System.Threading
open System.Diagnostics

type Food = {
    Name: string
    Energy: float
    CarbonHydrates: float
    Protein: float
    Fat: float
}

type Event = {
    time: int
    event: string
}

let getName x = x.Name
let getEnergy x = x.Energy
let getCarbonHydrates x = x.CarbonHydrates
let getProtein x = x.Protein
let getFat x = x.Fat

let getAttack x = x.CarbonHydrates
let getDefence x = x.Protein
let getDelayMs x = Convert.ToInt32(((getCarbonHydrates x) + (getProtein x) + (getFat x)) * 1000.0)
let advanceClock x y = x + y

let attack x y :Food = { y with Energy = getEnergy y - (getAttack x - (getDefence y))}
let pickRandomFood x  =
    let rnd = new Random()
    x |> List.item (rnd.Next(2))
let initiative x y = 
    let _x = getDelayMs x
    let _y = getDelayMs y
    if _x > _y then y
    elif _x = _y then pickRandomFood [x; y]
    else x
let order x y = 
    let starter = initiative x y
    if starter = x then (x, y) else (y, x)
let attackInfo clock x y= printf "%i %s attacked %s\n" clock (getName x) (getName y)
let winnerInfo x = getName x + " voitti"

    
//let event f x t = f x 
let sleep (ms: int):unit = Thread.Sleep(ms)
//let addTime t1 t2 = t1 + t2

let checkFighter x =
    if getEnergy x > 0 then true else false

let checkFighters x y = (checkFighter x, checkFighter y)

let fight (x, y) = 
    let mutable clock = 0
    let rec loop x y =
        clock <- (clock + getDelayMs x)
        let ny = attack x y
        attackInfo clock x ny
        match checkFighter ny with
        | false -> winnerInfo x
        | _ ->
            clock <- (clock + getDelayMs ny)
            let nx = attack ny x 
            match checkFighters nx ny with
            | (true, false) -> winnerInfo nx
            | (false, true) -> winnerInfo ny
            | (false, false) -> "molemmat alhaalla"
            | _ ->
                attackInfo clock ny nx
                loop nx ny
    loop x y

let porkkana: Food = { 
    Name = "Porkkana"
    Energy = 33
    CarbonHydrates = 5.6
    Protein = 0.6
    Fat = 0.2
}

let paprika: Food = { 
    Name = "Paprika"
    Energy = 30
    CarbonHydrates = 6
    Protein = 1
    Fat = 0.3
}

let starters = order porkkana paprika
let _fight = fight starters 
printf "%s\n" _fight

type MutFood = {
    Name: string
    mutable Energy: float
    CarbonHydrates: float
    Protein: float
    Fat: float
}


let mutable pork: MutFood = { 
    Name = "Porkkana"
    Energy = 33
    CarbonHydrates = 5.6
    Protein = 0.6
    Fat = 0.2
}

let mutable papr: MutFood = { 
    Name = "Paprika"
    Energy = 30
    CarbonHydrates = 6
    Protein = 1
    Fat = 0.3
}

let mutAttack (x:MutFood) (y:MutFood) = 
    y.Energy <- y.Energy - ( x.CarbonHydrates - (y.Protein))
let mutCheckFighter x =
    if x.Energy <= 0 then false else true
let mutGetDelayMs x = Convert.ToInt32(((x.CarbonHydrates + x.Protein + x.Fat) * 1000.0))

let mutCheckFighters x y = (mutCheckFighter x, mutCheckFighter y)

printf "%f\n" papr.Energy
mutAttack pork papr |> ignore
printf "%f\n" papr.Energy

let cancellationSource = new CancellationTokenSource()



let sleepWorkflowMs ms x y  = async {
    let timer = new Stopwatch()
    timer.Start()
    let rec loop x y =   
        match mutCheckFighters x y with
        | (true, false) -> 
            printfn "%s won" x.Name
            cancellationSource.Cancel()
        | (true, true) -> 
            sleep(ms)
            mutAttack x y
            if mutCheckFighter x = true then printfn "%i %s attacked %s %f\n" timer.ElapsedMilliseconds x.Name y.Name y.Energy
            loop x y
        | _ -> cancellationSource.Cancel()
    loop x y
    }

// Create them
let sleep1 = sleepWorkflowMs (mutGetDelayMs papr) papr pork
let sleep2 = sleepWorkflowMs (mutGetDelayMs pork) pork papr

printfn "Taistelu alkaa\n"
// run them in parallel
[sleep1; sleep2]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore