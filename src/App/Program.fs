open System
//open System.Threading

type Food = {
    Name: string
    Energy: float
    CarbonHydrates: float
    Protein: float
    Fat: float
}

type Event = {
    time: float
    event: string
}

let getName x = x.Name
let getEnergy x = x.Energy
let getCarbonHydrates x = x.CarbonHydrates
let getProtein x = x.Protein
let getFat x = x.Fat

let getAttack x = x.CarbonHydrates
let getDefence x = x.Protein
let getDelay x = (getCarbonHydrates x) + (getProtein x) + (getFat x)
let advanceClock x y = x + y

let attack x y :Food = { y with Energy = getEnergy y - (getAttack x - (getDefence y))}
let pickRandomFood x  =
    let rnd = new Random()
    x |> List.item (rnd.Next(2))
let initiative x y = 
    let _x = getDelay x
    let _y = getDelay y
    if _x > _y then y
    elif _x = _y then pickRandomFood [x; y]
    else x
let order x y = 
    let starter = initiative x y
    if starter = x then (x, y) else (y, x)
let attackInfo clock x y= printf "%f %s attacked %s\n" clock (getName x) (getName y)
let winnerInfo x = getName x + " voitti"

    
//let event f x t = f x 
//let sleep (ms: int):unit = Thread.Sleep(ms)
//let addTime t1 t2 = t1 + t2

let checkFighter x =
    if getEnergy x > 0 then true else false

let checkFighters x y = (checkFighter x, checkFighter y)

let fight (x, y) = 
    let mutable clock: float = 0
    let rec loop x y =
        clock <- (clock + getDelay x)
        let ny = attack x y
        attackInfo clock x ny
        match checkFighter ny with
        | false -> winnerInfo x
        | _ ->
            clock <- (clock + getDelay ny)
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