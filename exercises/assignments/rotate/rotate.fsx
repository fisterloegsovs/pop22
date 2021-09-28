let TEST = false;

let A = ['a' .. 'p']

let random = System.Random()

let rec randomPermute lst =
    match lst with
        | [] -> []
        | _ ->
            let i = random.Next (lst.Length - 1)
            lst.[i] :: randomPermute (lst.[..i-1]@lst.[i+1..])

let rec printLines lst =
    match lst with
        | [] -> Some ()
        | _ ->
            printfn "%2c%2c%2c%2c" lst.[0] lst.[1] lst.[2] lst.[3]
            printLines lst.[4..]

let rotate (lst : char list) (i : int) : char list option =
    match i with
        |1 |2 |3 |5 |6 |7 |9 |10 |11 ->
            // Permute as follows: i -> i2, i2 -> i3, i3-> i4, i4->i
            let j = i - 1
            let j2 = j+1
            let j3 = j+5
            let j4 = j+4
            Some (lst.[..j-1]@[lst.[j4]; lst.[j]]@lst.[j2+1..j4-1]@[lst.[j3]; lst.[j2]]@lst.[j3+1..]);
        | _ -> None

let flip f a b = f b a
let altRotate = flip rotate
let altRotateOption i =  Option.bind (altRotate i)

if TEST then
    for i = 1 to 12 do
        printfn "%d" i
        rotate A i |> Option.bind printLines |> ignore
        printfn "Check rotation symmetry"
        let A1 = Some A |> altRotateOption i;
        printf " (1, %b)" (Some A = A1)
        let A2 = A1 |> altRotateOption i;
        printf " (2, %b)" (Some A = A2)
        let A3 = A2 |> altRotateOption i;
        printf " (3, %b)" (Some A = A3)
        let A4 = A3 |> altRotateOption i;
        printfn " (4, %b)" (Some A = A4)

let rec userLoop lst stop =
    lst |> Option.bind printLines |> ignore
    let i = int (System.Console.ReadLine ())
    match i with
        | 0 -> ()
        | _ ->
            let C = lst |> altRotateOption i
            if lst = stop then ()
            else userLoop C stop

printfn "Given a random permutation of letters, enter coordinates to rotate until the puzzle is solved as:"
printLines A
printfn "Enter coordinates to rotate until the puzzle is solved."
printfn "The coordinates are as follows:"
printfn " 1  2  3  4\n 5  6  7  8\n 9 10 11 12\n13 14 15 16"
printfn "where 4, 8, 9, and 13-16 are invalid."
printfn "Enter 0 to quite."

userLoop (Some (randomPermute A)) (Some A)
