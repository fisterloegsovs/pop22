
type pitchclass = C | Cis | D | Dis | E | F | Fis
                | G | Gis | A | Ais | B

type octave = int
type pitch = pitchclass * octave
type duration = int

type music = Note of duration * pitch
           | Rest of duration

type melody = music list

let pitchclassToString pit =
    match pit with
    | C   -> "c"
    | Cis -> "cis"
    | D   -> "d"
    | Dis -> "dis"
    | E   -> "e"
    | F   -> "f"
    | Fis -> "fis"
    | G   -> "g"
    | Gis -> "gis"
    | A   -> "a"
    | Ais -> "ais"
    | B   -> "b"

(* There has got to be a better way *)

let pitchclassToInt pit =
    match pit with
    | C   -> 0
    | Cis -> 1
    | D   -> 2
    | Dis -> 3
    | E   -> 4
    | F   -> 5
    | Fis -> 6
    | G   -> 7
    | Gis -> 8
    | A   -> 9
    | Ais -> 10
    | B   -> 11

(* There has got to be a better way *)

exception Domain

let intToPitchclass n =
    match n with
    | 0  -> C
    | 1  -> Cis
    | 2  -> D
    | 3  -> Dis
    | 4  -> E
    | 5  -> F
    | 6  -> Fis
    | 7  -> G
    | 8  -> Gis
    | 9  -> A
    | 10 -> Ais
    | 11 -> B
    | _  -> raise Domain


let rec octaveToString oct =
    if (oct = 0) then string ""
    elif (oct > 0) then string "'" + octaveToString (oct - 1)
    else string "," + octaveToString (oct + 1)

let pitchToString p =
    let (pit, oct) = p
    pitchclassToString pit + octaveToString oct

let musicToString mus =
    match mus with
    | Note(dur, pit) -> pitchToString pit + string dur
    | Rest(dur)             -> string 'r'+ string dur

let rec melodyToString notes =
    match notes with
    | []   -> string ""
    | h::t -> musicToString h + string " " + melodyToString t

type abspitch = int

let absolutePitch pit =
    let (pc, oct) = pit
    pitchclassToInt pc + oct * 12

let modulo n m = ((n % m) + m) % m

let absToPitch n =
    let r = modulo n 12
    let d = (n - r) / 12
    (intToPitchclass r, d)

let transposePitch n mus =
    match mus with
    | Note(dur, pit) -> Note(dur, absToPitch ((absolutePitch pit) + n))
    | Rest(dur)      -> Rest(dur)

let rec transpose n mel =
    match mel with
    | []   -> []
    | h::t -> (transposePitch n h)::(transpose n t)

let rec maxPitchHelper mel =
    match mel with
    | []                -> (false, 0)
    | Note(dur, pit)::t ->
        let p1 = absolutePitch pit
        let (b, p2) = maxPitchHelper t
        if (b && p2 > p1) then (true, p2)
        else (true, p1)
    | Rest(dur)::t      -> maxPitchHelper t

let maxPitch mel =
    let (b, p) = maxPitchHelper mel
    if (b) then absToPitch p
    else raise Domain

type rational = int * int

let rec gcd r =
    match r with
    | (0, n) -> n
    | (m,n)  -> gcd (n % m, m)

let reduce r =
    let (a, b) = r
    let d = gcd r
    (a/d, b/d)

let radd r1 r2 =
    let (a, b) = r1
    let (c, d) = r2
    (a*d + b*c, b*d)

let rec duration mel =
    match mel with
    | []   -> (0,1)
    | h::t ->
        let d1 = duration t
        match h with
        | Note(dur, pit) ->
            let d2 = (1,dur)
            reduce (radd d1 d2)
        | Rest(dur)      ->
            let d2 = (1,dur)
            reduce (radd d1 d2)

[<EntryPoint>]
let main _ =
    // 4G1
    printfn "%s" (musicToString (Note(1, (A, -3))))
    // 4G2
    let mel1 = [Note(16, (A, 0));Note(16, (B, 0));Note(4, (C, 1));Note(8, (C, 1))]
    let mel2 = [Rest(16);Rest(4)]
    printfn "%s" (melodyToString mel1)
    // 4G3
    printfn "%d" (absolutePitch (Cis,-2))
    // 4G4
    printfn "%d" (absolutePitch (absToPitch -24))
    printfn "%d" (absolutePitch (absToPitch 0))
    printfn "%d" (absolutePitch (absToPitch 15))
    // 4G5
    printfn "%s" (melodyToString (transpose 2 mel1))
    // 4G6
    printfn "%s" (pitchToString (maxPitch mel1))
    // 4G7
    let (a, b) = duration mel1
    printfn "duration %d/%d" a b
    let (a, b) = duration mel2
    printfn "duration %d/%d" a b
    0