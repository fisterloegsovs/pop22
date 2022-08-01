(* requires 64-bit words! *)

signature BOARD = sig
  eqtype t
  datatype dir = UP | DOWN | LEFT | RIGHT
  type pos = int * int

  val init      : unit -> t
  val sets      : pos list -> t
  val peg       : t -> pos -> bool
  val valid     : pos -> bool
  val neighbor  : pos -> dir -> pos option
  val mv        : t -> pos -> dir -> t option
  val pegcount  : t -> int
  val pegcenter : t
  val pr        : t -> string
end

structure Board :> BOARD = struct
fun set (w:word) (i:int) (b:bool) : word =
    let val w1 = Word.<<(0w1,Word.fromInt i)
    in if b then Word.orb(w,w1)
       else Word.andb(w,Word.notb w1)
    end

fun get (w:word) (i:int) : bool =
    Word.andb(0w1,Word.>>(w,Word.fromInt i)) = 0w1

type t = word

datatype dir = UP | DOWN | LEFT | RIGHT
type pos = int * int

  (*
       0 1 2 3 4 5 6
     0     * * *
     1     * * *
     2 * * * * * * *
     3 * * * o * * *
     4 * * * * * * *
     5     * * *
     6     * * *

  *)

infix ==>
fun x ==> y = not x orelse y

fun valid (r,c) : bool =
    r >= 0 andalso c >= 0 andalso
    r <= 6 andalso c <= 6 andalso
    ((r < 2 orelse r > 4) ==> (c >= 2 andalso c <= 4)) andalso
    ((c < 2 orelse c > 4) ==> (r >= 2 andalso r <= 4))

fun posi ((r,c):pos) : int = r*7+c

fun peg (t:t) (p:pos) : bool =
    get t (posi p)

fun neighbor ((r,c):pos) (d:dir) : pos option =
    let fun chk p = if valid p then SOME p else NONE
    in chk (case d of
                UP => (r-1,c)
              | DOWN => (r+1,c)
              | LEFT => (r,c-1)
              | RIGHT => (r,c+1))
    end

fun mv (t:t) (p:pos) (d:dir) : t option =
    if peg t p then
      case neighbor p d of
          SOME p' =>
          if peg t p' then
            case neighbor p' d of
                SOME p'' =>
                if peg t p'' then NONE
                else let val t1 = set t (posi p) false
                         val t2 = set t1 (posi p') false
                         val t3 = set t2 (posi p'') true
                     in SOME t3
                     end
              | NONE => NONE
          else NONE
        | NONE => NONE
    else NONE

fun sets ps =
    List.foldl (fn (p,t) => set t (posi p) true) 0w0 ps

fun init () : t =
    sets [            (0,2),(0,3),(0,4),
                      (1,2),(1,3),(1,4),
          (2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),
          (3,0),(3,1),(3,2),      (3,4),(3,5),(3,6),
          (4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),
                      (5,2),(5,3),(5,4),
                      (6,2),(6,3),(6,4)             ]

  fun pr (t:t) : string =
      let val lines =
              List.foldl (fn (r,acc) =>
                             let val line =
                                     List.foldl (fn (c,acc) =>
                                                    if get t (posi (r,c)) then acc^" *" else acc^"  ")
                                                "" [0,1,2,3,4,5,6]
                             in line::acc
                             end) nil [0,1,2,3,4,5,6]
      in String.concatWith "\n" (rev lines)
      end

  fun pegcount (b:t) : int =
      let fun iter b i a =
              if i < 0 then a
              else iter (Word.>>(b,0w1)) (i-1)
                        (if Word.andb(b,0w1) = 0w1 then a+1 else a)
      in iter b 49 0
      end

  val pegcenter : t = set 0w0 (posi (3,3)) true
end

structure Main = struct

structure B = Board

fun move i p d =
    case B.mv i p d of
        SOME i => i
      | NONE => raise Fail "illegal move"

fun show i = print (B.pr i ^ "\n")

type move = B.pos * B.dir

val mv0 : move = ((0,0), B.RIGHT)

fun nextd (d:B.dir) : B.dir option =
    case d of B.RIGHT => SOME B.DOWN
            | B.DOWN => SOME B.LEFT
            | B.LEFT => SOME B.UP
            | B.UP => NONE

fun nextmv ((r,c),d) =
    case nextd d of
        SOME d => SOME ((r,c),d)
      | NONE => if c < 6 then SOME ((r,c+1),B.RIGHT)
                else if r < 6 then SOME ((r+1,0),B.RIGHT)
                else NONE

type s = B.t * move list

fun find P (b,mvs) ((p,d):move) : s option =
    let fun maybenext () =
            case nextmv (p,d) of
                SOME (p,d) => find P (b,mvs) (p,d)
              | NONE => NONE
    in case B.mv b p d of
           NONE => maybenext()
         | SOME b' =>
           if P b' then SOME (b',rev((p,d)::mvs))
           else case find P (b',(p,d)::mvs) mv0 of
                    SOME s => SOME s
                  | NONE => maybenext()
    end

fun pr_pos (r,c) = Int.toString r ^ ":" ^ Int.toString c

fun pr_dir dir = case dir of
                     B.UP => "^"
                   | B.DOWN => "v"
                   | B.LEFT => "<"
                   | B.RIGHT => ">"

fun pr_mv (p,d) = pr_pos p ^ pr_dir d

fun pr_mvs mvs = String.concatWith "," (map pr_mv mvs)

fun show_s (i,mvs) = (show i; print(pr_mvs mvs ^ "\n"))

val () =
    let fun P b = b = B.pegcenter
    in case find P (B.init(),[]) mv0 of
           SOME s => show_s s
         | NONE => print "** No solutions...\n"
    end
end
