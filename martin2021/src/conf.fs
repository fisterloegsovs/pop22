
type time = int * int  // hour, minutes

let rec pad n s =
  if String.length s >= n then s
  else "0" + pad (n-1) s

let pp_time (h,m) =
  pad 2 (h.ToString ()) + ":" + pad 2 (m.ToString())

let (++) (h,m) m1 =
  (h + (m+m1) / 60,
   (m + m1) % 60)

type author = string
type talk = string
type tag = string
type entry = Talk of int * tag * author * talk
           | Session of string
           | Break of int * string

let rec session_minutes es =
  match es with
    | Talk (m,_,_,_) :: es -> m + session_minutes es
    | _ -> 0

let pp_line t1 t2 s =
  pp_time t1 + " -- " + pp_time t2 + ": " + s + "\n"

let rec pp (t:time) (es : entry list) : string =
  match es with
   | [] -> "\n"
   | Session s::es ->
     let m = session_minutes es
     let s = pp_line t (t ++ m) ("*** " + s + " ***")
     in s + pp t es
   | Break (m,s)::es ->
     let s = pp_line t (t ++ m) s
     in "\n" + s + "\n" + pp (t++m) es
   | Talk (m,_,author,talk)::es ->
     let s = pp_line t (t ++ m) (talk + " (" + author + ")")
     in s + pp (t++m) es

let tag t e = "<" + t + ">" + e + "</" + t + ">"
let taga t a e = "<" + t + " " + a + ">" + e + "</" + t + ">"
let pretty_line t1 t2 s =
  tag "dt" (pp_time t1) + tag "dd" s + "\n"

let rec pretty (t:time) (es : entry list) : string =
  match es with
   | [] -> "\n"
   | Session s::es ->
     let m = session_minutes es
     let s = pretty_line t (t ++ m) (tag "b" s)
     in s + pretty t es
   | Break (m,s)::es ->
     let s = pretty_line t (t ++ m) (tag "b" s)
     in "\n" + s + "\n" + pretty (t++m) es
   | Talk (m,tg,author,talk)::es ->
     let talk = if tg = "" then talk
                else taga "a" ("href='#" + tg + "'") talk
     let s = pretty_line t (t ++ m) (talk + " " + tag "i" ("(" + author + ")"))
     in s + pretty (t++m) es

let rec abstracts (t:time) (es: entry list) : string =
  match es with
   | [] -> "\n"
   | Session s::es ->
     let m = session_minutes es
     let s = "#### " + pp_time t + "-" + pp_time (t ++ m) + " &nbsp;" + s + "\n\n"
     in s + abstracts t es
   | Break (m,s)::es -> abstracts (t++m) es
   | Talk (m,"",author,talk)::es -> abstracts (t ++ m) es
   | Talk (m,tg,author,talk)::es ->
     let thetag = taga "a" ("name='" + tg + "'") ""
     let talk = "__" + talk + "__ _(" + author + ")_\n"
     in thetag + talk + "\nTBA.\n\n" + abstracts (t++m) es

let lunch = Break (60,"Lunch")
let coffee30 = Break (30,"Coffee Break")
let coffee15 = Break (15,"Mini Coffee Break")
let welcome s = Break(5,"Welcome (" + s + ")")
let closing s = Break(5,"Closing Remarks (" + s + ")")

let talk a t = Talk(30,"",a,t)

let talk_tag tag a t = Talk(30,tag,a,t)

let program =
     [welcome "Fritz Henglein, DIKU";
      Session "Partial Evaluation and Formalisation";
      talk_tag "mycroft" "Alan Mycroft, U. of Cambridge" "Object-oriented partial evaluation and the expression problem";
      talk_tag "annenkov" "Danil Annenkov, DIKU" "Nominal techniques in Coq";
      coffee30;
      Session "Finance";
      talk_tag "poulsen" "Rolf Poulsen, MATH" "Special FX";
      talk_tag "nordfang" "Maj-Britt Nordfang, MATH" "Household finance";
      talk_tag "benth" "Fred Esben Benth, U. of Oslo" "Modelling stochastic volatility in forward markets";
      lunch;
      Session "Blockchain Technology";
      talk_tag "ross" "Omri Ross, DIKU" "Automated execution of financial contracts on blockchain";
      coffee15;
      Session "Streaming and Life-Insurance on GPUs";
      talk_tag "filinski" "Andrzej Filinski, DIKU" "Streaming data-parallelism";
      talk_tag "biboudis" "Aggelos Biboudis, École Polytechnique Fédérale de Lausanne" "Streaming";
      talk_tag "sestoft" "Peter Sestoft, ITU" "Domain-specific languages and GPGPUs in life insurance and pensions";
      coffee30;
      Session "Data-parallel Programming";
      talk_tag "henriksen" "Troels Henriksen, DIKU" "Design and implementation of the Futhark programming language";
      talk_tag "oancea" "Cosmin Oancea, DIKU" "Futhark: Challenges and Future Research Directions";
      talk_tag "elsman" "Martin Elsman, DIKU" "APL on GPUs--a progress report with a touch of machine learning";
      closing "Fritz Henglein, DIKU";
      Break (60, "Chat and Snacks");
      ]

let program2 =
      [welcome "Fritz Henglein, DIKU";
       Talk (55, "", "HIPERFIT Team", "HIPERFIT highlights---a summary of project results");
       coffee30;
       Session "Invited Talks";
       talk "Anders Kirkeby, VP Enterprise Architecture, SimCorp" "Enterprise fintech opportunities";
       talk "Gitte Christensen, CEO, Dyalog" "Putting parallel processing at the fingertips of the domain expert";
       Session "Panel Discussion";
       Talk(40,"","TBA","Computational finance for the 21. Century");
       closing "Fritz Henglein, DIKU";
       Break(60, "Networking, Snacks, and Vine");
       ]

let p =
  taga "dl" "class='event'"
    (pretty (8,55) program)

let p2 =
  taga "dl" "class='event'"
    (pretty (13,00) program2)

let a =
  "### Abstracts for Talks on November 16, 2017\n\n" + abstracts (8,55) program

do printfn "%s" p

do printfn "%s" a

do printfn "%s" p2
