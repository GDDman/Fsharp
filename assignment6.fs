(* Assignment 6 *) (* Do not edit this line. *)
(* Student name: Josh Liu, Id Number: 260612384 *) (* Edit this line. *)

(* Some basic functions that I like to use.  You can use the raw F# versions
if you want. *)

(* Sticks an item onto a lazy stream *)
let cons x sigma = Seq.append (Seq.singleton x) sigma

(* Grabs the first item from a stream. *)
let first sigma = Seq.nth 0 sigma

(* Removes the first item and gives you the rest of the stream. *)
let rest sigma = Seq.skip 1 sigma

(* Makes a list out of the first n elements of a stream.  Useful for display.  There is
a built-in primitive to do this but I like this better. *)

let rec prefix (n: int) sigma =
  if (n = 0) then []
  else (first sigma) :: (prefix (n - 1) (rest sigma))

(* Question 1 *)

let rec expand num den radix =
  (Seq.delay (fun() -> cons (num*radix / den) (expand ((num*radix) % den) den radix)))

(* Question 2 *)

type term = Term of float * int

(* pown is what you use to raise a float to an integer power. *)
let evalTerm (t: term) (x:float):float =
  match t with
  | Term (c,e) -> c * (pown x e)

(* This is very similar in pattern to evalTerm *)
let integrateTerm (t: term):term =
  match t with
  | Term (c,e) -> Term ((c/(float (e+1))), (e+1))

let rec integrateSeries sigma =
  Seq.delay(fun() -> cons (integrateTerm (first sigma)) (integrateSeries (rest sigma)))

(* Examples for testing. *)
let sigma1 = Seq.initInfinite (fun i -> Term(1.0,i))
let sigma2 = integrateSeries sigma1

let rec expSeries =
  let rec helper t = Seq.delay(fun() -> cons t (helper (integrateTerm t)))
  helper (Term(1.0, 0))
(* Just one line!  Do NOT forget to delay the code. *)

let rec sumSeries (sigma: seq<term>) (x: float) (n: int) : float =
  let pseries = prefix n sigma
  Seq.fold(fun acc i -> acc + (evalTerm i x)) 0.0 pseries
(* Just 2 lines of code! *)
