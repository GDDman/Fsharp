(* Assignment 4 *) (* Do not edit this line. *)
(* Student name: Josh Liu, Id Number: 260612384 *) (* Edit this line. *)

type typExp =
  | TypInt
  | TypVar of char
  | Arrow of typExp * typExp
  | Lst of typExp

type substitution = (char * typExp) list

(* check if a variable occurs in a term *)
let rec occurCheck (v: char) (tau:typExp) : bool =
  match tau with
    | TypInt -> false
    | TypVar c -> if (c = v) then true
                  else false
    | Arrow(a, b) -> (( occurCheck v a) || (occurCheck v b))
    | Lst l -> (occurCheck v l)

(* substitute typExp tau1 for all occurrences of type variable v in typExp tau2 *)
let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
  match tau2 with
    | TypInt -> TypInt
    | TypVar c -> if (c = v) then tau1
                  else TypVar c
    | Arrow(a, b) -> Arrow ((substitute tau1 v a), (substitute tau1 v b))
    | Lst l -> Lst (substitute tau1 v l)

let applySubst (sigma: substitution) (tau: typExp) : typExp =
  List.fold(fun acc x -> substitute (snd x) (fst x) acc) tau sigma

(* This is a one-line program *)

let rec unify (tau1: typExp) (tau2:typExp) : substitution =

  let mutable subs = []
  let mutable stack = [(tau1, tau2)]

  while not(List.isEmpty stack) do
    match stack with
    | [] -> failwith "Stack should be empty"
    | x::xs ->
        stack <- xs
        match x with
        | (exp1, exp2) ->
            if not(exp1 = exp2) then
              match (exp1, exp2) with
              | (TypVar c, _) -> if not(occurCheck c exp2) then
                                   stack <- List.map(fun x -> ((substitute exp2 c (fst x)), (substitute exp2 c (snd x))))stack
                                   subs <- List.map(fun y -> ((fst y), (substitute exp2 c (snd y)))) subs
                                   subs <- (c, exp2)::subs
                                 else failwith "Failed occurs check"
              | (_, TypVar c) -> if not(occurCheck c exp1) then
                                   stack <- List.map(fun x -> ((substitute exp1 c (fst x)), (substitute exp1 c (snd x)))) stack
                                   subs <- List.map(fun y -> ((fst y), (substitute exp1 c (snd y)))) subs
                                   subs <- (c, exp1)::subs
                                 else failwith "Failed occurs check"
              | (Arrow(a1, b1), Arrow(a2, b2)) -> stack <- (b1, b2)::(a1, a2)::stack
              | (Lst(a), Lst(b)) -> stack <- (a, b)::stack
              | (TypInt, _) -> failwith "Not unifiable"
              | (_, TypInt) -> failwith "Not unifiable"
              | _ -> failwith "Clash in principal type constructor"
  subs

(* Use the following signals if unification is not possible:

 failwith "Clash in principal type constructor"
 failwith "Failed occurs check"
 failwith "Not unifiable"

*)
