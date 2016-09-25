(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Joshua Liu, Id Number: 260612384 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code must compile and must not go into infinite
loops.  *)

(* Question 1 *) (* Do not edit this line. *)

(* val sumlist : l:float list -> float *)
let rec sumlist (l: float list): float =
    match l with
    | [] -> 0.0
    | x::xs -> x + sumlist xs

(* val squarelist : l:float list -> float list *)
let rec squarelist (l: float list): float list =
    match l with
    | [] -> []
    | x::xs -> x * x :: squarelist xs

(* val mean : l:float list -> float *)
let mean (l: float list): float =
    let rec length (l: float list): float =
        match l with
        | [] -> 0.0
        | x::xs -> 1.0 + (length xs)
    (sumlist l) / (length l)

(* val mean_diffs : l:float list -> float list *)
let mean_diffs l =
    let m = mean l
    let rec diff (l: float list): float list =
       match l with
       | [] -> []
       | x::xs -> x - m :: diff xs
    diff l

(* val variance : l:float list -> float *)
let variance l =
    let rec length (l: float list): float =
        match l with
        | [] -> 0.0
        | x::xs -> 1.0 + (length xs)
    sumlist (squarelist (mean_diffs l)) / (length l)

(* End of question 1 *) (* Do not edit this line. *)

(* Question 2 *) (* Do not edit this line. *)

(* val memberof : 'a * 'a list -> bool when 'a : equality *)
let rec memberof (item, l): bool =
    match l with
    | [] -> false
    | x::xs -> if (x = item) then true
               else memberof (item, xs)

(* val remove : 'a * 'a list -> 'a list when 'a : equality *)
let rec remove (item, l) =
    match l with
    | [] -> []
    | x::xs -> if (x = item) then remove (item, xs)
               else x :: remove (item, xs)

(* End of question 2 *) (* Do not edit this line *)

(* Question 3 *) (* Do not edit this line *)

(* val isolate : l:'a list -> 'a list when 'a : equality *)
let rec isolate l =
    match l with
    | [] -> []
    | x::xs -> x :: (isolate (remove (x, xs)))

(* End of question 3 *) (* Do not edit this line *)

(* Question 4 *) (* Do not edit this line *)

(* val common : 'a list * 'a list -> 'a list when 'a : equality *)
let rec common (l1, l2): 'a list =
    match l1 with
    | [] -> []
    | x::xs -> if ((memberof (x, l2)) = true) then x :: common (xs, l2)
               else common (xs, l2)

(* End of question 4 *) (* Do not edit this line *)

(* Question 5 *) (* Do not edit this line *)

(* val split : l:'a list -> 'a list * 'a list *)
let rec split l =
    match l with
    | [] -> ([], [])
    | [x] -> ([x], [])
    | x::y::xs -> let (Left, Right) = split xs
                  (x::Left, y::Right)

(* val merge : 'a list * 'a list -> 'a list when 'a : comparison *)
let rec merge (l1, l2) =
    match (l1, l2) with
    | ([], ys) -> ys
    | (xs, []) -> xs
    | (x::xs, y::ys) -> if x < y then x :: merge (xs, y::ys)
                        else y :: merge (x::xs, ys)

(* val mergesort : l:'a list -> 'a list when 'a : comparison *)
let rec mergesort l =
    match l with
    | [] -> []
    | [a] -> [a]
    | list -> let(Left, Right) = split list
              merge (mergesort Left, mergesort Right)

(* End of question 5 *) (* Do not edit this line *)
