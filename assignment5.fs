(* Assignment 5 *) (* Do not edit this line. *)
(* Student name: Josh Liu, Id Number: 260612384 *) (* Edit this line. *)

(* This is the type definition for the expressions that we will produce as a result
of parsing the input strings. *)

type exptree = Var of char | Expr of char * exptree * exptree

(* We only allow lower-case one-character variable names*)
let charSet = ['a' .. 'z']

(* Here is an example input string.  Blank spaces are not allowed. *)
let example = "(a+(b+(c*d)+e)*f)*g"

(* This just tests if the character is one of the allowed variable names.*)
let isin (x: char) L = List.exists (fun y -> x = y) L

(* This is the top-level function.  It reads an input string and outputs the parse tree.
It is not recursive at top level but the main guts of it consists of three
mutually-recursive functions called expr, term and primary.  There is also a function
called getsym which returns the next character from the input string.  getsym is imperative
and uses the mutable local variables sym and cursor.  Please do not change the definition of
getsym or the declarations of sym and cursor.  No doubt all this can be done more slickly,
but I am trying to be as simple-minded as possible. *)

let parse (inputexp: string): exptree =
  let sym = ref inputexp.[0]
  let cursor = ref 0

  let getsym () =
    cursor := !cursor + 1
    sym := inputexp.[!cursor]

  let rec expr (): exptree =
    let b = primary()
    if (!cursor = inputexp.Length - 1 || !sym = ')') then b
    else
      let a = !sym
      getsym()
      if a = '+' then Expr (a, b, expr())
      else
        let c = primary()
        if (!cursor = inputexp.Length - 1 || !sym = ')') then Expr (a, b, c)
        else
          let d = !sym
          getsym()
          Expr (d, Expr(a, b, c), expr())
  and primary (): exptree =  //I did this for you.
    if !sym = '(' then
      getsym()
      let result = expr ()
      if not (!sym = ')') then
        failwith "Mismatched parens"
      else
        if (!cursor = inputexp.Length - 1)
        then
          result
        else
          getsym()
          result
    elif (isin !sym charSet) then
      if (!cursor = inputexp.Length - 1)
      then
        (Var !sym)
      else
        let result = Var !sym in (getsym(); result)
    else
      printfn "sym is : %c." !sym
      failwith "In primary"
  expr() //This is part of the top-level function parse.

(* Now for Question 2 *)

(*  Do not change this.  tempstore will be a global variable.  Remember to reset this between
tests of the program. *)

let mutable tempstore = 0

let codegen (e: exptree) =

  let rec helper (e: exptree, tag: char) =
    match e with
    | Expr (a, b, c) ->
      if tag = '=' then
        helper (b, '=')
        helper (c, a)
      else
        tempstore <- tempstore + 1
        printfn "STORE %i" tempstore
        helper (b, '=')
        helper (c, a)
        if tag = '+' then
          printfn "ADD %i" tempstore
        elif tag = '*' then
          printfn "MUL %i" tempstore
        tempstore <- tempstore - 1
    | Var a ->
      if tag = '=' then printfn "LOAD  %c" a
      elif tag = '+' then printfn "ADD  %c" a
      else printfn "MUL  %c" a
  helper(e,'=') //This is part of the-level function codegen.  Do not change it.
