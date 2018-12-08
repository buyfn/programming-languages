(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
val only_capitals =
    List.filter (fn s => Char.isUpper (String.sub (s, 0)))

(* 2 *)
val longest_string1 =
    foldl (fn (current, longest) =>
	      if String.size (current) > String.size (longest)
	      then current
	      else longest) ""

(* 3 *)
val longest_string2 =
    foldl (fn (current, longest) =>
	      if String.size (current) >= String.size (longest)
	      then current
	      else longest) ""

(* 4 *)
fun longest_string_helper f ss =
    foldl (fn (current, longest) =>
	      if f (String.size (current), String.size(longest))
	      then current
	      else longest) "" ss

val longest_string3 = longest_string_helper (fn (a, b) => a > b)

val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = String.implode o rev o String.explode

(* 7 *)
fun first_answer f [] = raise NoAnswer
  | first_answer f (x::xs') = case f x of
				  SOME y => y
				| NONE => first_answer f xs'

(* 8 *)
fun all_answers f xs =
    let
      fun aux (SOME acc) [] = SOME acc
	| aux (SOME acc) (head :: tail) =
	  (case (f head) of
	      NONE => NONE
	    | SOME y => aux (SOME (acc @ y)) tail)
	| aux NONE _ = NONE
    in
      aux (SOME []) xs
    end

(* 9a *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(* 9b *)
val count_wild_and_variable_lengths =
    g (fn _ => 1)
      (fn idf => String.size idf)

(* 9c *)
fun count_some_var (var, p) =
    g (fn _ => 0)
      (fn idf => if idf = var then 1 else 0)
      p

(* 10 *)
fun check_pat pat =
    let
      fun count_idfs p =
	  case p of
	      Variable x => [x]
	    | ConstructorP (st, pt) => count_idfs pt
	    | TupleP ps => List.foldl (fn (curP, acc) =>
					  acc @ (count_idfs curP))
				      [] ps
	    | _ => []

      fun has_repeats xs =
	  List.exists (fn x => length (List.filter (fn y => y = x) xs) > 1) xs
    in
      not (has_repeats (count_idfs pat))
    end

(* 11 *)
fun match (v, p) =
    case (v, p) of
	(value, Variable x) => SOME [(x, value)]
      | (_, Wildcard) => SOME []
      | (Unit, UnitP) => SOME []
      | (Const n, ConstP m) => if n = m then SOME [] else NONE
      | (Tuple vs, TupleP ps) =>
	if length vs = length ps
	then all_answers match (ListPair.zip (vs, ps))
	else NONE
      | (Constructor (sv, value), ConstructorP (sp, pat)) =>
	if sv = sp
	then match (value, pat)
	else NONE
      | _ => NONE

(* 12 *)
fun first_match v ps =
    SOME (first_answer (fn p => match (v, p)) ps)
    handle NoAnswer => NONE
