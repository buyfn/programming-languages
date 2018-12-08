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
      fun aux (SOME acc) rest =
	  case rest of
	      [] => SOME acc
	    | head::tail =>
	      case (f head) of
		  NONE => NONE
		| SOME y => aux (SOME (acc @ y)) tail
    in
      aux (SOME []) xs
    end
