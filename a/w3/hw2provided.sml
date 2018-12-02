(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s, lst) =
    let
      fun contains tail =
	  case tail of
	      [] => false
	    | fst::rest =>
	      same_string(s, fst) orelse contains rest
      fun remove (acc, tail) =
	  case tail of
	      [] => acc
	    | fst::rest => if same_string(s, fst)
			   then remove (acc, rest)
			   else remove (acc @ [fst], rest)
    in
      if contains lst
      then SOME (remove ([], lst))
      else NONE
    end

fun get_substitutions1 (substitutions, str) =
    case substitutions of
	[] => []
      | cur::rest =>
	case all_except_option (str, cur) of
	    NONE => get_substitutions1 (rest, str)
	  | SOME res => res @ get_substitutions1(rest, str)

fun get_substitutions2 (substitutions, str) =
    let
      fun aux (acc, lst) =
	  case lst of
	      [] => acc
	    | cur::rest =>
	      case all_except_option (str, cur) of
		  NONE => aux (acc, rest)
		| SOME res => aux (acc @ res, rest)
    in
      aux ([], substitutions)
    end

fun similar_names (substitutions, fullname) =
    let
      val { first = firstname,
	    middle = middlename,
	    last = lastname
	  } = fullname
      val alternative_names =
	  get_substitutions2(substitutions, firstname)
      fun aux (fullnames, firstnames) =
	  case firstnames of
	      [] => fullnames
	    | cur::rest => aux (fullnames @
				[{ first = cur,
				   middle = middlename,
				   last = lastname }],
				rest)
    in
      aux ([fullname], alternative_names)
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
