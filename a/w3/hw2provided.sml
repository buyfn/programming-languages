(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1a *)
fun all_except_option (s, ss) =
    case ss of
	[] => NONE
      | cur::ss' => if same_string (s, cur)
		    then SOME ss'
		    else case all_except_option (s, ss') of
			     NONE => NONE
			   | SOME lst => SOME (cur::lst)

(* 1b *)
fun get_substitutions1 (substitutions, str) =
    case substitutions of
	[] => []
      | cur::rest =>
	case all_except_option (str, cur) of
	    NONE => get_substitutions1 (rest, str)
	  | SOME res => res @ get_substitutions1(rest, str)

(* 1c *)
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

(* 1d *)
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

(* 2a *)
fun card_color (Diamonds, _ ) = Red
  | card_color (Hearts, _) = Red
  | card_color (Clubs, _) = Black
  | card_color (Spades, _) = Black

(* 2b *)
fun card_value (_, Num n) = n
  | card_value (_, Ace) = 11
  | card_value _ = 10

(* 2c *)
fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | cur :: rest => if cur = c
		     then rest
		     else cur :: remove_card(rest, c, e)

(* 2d *)
fun all_same_color [] = true
  | all_same_color (head::[]) = true
  | all_same_color (head::neck::tail) =
    card_color head = card_color neck andalso all_same_color (neck::tail)

(* 2e *)
fun sum_cards cs =
    let
      fun aux (sum, lst) =
	  case lst of
	      [] => sum
	    | cur::rest => aux(sum + card_value cur, rest)
    in
      aux (0, cs)
    end

(* 2f *)
fun score (cs, goal) =
    let
      val sum = sum_cards cs
      val prelim_score = if sum > goal
			 then (sum - goal) * 3
			 else goal - sum
    in
      if all_same_color cs
      then prelim_score div 2
      else prelim_score
    end

(* 2g *)
fun officiate (cs, ms, goal) =
    let
      fun aux (hand, moves, deck) =
	  let
	    val sum = sum_cards hand
	  in
	    if sum > goal
	    then score (hand, goal)
	    else
	      case (moves, deck) of
		  ([], _) => score (hand, goal)
		| (Draw::_, []) => score (hand, goal)
		| ((Discard card)::moves', _) =>
		  let
		    val new_hand = remove_card (hand, card, IllegalMove)
		  in
		    aux (new_hand, moves', deck)
		  end
		| (Draw::moves', top_card::deck') =>
		  let
		    val new_hand = top_card::hand
		  in
		    aux (new_hand, moves', deck')
		  end
	  end
    in
      aux ([], ms, cs)
    end
