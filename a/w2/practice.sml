(* 1 *)
fun alternate (xs : int list) : int =
    let
      fun iter (rest : int list, acc : int, add : bool) =
	  if null rest
	  then acc
	  else
	    if add
	    then iter (tl rest, acc + hd rest, false)
	    else iter (tl rest, acc - hd rest, true)
    in
      iter (xs, 0, true)
    end

(* 2 *)
fun min_max (xs : int list) : int * int =
    let
      fun first (rest : int list, best : int, better) : int =
	  if null rest
	  then best
	  else
	    if better (hd rest, best)
	    then first (tl rest, hd rest, better)
	    else first (tl rest, best, better)

      fun smaller (a : int, b : int) : bool = a < b
      fun bigger (a : int, b : int) : bool = a > b

      val min = first (tl xs, hd xs, smaller)
      val max = first (tl xs, hd xs, bigger)
    in
      (min, max)
    end
