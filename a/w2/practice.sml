(* 1 *)
fun alternate (xs : int list) =
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
