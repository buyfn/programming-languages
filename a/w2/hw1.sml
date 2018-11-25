(* fun count (xs : 'a list) = *)
(*     if null xs *)
(*     then 0 *)
(*     else 1 + count (tl xs) *)

fun is_older (a : int * int * int, b : int * int * int) : bool =
    #1 a < #1 b orelse
    (#1 a = #1 b andalso #2 a < #2 b) orelse
    (#1 a = #1 b andalso #2 a = #2 b andalso #3 a < #3 b)

fun number_in_month (ds : (int * int * int) list, month : int) =
    if null ds
    then 0
    else
      if #2 (hd ds) = month
      then 1 + number_in_month (tl ds, month)
      else number_in_month (tl ds, month)

fun number_in_months (ds : (int * int * int) list, ms : int list) =
    if null ms
    then 0
    else number_in_month (ds, hd ms) + number_in_months (ds, tl ms)

fun dates_in_month (ds : (int * int * int) list, month : int) =
    if null ds
    then []
    else
      if #2 (hd ds) = month
      then [hd ds] @ dates_in_month (tl ds, month)
      else dates_in_month (tl ds, month)

fun dates_in_months (ds : (int * int * int) list, ms : int list) =
    if null ms
    then []
    else (dates_in_month (ds, hd ms)) @ (dates_in_months (ds, tl ms))

fun get_nth (ss : string list, n : int) : string =
    if n <= 1
    then hd ss
    else get_nth (tl ss, n - 1)

fun date_to_string (date : (int * int * int)) : string =
    let
      val months = [
	"January",
	"February",
	"March",
	"April",
	"May",
	"June",
	"July",
	"August",
	"Sepetember",
	"October",
	"November",
	"December"
      ]
    in
      get_nth (months, #2 date) ^ " " ^
      Int.toString (#3 date) ^ ", " ^
      Int.toString (#1 date)
    end

fun number_before_reaching_sum (sum : int, xs : int list) : int =
    if (hd xs) >= sum
    then 0
    else 1 + number_before_reaching_sum (sum - (hd xs), tl xs)

fun what_month (day : int) : int =
    let
      val monthDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
      number_before_reaching_sum (day, monthDays) + 1
    end

fun month_range (d1 : int, d2 : int) : int list =
    if d1 > d2
    then []
    else [what_month d1] @ month_range (d1 + 1, d2)

fun oldest (ds : (int * int * int) list) =
    if null ds
    then NONE
    else let
      fun oldest_nonempty (ds : (int * int * int) list) =
	  if null (tl ds)
	  then hd ds
	  else let
	    val oldest_tl = oldest_nonempty (tl ds)
	  in
	    if is_older (hd ds, oldest_tl)
	    then hd ds
	    else oldest_tl
	  end
    in
      SOME (oldest_nonempty ds)
    end
