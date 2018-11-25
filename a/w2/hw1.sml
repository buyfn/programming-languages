(* 1 *)
fun is_older (a : int * int * int, b : int * int * int) : bool =
    #1 a < #1 b orelse
    (#1 a = #1 b andalso #2 a < #2 b) orelse
    (#1 a = #1 b andalso #2 a = #2 b andalso #3 a < #3 b)

(* 2 *)
fun number_in_month (ds : (int * int * int) list, month : int) =
    if null ds
    then 0
    else
      if #2 (hd ds) = month
      then 1 + number_in_month (tl ds, month)
      else number_in_month (tl ds, month)

(* 3 *)
fun number_in_months (ds : (int * int * int) list, ms : int list) =
    if null ms
    then 0
    else number_in_month (ds, hd ms) + number_in_months (ds, tl ms)

(* 4 *)
fun dates_in_month (ds : (int * int * int) list, month : int) =
    if null ds
    then []
    else
      if #2 (hd ds) = month
      then (hd ds) :: dates_in_month (tl ds, month)
      else dates_in_month (tl ds, month)

(* 5 *)
fun dates_in_months (ds : (int * int * int) list, ms : int list) =
    if null ms
    then []
    else (dates_in_month (ds, hd ms)) @ (dates_in_months (ds, tl ms))

(* 6 *)
fun get_nth (ss : string list, n : int) : string =
    if n <= 1
    then hd ss
    else get_nth (tl ss, n - 1)

(* 7 *)
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

(* 8 *)
fun number_before_reaching_sum (sum : int, xs : int list) : int =
    if (hd xs) >= sum
    then 0
    else 1 + number_before_reaching_sum (sum - (hd xs), tl xs)

(* 9 *)
fun what_month (day : int) : int =
    let
      val monthDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
      number_before_reaching_sum (day, monthDays) + 1
    end

(* 10 *)
fun month_range (d1 : int, d2 : int) : int list =
    if d1 > d2
    then []
    else [what_month d1] @ month_range (d1 + 1, d2)

(* 11 *)
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

(* 12 *)
fun contains (xs : int list, x : int) =
    if null xs
    then false
    else ((hd xs) = x) orelse (contains (tl xs, x))

fun uniq (xs : int list) : int list =
    let
      fun iter (xss : int list, acc : int list) =
	  if null xss
	  then acc
	  else
	    if contains (acc, hd xss)
	    then iter (tl xss, acc)
	    else iter (tl xss, acc @ [hd xss])
    in
      iter (xs, [])
    end

fun number_in_months_challenge (ds : (int * int * int) list, ms : int list) =
    number_in_months (ds, uniq ms)

fun dates_in_months_challenge (ds : (int * int * int) list, ms : int list) =
    dates_in_months (ds, uniq ms)

(* 13 *)
fun reasonable_date (date : int * int * int) : bool =
    let
      val year = #1 date
      val month = #2 date
      val day = #3 date

      val reasonable_year = year > 0
      val reasonable_month = month >= 1 andalso month <= 12
      val reasonable_day =
	  let
	    fun get_nth (xs : int list, n : int) =
		if n <= 1
		then hd xs
		else get_nth (tl xs, n - 1)
	    val is_leap_year = year mod 400 = 0 orelse
			       (year div 4 = 0 andalso year mod 100 > 0)
	    val monthDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	    val days_in_month = if month = 2 andalso is_leap_year
				then 29
				else get_nth (monthDays, month)
	  in
	    day >= 0 andalso day <= days_in_month
	  end
    in
      reasonable_year andalso reasonable_month andalso reasonable_day
    end
