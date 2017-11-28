fun is_older (date1:int*int*int ,date2:int*int*int) =
  if (#1 date1) < (#1 date2)	      
  then true
  else
      if (#1 date1)=(#1 date2) andalso (#2 date1) < (#2 date2)
      then true
      else
	  if (#1 date1)=(#1 date2) andalso (#2 date1) = (#2 date2) andalso (#3 date1) < (#3 date2)
	  then true
	  else false
fun number_in_month (dateList : (int*int*int) list, mjesec : int) =
  if null dateList
  then 0
  else
      let
	  fun count(date : int * int * int) =
	    if (#2 date) = mjesec		
	    then 1
	    else 0
      in
	  count(hd dateList) + number_in_month(tl dateList,mjesec)
      end
fun number_in_months(dateList : (int*int*int) list, monthList:int list)=
  if null monthList
  then 0
  else
      number_in_month(dateList,hd monthList) + number_in_months(dateList,tl monthList)
fun dates_in_month(dateList : (int*int*int) list, mjesec:int)=
  if null dateList
  then []
  else
      if #2(hd dateList)=mjesec
      then hd(dateList)::dates_in_month(tl(dateList),mjesec)
      else dates_in_month(tl(dateList),mjesec)
fun dates_in_months(dateList:(int*int*int)list,monthList:int list)=
  if null monthList
  then []
  else
      dates_in_month(dateList,hd monthList)@dates_in_months(dateList,tl monthList)
fun get_nth(strings:string list,n:int)=
  if n<2
  then hd strings
  else
      if n=1
      then hd strings
      else get_nth(tl strings,n-1)
fun date_to_string(datum:int*int*int)=
  let
      val months = ["January","February","March","April","May","June","July","August","September","October","November",
 	            "December"]
  in
     get_nth(months,#2 datum) ^ " "  ^ Int.toString(#3 datum) ^ ", " ^ Int.toString(#1 datum)
  end
fun number_before_reaching_sum(sum:int,numbers:int list)=
  if (hd numbers)>=sum
  then 0
  else 1+number_before_reaching_sum(sum-(hd numbers),tl numbers)
fun what_month(day:int)=
  let
      val days = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      number_before_reaching_sum(day,days)+1
  end
fun month_range(day1:int, day2:int)=
  (* month_range (31, 34) = [1,2,2,2] *)
  if day1>day2
  then []
  else
      what_month(day1)::month_range(day1+1,day2)
fun oldest(dateList:(int*int*int)list)=
  (* oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31) *)
  if null dateList
  then NONE
  else
      let
	  fun older_date(dateList:(int*int*int)list,date:(int*int*int))=
	    if null dateList
	    then date
	    else
		if is_older(hd(dateList),date)
		then older_date(tl dateList,hd(dateList))
		else older_date(tl dateList,date)
      in 
	  SOME(older_date(tl(dateList),hd(dateList)))
      end
  
	   
