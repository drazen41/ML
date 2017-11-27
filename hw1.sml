fun is_older (date1:int*int*int ,date2:int*int*int) =
  if (#1 date1) < (#1 date2)	      
  then true
  else
      if (#2 date1) < (#2 date2)
      then true
      else
	  if (#3 date1) < (#3 date2)
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
	   
			    

	  

  
	   
