fun longest_string_helper f =
    List.foldl (fn (str, acc) =>
		   if f (String.size str, String.size acc)
		   then str
		   else acc) ""
val longest_string3 = longest_string_helper (fn (a, b) => a > b)
val longest_string4 = longest_string_helper (fn (a, b) => a >= b)
	       
fun longest_string_helper f strings =
  foldl (fn (x,y) => if f (String.size(x), String.size(y))
		     then x
		     else y) "" strings		

val longest_string3 = longest_string_helper (op >)
val longest_string4 = longest_string_helper (op >=)
