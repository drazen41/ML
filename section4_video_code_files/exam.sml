fun foo f x y z = 
	if x >= y
	then (f z)
	else foo f y x (tl z)
fun baz f a b c d e = (f (a ^ b))::(c + d)::e

fun f(xs:(int*int) list)  =
  case xs of
      []=>0
       |(a,b)::(c,d)::(e,f)::[] => 1 
				| x::y =>2 
					 
					
						
signature DIGIT = 
sig
type digit = int
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val down_and_up : digit -> digit
val test : digit -> unit
end


						
structure Digit :> DIGIT =
struct
type digit = int
exception BadDigit
exception FailTest
fun make_digit i = if i < 0 orelse i > 9 then raise BadDigit else i
fun increment d = if d=9 then 0 else d+1
fun decrement d = if d=0 then 9 else d-1
val down_and_up = increment o decrement (* recall o is composition *)
fun test d = if down_and_up d = d then () else raise FailTest
end

(*   
signature COUNTER =
sig
    type t = int
    val newCounter : int -> t
    val increment : t -> t 
    val first_larger : t * t -> bool 
end
*)
signature COUNTER =
sig
    type t
    val newCounter : int -> t
    val increment : t -> t
    val first_larger : t * t -> bool
end
structure NoNegativeCounter :> COUNTER = 
struct

exception InvariantViolated

type t = int

fun newCounter i = if i <= 0 then 1 else i

fun increment i = i + 1

fun first_larger (i1,i2) =
    if i1 <= 0 orelse i2 <= 0
    then raise InvariantViolated
    else (i1 - i2) > 0

end					
					
					
				       
