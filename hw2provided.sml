(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (x,ys)=
  case ys of
      [] => SOME ys 
    | y::ys' =>
      let
	  val ok = false
	  fun lista (input, output,inList)=	   
	    case input of
		[] => if ys<>output andalso inList 
		      then SOME(output)
		      else NONE	     
	       |i::input'  =>
	
		    if same_string(x,i)
		    then lista(input',output,true)
		    else lista(input',i::output,inList) 
	       
		            
      in
	lista(ys,[],false)
      end 


fun  get_substitutions1(yss,x)=
  case yss of
      [] => [] 
    | y::yss' =>
     let
		   val a = all_except_option(x,y)
     in
		   case a of 
			NONE => get_substitutions1(yss',x)
		      | SOME i =>i@get_substitutions1(yss',x)
     end

fun  get_substitutions2(yss,x)=
  case yss of
      [] => [] 
    | y::yss' =>
      let
	  fun f (yss,acc)=
	    case yss of
		[] => acc 
	     | y::yss' => 
	       let
		   val a = all_except_option(x,y)
	       in
		   case a of 
			NONE => f(yss',acc) 
			    | SOME i => f(yss',i@acc) 
			     
	       end
      in
	  f(yss,[])
      end
fun similar_names(yss,r)=
  let val {first=a,last=b,middle=c}=r
      fun f(subs,acc) =
	case subs of
	    []=>r::acc 
	   |s::subs' => let val r1= {first=s, middle=c,last=b}
			in
			    f(subs',r1::acc)
			end
  in
     f(get_substitutions2(yss,a),[])  			   
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
fun card_color card =
  case card of
      (Hearts,_) => Red
   | (Clubs,_) => Black 
   | (Diamonds,_) => Red 
   | (Spades,_) => Black   
