(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (x,ys)=
  case ys of
      [] => NONE 
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
fun card_value card =
  case card of
      (_, Ace) => 11 
   | (_, Num i) => i 
   | (_,_) => 10  
fun remove_card (cards,card,e)=
  let
      fun f (cards,acc,inList)=
	case cards of
	    []  => if inList
		  then acc
		  else raise e
	  | c:: cards' => if c=card andalso inList
			  then f(cards',c::acc,true)
			  else
			      if c=card
			      then f(cards',acc,true)
			      else f(cards',c::acc,inList)
	 
					  
  in
      f (cards,[],false)
  end
fun all_same_color cards =
  case cards of
      [] => true 
    | c::[] => true 
    | head::(neck::rest) => if card_color(head)=card_color(neck) then all_same_color(neck::rest) else false 
fun sum_cards cards =
  let
      fun f (cards,acc)=
	case cards of
	    []=>acc 
	     | c::cards' => f(cards',card_value(c)+acc) 
		    
  in
      f(cards,0)
  end
fun score (cards,goal)=
  let
      val sameColor = all_same_color(cards)
      val sumCards = sum_cards(cards)
      val preliminary = if sumCards > goal then 3*(sumCards-goal) else goal-sumCards 
      
			      
  in
      if sameColor
      then preliminary div 2
      else preliminary
  end
fun officiate (cards,moves,goal)=
  let
      (*val held_cards = [] *)
      fun f (moves,cards,sum,held_cards)=
	case moves of
	    [] => goal
	  | m::moves' =>
	    case m of
		Discard d => f(moves',cards,sum,remove_card(held_cards,d,IllegalMove))
	      | Draw => case cards of
			[] => score(helds,goal)
			 | c::cards' =>
			   let val helds = c::held_cards
			   in
			       	if (sum_cards(helds)>goal)
				then score(helds,goal)
				else f(moves',cards',sum+score(helds,goal),helds)
			   end	
  in
     f(moves,cards,0,[])
  end
      (*
fun score_challange (cards,goal)=
  let
      fun aces (cs,sum)=
	case cs of
	    []=>sum 
	  | c::cards' => case c of
			     (_,Ace)=>aces(cards',sum+1) 
			   | (_,_) => aces(cards',sum)
      val old_score = score(cards,goal)
      val naces = aces(cards,0) 		      
      fun new_score i =
	let
	   		      
	    val sumCards = sum_cards(cards)-i*10
	    val sameColor = all_same_color(cards)
					  let
					      
					  in
					  end
	    val preliminary =
		if sumCards > goal 
		then 3*(sumCards-goal)
		else goal-sumCards		   				 
	in
	    if sameColor then preliminary div 2 else preliminary
	end
      
			   
  in
      if old_score > goal andalso naces > 0
      then new_score 1
      else old_score
  end
   
*)
