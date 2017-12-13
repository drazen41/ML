fun all_except_option (s,xs) =
  case xs of
      [] => NONE
    | x::xs' => if same_string(s,x)
                then SOME xs'
                else case all_except_option(s,xs') of
                         NONE => NONE
                       | SOME y => SOME(x::y)
fun get_substitutions1 (substitutions,str) =
    case substitutions of
	      [] => []
      | x::xs => case all_except_option(str,x) of
		                 NONE => get_substitutions1(xs,str)
		               | SOME y => y @ get_substitutions1(xs,str)
fun get_substitutions1 (substitutions,str) = 
    case substitutions of 
        [] => [] 
      | x::xs => let val foo = all_except_option(str,x) 
                 in case foo of 
                        NONE => get_substitutions1(xs,str) 
                      | SOME y => y @ get_substitutions1(xs,str) 
                 end
fun get_substitutions2 (substitutions,str) =
    let fun loop (acc,substs_left) =
        case substs_left of
            [] => acc
          | x::xs => loop ((case all_except_option(str,x) of
                                NONE => acc
                              | SOME y => acc @ y),
                           xs)
    in
        loop ([],substitutions)
    end		     
fun similar_names (substitutions,name) =
    let 
        val {first=f, middle=m, last=l} = name
	      fun make_names xs =
	         case xs of
		           [] => []
	           | x::xs' => {first=x, middle=m, last=l}::(make_names(xs'))
    in
	      name::make_names(get_substitutions2(substitutions,f))
    end
fun card_color card =
    case card of
        (Clubs,_)    => Black
      | (Diamonds,_) => Red
      | (Hearts,_)   => Red
      | (Spades,_)   => Black

fun card_value card =
    case card of
	      (_,Jack) => 10
      | (_,Queen) => 10
      | (_,King) => 10
      | (_,Ace) => 11 
      | (_,Num n) => n
			 
fun remove_card (cs,c,e) =
    case cs of
	      [] => raise e
      | x::cs' => if x = c then cs' else x :: remove_card(cs',c,e)

fun remove_card (cs,c,e) =
    let	fun f cs =
	          case cs of
		            [] => raise e
	           | x::cs' => if x = c then cs' else x :: f cs'
    in
        f cs
    end
fun all_same_color cs = 
    case cs of
        [] => true
      | [_] => true
      | head::neck::tail => card_color head = card_color neck 
			    andalso all_same_color(neck::tail)

fun all_same_color cs = 
    case cs of
        head::neck::tail => card_color head = card_color neck 
			    andalso all_same_color(neck::tail)
      | _ => true	
fun sum_cards cs =
    let fun loop (acc,cs) =
	    case cs of
		      [] => acc
	      | c::cs' => loop (acc + card_value c, cs')
    in
	    loop (0,cs)
    end
fun score (cs,goal) = 
    let 
        val sum = sum_cards cs
    in
        (if sum >= goal then 3 * (sum - goal) else goal - sum)
	      div (if all_same_color cs then 2 else 1)
    end
fun officiate (cards,plays,goal) =
    let 
        fun loop (current_cards,cards_left,plays_left) =
            case plays_left of
                [] => score(current_cards,goal)
              | (Discard c)::tail => 
                loop (remove_card(current_cards,c,IllegalMove),cards_left,tail)
              | Draw::tail =>
                (* note: must score immediately if go over goal! *)
                case cards_left of
                    [] => score(current_cards,goal)
                  | c::rest => if sum_cards (c::current_cards) > goal
                               then score(c::current_cards,goal)
                               else loop (c::current_cards,rest,tail)
    in 
        loop ([],cards,plays)
    end
fun officiate1 (cardlist, moves, goal) =
  let
      fun helper(cs, hs, ms) =
	case (ms,cs) of 
	    ([],_) => score(hs, goal) 
	  | ((Discard c)::ms',_) => helper(cs, remove_card(hs, c, IllegalMove), ms')
          | ((Draw)::ms',[]) => score(hs, goal)
	  | ((Draw)::ms',c::cs') => let val temp = c::hs 
				    in if sum_cards(temp) > goal
       			               then score(temp, goal)
				       else helper(cs', temp, ms')
				    end
  in
      helper(cardlist, [], moves)
  end	
fun count_aces(cs, count) =
  case cs of
      [] => count
    | (_, Ace)::cs' => count_aces(cs', 1+count)
    | _::cs' => count_aces(cs', count)
      
fun score_challenge(cards, goal) =
  let
      val diff = goal - sum_cards(cards)
      fun pscore_ace(d, ace_count) =
	if ace_count = 0
	then if d < 0 then ~3*d else d
	else if d > 0 then d
	else Int.min(~3*d, pscore_ace(d+10, ace_count-1))
		       
      val pscore = pscore_ace(diff, count_aces(cards, 0))
  in
      if all_same_color(cards) then pscore div 2 else pscore
  end

fun officiate_challenge(cardlist, moves, goal) =
  let
      fun helper(cs, hs, ms) =
	case (ms,cs) of 
	    ([],_) => score_challenge(hs, goal) 
	  | ((Discard c)::ms',_) => helper(cs, remove_card(hs, c, IllegalMove), ms')
          | ((Draw)::ms',[]) => score_challenge(hs, goal)
	  | ((Draw)::ms',c::cs') => let val temp = c::hs 
				    in if sum_cards(temp) -
					  (10*count_aces(temp, 0))< goal
				       then helper(cs', temp, ms')
				       else score_challenge(temp, goal)
				    end
  in
      helper(cardlist, [], moves)
  end
