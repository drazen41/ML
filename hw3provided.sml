(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals strings = List.filter(fn x=> Char.isUpper(String.sub(x,0))) strings

fun longest_string1 strings =
  foldl(fn (x,y) =>
	   let
	       val t1 = String.size(x)
	       val t2 = String.size(y)	 			   
	   in
	       if t1>t2
	       then x
	       else y
	   end) "" strings
fun longest_string2 strings =
  List.foldl(fn (x,y) =>
	   let
	       val t1 = String.size(x)
	       val t2 = String.size(y)		   
	   in
	       if t2>t1 then y else x
	   end ) "" strings
fun longest_string_helper g xs =
  List.foldl(fn(x,y)=>
	   if g(String.size(x),String.size(y))
	   then x
	   else y)"" xs
val longest_string3 =
    longest_string_helper(fn(x,y)=>
			     if x>y
			     then true
			     else false) 
val longest_string4 =
   longest_string_helper(fn(x,y)=>
			     if y>x
			     then false
			     else true) 
	  
val longest_capitalized = longest_string1 o only_capitals

fun rev_string x = (String.implode o List.rev o String.explode) x
			  

fun first_answer g xs =
  let
      fun f xs =
	case xs of
	    [] => raise NoAnswer 
	  | x::xs' => case g x of
			  NONE => f xs' 
			       | SOME i=>i 
  in
      f xs
  end
fun all_answers g xs =
  let
      fun f (xs,acc,t)=
	case xs of
	    []=>if t then SOME [] else SOME acc 
	  | x::xs' => case g x of
			  NONE=>NONE 
			| SOME i => f(xs',i@acc,false)
			 
  in
      f(xs,[],true)
  end
   
fun count_wildcards p = g (fn x => 1) (fn x => 0) p
fun count_wild_and_variable_lengths p = g (fn x => 1)(fn y => String.size(y)) p
fun count_some_var (s,p) = g (fn x => 0) (fn y => if y=s then 1 else 0) p
fun check_pat p =
  let
      fun extract t =
	case t of
	    Variable x => [x] 
		       | TupleP ps => List.foldl(fn (x,y) => y@extract(x)) [] ps 
				   | ConstructorP(_,p) => extract(p)  
				   | _ =>[]
      fun check list =
	case list of
	    [] => true 
	       | x::xs' => if List.exists(fn y => y=x) xs' then false else check(xs') 
  in
     check(extract(p))
  end

fun match (v,p) =
  case (v,p) of
      (_,Wildcard) => SOME [] 
    | (Unit,UnitP) => SOME [] 
    | (Const cv,ConstP pv) => if cv = pv then SOME[] else NONE
    | (Tuple vs,TupleP ps) => if List.length(vs) = List.length(ps)
			      then all_answers (fn (x,y)=>match(x,y))(ListPair.zip(vs,ps))
			      else NONE
    | (Variable s,_) => SOME[(s,v)]
    | (Constructor(s1,cv),ConstructorP(s2,cpv)) => if cv=cpv then match(cv,cpv) else NONE 
    | _ => NONE 
				   
fun first_match v ps =
  NONE
		
			     
