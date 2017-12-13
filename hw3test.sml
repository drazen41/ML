use "hw3provided.sml";
(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test7A = first_answer (fn x => if String.size(x) > 6 then SOME x else NONE) ["the","walrus","and","the","carpenter",	 "talked","of","many","things","","of","shoes","and","ships","and","ceiling","wax","","of","Cabbages","and","Kings"]
    = "carpenter"
val test7B = first_answer (fn x => if String.size(x) > 6 then SOME x else NONE) ["Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware"] = "Alabama"

val test7C = first_answer(fn x => if String.size(x)>1 then
				      let
					  val v = String.size(x)
				      in
					  SOME v
				      end else NONE) ["a","b","max","c"]=3
    
val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1
val test9a1 = count_wildcards TupleP[Wildcard,Wildcard]=2
							    
val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true
val test10a = check_pat (TupleP[ConstructorP("egg",ConstP 4),ConstructorP("egg",ConstP 4)]) = true
val test10b = check_pat (TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]) = true
val test10c = check_pat(TupleP[Variable "x",ConstructorP ("wild",Wildcard)]) = true
val test10d = check_pat (TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard]) = true
val test10e = check_pat (ConstructorP ("egg",ConstructorP ("egg",ConstP 4))) = true
val test10f = check_pat (TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))]) = true
												   
val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

