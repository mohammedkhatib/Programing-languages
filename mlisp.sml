(* mohammed 319053823 obedat@campus.technion.ac.il     mohammed 206890998 kh.mohammad@campus.technion.ac.il *)
exception MlispError;
exception Undefined;
exception Empty;


(*First section*)
fun isNumber(s:string) = if(String.size(s) = 0) then true else if Char.isDigit(String.sub(s,0)) then isNumber(String.substring(s,1, String.size(s) -1)) else false;

(*Second section*)
fun atoi (s) = if(String.size(s)=0) then 0 else ord(String.sub(s, String.size(s)-1)) - ord(#"0")+ 10*atoi(String.substring(s, 0, String.size(s) -1));

(*Third section*)
fun seprate x = if x = #"(" then " ( " else if x = #")" then " ) " else str(x);
val tokenize = fn s => String.tokens Char.isSpace(String.translate(seprate)(s));


datatype Atom =
   SYMBOL of string | NUMBER of int | NIL;

datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp);
			
fun parse nil = ATOM(NIL) |
parse parse_input = 
let
    fun find_offset [] counter = 0	|
	find_offset lst 0 = 0	|
    find_offset lst counter =
		let
			val c = List.hd(lst);
			val next_lst = List.tl(lst);
        
		in if (c=")") then (1 + (find_offset next_lst (counter-1))) else if (c="(") then (1 + (find_offset next_lst (counter+1))) else (1 + (find_offset next_lst (counter)))
		end;
	
    fun rec_parse (lst,i) = if (i >= List.length(lst)) then ATOM(NIL) else
        let
			val tmp = List.drop (lst,i);
			val curr = List.hd(tmp);
			val new_lst = List.tl(tmp);
        in if (curr = "(") then
			CONS(rec_parse(List.take(new_lst,((find_offset new_lst 1)-1)),0), rec_parse(lst, i+1+(find_offset new_lst 1)))

        else if (isNumber(curr)) then
            CONS(ATOM(NUMBER(atoi(curr))), rec_parse(lst, i+1)) 

        else 
			CONS(ATOM(SYMBOL(curr)), rec_parse(lst, i+1))
        end;
    val tmp = if(List.hd(parse_input) = "(" andalso (List.hd(List.drop(parse_input, (List.length(parse_input)-1))))=")") then List.take(List.tl(parse_input), List.length(List.tl(parse_input))-1) else parse_input;
in rec_parse(tmp,0)
end;



(*Section 1*)
fun initEnv () = fn (str:string) => raise Undefined; 

(*Section 2*)
fun define str f a = (fn (x:string) => if(x = str) then a else (f x));

(*Section 3*)
fun emptyNestedEnv () = [initEnv ()];
fun pushEnv newenv lst = newenv::lst;
fun popEnv [] = raise Empty | popEnv lst = List.tl lst;
fun topEnv [] = raise Empty | topEnv lst = List.hd lst;

(*Section 4*)
fun defineNested _ [] _ = raise Empty | defineNested str lst vlue = (define str (List.hd lst) vlue)::List.tl lst;

(*Section 5*)
fun find str [] = raise Undefined | find (str:string) EnvLst = (topEnv EnvLst str) handle Undefined =>(find str (popEnv EnvLst));


