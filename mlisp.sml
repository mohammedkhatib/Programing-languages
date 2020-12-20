datatype Atom =
   SYMBOL of string | NUMBER of int | NIL;
datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp);

fun seprate x = if x = #"(" then " ( " else if x = #")" then " ) " else str(x);
val tokenize = fn s => String.tokens Char.isSpace(String.translate(seprate)(s));
fun and1(x:bool,y:bool) = if x then y else false;
fun isInRange(c:char) = if and1(ord(c) >47 ,ord(c) < 58) then true else false;

fun isNumber(s:string) = if size(s) = 0 then true else if isInRange(String.sub(s,0)) then isNumber(substring(s,1,size(s)-1)) else false;



fun atoi(s:string) = if size(s)= 0 then 0 else ord(String.sub(s,size(s)-1)) - 48 + 10 * atoi(substring(s,0,size(s)-1)) ;




fun drop list 0 = list
  | drop [] n = []
  | drop  (x::xs) n = drop xs (n - 1);


fun find_offset [] counter = 0	|
	find_offset lst 0 = 0	|
    find_offset lst counter =
		let
			val next_lst = tl(lst);  
		in if (hd(lst)=")") then (1 + (find_offset next_lst (counter-1))) else if (hd lst="(") then (1 + (find_offset next_lst (counter+1))) else (1 + (find_offset next_lst (counter)))
		end;



fun rec_parse (lst,i) = if (i >= List.length(lst)) then ATOM(NIL) else
        let
			val curr = hd(drop lst i);
			val new_lst = tl(drop lst i);
        in if (curr = "(") then
			CONS(rec_parse(List.take(new_lst,((find_offset new_lst 1)-1)),0), rec_parse(lst, i+1+(find_offset new_lst 1)))

        else if (isNumber(curr)) then
            CONS(ATOM(NUMBER(atoi(curr))), rec_parse(lst, i+1)) 

        else 
			CONS(ATOM(SYMBOL(curr)), rec_parse(lst, i+1))
        end;

fun aux parse_input =if(List.hd(parse_input) = "(" andalso (List.hd(drop parse_input (List.length(parse_input)-1)))=")") then List.take(tl(parse_input), List.length(tl(parse_input))-1) else parse_input;


fun parse nil = ATOM(NIL) |
parse parse_input = 
rec_parse(aux parse_input,0);

    
	
  
