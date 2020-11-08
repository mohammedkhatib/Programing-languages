
	(*mohammed khateeb 206890998 kh.mohammed@campus.technion.ac.il mohamad obedat 319053823 obedat@campus.technion.ac.il *)


fun and1(x:bool,y:bool) = if x then y else false;

fun isInRange(c:char) = if and1(ord(c) > 47 ,ord(c) < 58) then true else false;

fun isNumber(s:string) = if size(s) = 0 then true else if isInRange(String.sub(s,0)) then isNumber(substring(s,1,size(s)-1)) else false;






fun atoi(s:string) = if size(s)= 0 then 0 else ord(String.sub(s,size(s)-1)) - 48 + 10 * atoi(substring(s,0,size(s)-1)) ;





fun getNextWS(s:string,count:int) =
if size(s) = 0 then count
else if  str(String.sub(s,0)) = " " then count 
else getNextWS(substring(s,1,size(s)-1),count + 1);

fun tokenizeAux(s:string,l:(string list)) = if 
size(s)=0 then l 
else if str(String.sub(s,0)) 
= " " then tokenizeAux(substring(s,1,size(s)-1),l)
else tokenizeAux(substring(s,getNextWS(s,0) ,size(s)-getNextWS(s,0)),l@[substring(s,0,getNextWS(s,0))]) ;


fun tokenize(s:string) = tokenizeAux(s,[]);


fun tokenize2 (x:string)= String.tokens (Char.isSpace) (String.translate (str) (x));









