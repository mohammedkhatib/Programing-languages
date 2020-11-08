


fun and1(x:bool,y:bool) = if x then y else false;

fun isInRange(c:char) = if and1(ord(c) >47 ,ord(c) < 58) then true else false;

fun isNumber(s:string) = if size(s) = 0 then true else if isInRange(String.sub(s,0)) then isNumber(substring(s,1,size(s)-1)) else false;



fun atoi(s:string) = if size(s)= 0 then 0 else ord(String.sub(s,size(s)-1)) - 48 + 10 * atoi(substring(s,0,size(s)-1)) ;



fun tokenize(x:string)= String.tokens (Char.isSpace) (String.translate (str) (x));



