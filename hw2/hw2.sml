(* mohammed 319053823 obedat@campus.technion.ac.il     mohammed 206890998 kh.mohammad@campus.technion.ac.il *)
fun valAt (x::xs) n = if n =0 then x else valAt xs (n-1);

fun sumAtIndices [] [] = 0
 |sumAtIndices list1 [] = 0
 |sumAtIndices list1 (x::xs) = (valAt list1 x) + sumAtIndices list1 xs;

 

fun toLower s = let
  fun getSmal  x = if ord x < 91 andalso ord x>64 then chr(ord x + 32) else x
  val new = map getSmal (explode s)
  fun backStr [] = ""
|backStr (x::xs) = (str x) ^ backStr xs
in
  backStr new
end;

fun countOccurrs (s,c) = let
  fun get [] n k =k
  | get (x::xs) n k = if ord x = n then get xs n k+1
        else get xs n k
in
  get (explode (toLower s)) (ord c) 0
end;



fun getAllOccurrs s = let

fun getOcc s 122 = [(chr 122,countOccurrs (s ,#"z") )]
    |getOcc s n =  [(chr n,countOccurrs (s,(chr n)))] @ (getOcc s (n+1))

val new = (toLower s)
in
  getOcc new 97
end;



fun areAnagrams (s1,s2) = let
  val ss1 = getAllOccurrs s1
  val ss2 = getAllOccurrs s2
in
  ss1=ss2
end;

