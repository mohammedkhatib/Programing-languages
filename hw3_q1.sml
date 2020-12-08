(* mohammed 319053823 obedat@campus.technion.ac.il     mohammed 206890998 kh.mohammad@campus.technion.ac.il *)

datatype ('a, 'b) heterolist = heteronil | ::: of 'a * ('b,'a) heterolist ;
infixr 5 ::: ;
 

fun build4 (x, one, y, two) = x:::one:::y:::two:::heteronil; 


fun unzip listH = let
  fun gettwo heteronil f s = (f,s)
  |gettwo (x:::heteronil) f s = (x::f, s)
  | gettwo (x:::y:::xs) f s = gettwo xs (x::f) (y::s)

   fun swap []  f= f
    |swap (x::xs) f= swap (xs) (x::f)

    fun swap2  (f,s)  = (swap f [], swap s [])
    
in
   swap2 (gettwo listH [] [])
end;


fun zip ([],[]) = heteronil
    | zip (nil , (y::ys)) =  raise Empty
    | zip ((x::xs),nil) =  raise Empty
    | zip ((x::xs),(y::ys)) =  x ::: y :::zip (xs ,ys);
