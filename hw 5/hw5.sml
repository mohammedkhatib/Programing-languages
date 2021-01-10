(* mohammed obedat 319053823 obedat@campus.technion.ac.il     mohammed khateeb 206890998 kh.mohammad@campus.technion.ac.il *)

datatype 'a seq = Nil | Cons of 'a * (unit-> 'a seq);


fun arithmeticSeq a1 d = (Cons(a1,fn()=>(arithmeticSeq(a1+d) d)));





fun getSubSeq a1 d s e =
 let
    fun getS Nil s = Nil
    |getS sq1 1 = sq1
    |getS (Cons(x,xs)) s = (getS (xs()) (s-1))

    fun get Nil e= nil
    |get (Cons(x,_)) 0 = nil
    |get (Cons(x,xs)) e = x::(get(xs()) (e-1))
in
  (get (getS(arithmeticSeq a1 d) s) (e-s +1))
end;







fun getKDivElems a1 d n k =
 let
    fun get Nil k n= nil
    |get sq1 k 0 = nil
    |get (Cons(x,xs)) k n = if((x mod k)=0) then x::(get(xs()) k (n-1)) else (get(xs()) k (n))

    fun check t s = if( t < 0 orelse s <0 orelse s = 0) then true else false
in 
if((check n k)) then []
else
  (get (arithmeticSeq a1 d) k n)
end;










datatype 'a lazyTree = tNil | tCons of 'a * (unit -> 'a lazyTree) * (unit -> 'a lazyTree);




fun lazyTreeFrom x = (tCons(x , fn()=> (lazyTreeFrom (2*x)) , fn()=> (lazyTreeFrom(2*x+1))));




fun lazyTreeMap (f, tNil) = tNil
|lazyTreeMap (f,(tCons(x,xl,xr))) = (tCons(f(x) ,fn()=>(lazyTreeMap(f,xl())),fn()=>(lazyTreeMap(f,xr()))));





fun lazyTreeFilter (f,tNil) = tNil
|lazyTreeFilter (f,(tCons(x,xl,xr))) = if (f(x) = true) then (tCons(x ,fn()=>(lazyTreeFilter(f, xl())),fn()=>(lazyTreeFilter(f,xr()))))
else tNil;

