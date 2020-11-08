(* mohammed obedat 319053823 obedat@campus.technion.ac.il     mohammed khateeb 206890998 kh.mohammad@campus.technion.ac.il *)
    fun sig1 a b f = f(a,f(a,b));
    fun sig2 (a,b) f = if(f(Math.ln(b))^"a" = "a") then true else sig2 (a+1,b) f;
    fun sig3 f a b c = f a b;
    fun sig4 a b c d = c+d;
    fun sig5 f a g = g(f a , f a);
    fun sig6 () () = ();
    fun sig7 x (y,z) = if 1=2 then x else if 3=4 then z else y;
    fun sig8 (a ,(b,c))=(a+1,b^"mo",c^"mo");



    fun curry f x y =f(x,y);
    fun uncurry f(x,y) = f x y;
