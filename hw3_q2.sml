(* mohammed 319053823 obedat@campus.technion.ac.il     mohammed 206890998 kh.mohammad@campus.technion.ac.il *)

fun to_binary num =  let
  fun bin  x = if (x mod 2) = 1 then 1 else 0
  fun bin_new x = if (x = 0) then nil else  (bin x)::(bin_new (x div 2))
in
    bin_new num
end;


    fun encode  list = let

    fun len [] = 0
    | len (_::xs) = 1 + len(xs)

    fun num_one [] = 0
    | num_one(x::xs) = x + num_one(xs)

    fun change [] (one:int) (zero:int) last = to_binary (last)
    |change (1::xs) one zero  last = if one > zero orelse one < zero
    then 0 :: ((((change xs) (one -1))(zero +1)) (last+1))
        else 1:: ((((change xs) one) zero) last)
    |change (0::xs) one zero  last = if one > zero orelse one < zero
    then 1 :: ((((change xs) (one +1))(zero -1)) (last+1))
        else 0:: ((((change xs) one) zero) last)
    |change (x::xs) one zero  last = []


    in
        ((((change list) (num_one(list))) (len(list) - num_one(list))) 0)
    end;



fun decode (list , num) = let

    fun get_num_list ((x::xs),0) = (x::xs)
    |get_num_list ([],n) = []
    | get_num_list ((x::xs),n) = get_num_list (xs,n-1)

    fun pow2 0 = 1
    |pow2 p = 2 * pow2(p-1)

    fun get_num [] i = 0
    |get_num (x::xs) i = x*pow2(i) + (get_num(xs) (i+1))

    fun change (x::xs) 0 = (x::xs)
    |change [] i = []
    |change (x::xs) i = ((x+1)mod 2) :: ((change xs) (i-1))

    fun delete_last (x::xs) 0 = []
    |delete_last [] n = []
    |delete_last (x::xs) n = x::delete_last xs (n-1)
in
   delete_last (change list ((get_num (get_num_list (list,num)) 0))) num
end;
