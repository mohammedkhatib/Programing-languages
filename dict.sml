(* mohammed 319053823 obedat@campus.technion.ac.il     mohammed 206890998 kh.mohammad@campus.technion.ac.il *)

datatype ('a, 'b) dictionary = 
    Nil
  | Dict of {key: 'a, value: 'b} list;

exception ItemNotPresent;

(*
fun insert Nil k v = (Dict([{key= k , value= v}]))
|   insert (Dict((d::ds))) k v = if((#key d) = k) then (Dict({key=k,value=v}::ds))
        else let
         val Dict(x) =  (insert (Dict(ds)) k v)
         in
                Dict(d::x)
         end
| insert (Dict(nil)) k v = (Dict([{key=k , value= v}]));
*)

fun insert Nil k v = (Dict([{key= k , value= v}]))
| insert (Dict(nil)) k v = (Dict([{key=k , value= v}]))
| insert (Dict((d::nil))) k v = (Dict(d::[{key=k , value= v}]))
| insert x k v =  let
        fun change Nil k v = nil
        |change (Dict(nil)) k v = [{key=k , value= v}]
        |change (Dict(d::ds)) k v = if((#key d) = k) then ({key=k,value=v}::ds)
         else (d::(((change((Dict(ds)))k)v)))
in
        (Dict(change x k v))
end;


fun find Nil k = raise ItemNotPresent
|find (Dict(nil)) k = raise ItemNotPresent
|find (Dict(d::ds)) k = if ((#key d) = k) then (#value d) else (find (Dict(ds)) k);


(*
fun remove dic key = let
        fun remove_h Nil k n = raise ItemNotPresent
        |remove_h (Dict(nil)) k n = raise ItemNotPresent
        |remove_h (Dict(d::_)) k 1 = if ((#key d)= k) then Nil else raise ItemNotPresent
        |remove_h (Dict(d::ds)) k n = if ((#key d)= k) then (Dict(ds))
                else let
                        val (Dict(t)) = (remove_h (Dict(ds)) k n)
                        in Dict(d::t) end
        fun length Nil = 0
        |length (Dict(nil)) = 0
        |length (Dict(d::ds)) = 1 + (length (Dict(ds)))
in
  (((remove_h dic) key) (length dic))
end;
*)

fun remove dic key = let
        fun remove_h Nil k = raise ItemNotPresent
        |remove_h (Dict(nil)) k = raise ItemNotPresent
        |remove_h (Dict(d::ds)) k = if((#key d) = k) then ds
        else (d::((remove_h((Dict(ds)))k)))
in
        (Dict((remove_h dic) key))
end;

fun keys Nil = nil
|keys (Dict(nil)) = nil
|keys (Dict(d::ds)) = (#key d ) :: (keys(Dict(ds)));

fun values  Nil = nil
|values  (Dict(nil)) = nil
|values  (Dict(d::ds)) = (#value d ) :: (values (Dict(ds)));
