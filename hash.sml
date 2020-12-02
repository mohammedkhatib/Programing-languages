datatype 'a HashTableEntry = Empty
    | Deleted
    | Value of int * 'a;

type 'a HashTable = 'a HashTableEntry list;

fun create 0 = [] 
            | create n= Empty :: create (n-1);
create 8;


datatype mass= kg of int;


val r = kg(5);
val kg t = r;


(*fun h n x = (x + 1) mod n;
val h = h 8;*)

val r = Value(10,5.5);
val Value(a,b)=r;


fun insertAux index h [] Value(x,v)= []
|
insertAux index h (htable:HashTable) Value(x,v) = if h x=index then (if (hd htable=Empty) orelse (hd htable=Deleted) then [Value(x,v)]@(tl htable) else Value(x,v)::insertAux (index+1) h (tl htable) ********* else (hd htable)::insertAux (index+1) h (tl htable) (x,v);
end;


