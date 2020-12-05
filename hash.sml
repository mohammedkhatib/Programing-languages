datatype 'a HashTableEntry = Empty
    | Deleted
    | Value of int * 'a;

type 'a HashTable = 'a HashTableEntry list;

fun create 0 = [] 
            | create n= Empty :: create (n-1);




fun h n x = (x + 1) mod n;
val h = h 8;





fun insertAux index h [] (x:int,v) hashIndex = []
|
insertAux index h (Empty::hashTail:'a HashTable) (x:int,v) hashIndex = if (h hashIndex) = index  then [Value(x,v)]@hashTail 
                                                                        else Empty:: insertAux (index+1) h hashTail (x,v) (hashIndex)
|
insertAux index h (Deleted::hashTail:'a HashTable) (x:int,v) hashIndex = if (h hashIndex) = index then [Value(x,v)]@hashTail
                                                                        else Deleted:: insertAux (index+1) h hashTail (x,v) hashIndex
|
insertAux index h (Value(a,b)::hashTail:'a HashTable) (x:int,v) hashIndex = if a = x then [Value(x,v)]@hashTail
                                                                        else if h hashIndex = index then Value(a,b)::insertAux (index+1) h hashTail (x,v) index
                                                                                    else Value(a,b)::insertAux (index+1) h hashTail (x,v) hashIndex;

                                                   

fun insert h htable (x,v)= insertAux 0 h htable (x,v) x;


