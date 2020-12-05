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


fun get h (Empty::hashTail:'a HashTable) key = get h hashTail key |
    get h (Deleted::hashTail:'a HashTable) key = get h hashTail key |
    get h (Value(x:int,v)::hashTail:'a HashTable) key = if x=key then (x,v)
                                                        else get h hashTail key;
                                                        
                                                        


fun removeAux index h [] key = []
|
removeAux index h (Empty::hashTail:'a HashTable) key = removeAux (index+1) h hashTail key
|
removeAux index h (Deleted::hashTail:'a HashTable) key = removeAux (index+1) h hashTail key
|
removeAux index h (Value(a,b)::hashTail:'a HashTable) key = if a= key then Deleted::hashTail
                                                            else removeAux (index+1) h hashTail key;
                                                  
