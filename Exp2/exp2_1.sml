fun reverse [] = []
    | reverse (x::L)  = reverse(L)@[x];

fun reverse_n ([] , L:int list) = L
    | reverse_n (x::L , J:int list)  = reverse_n(L,x::J);

fun myreverse(L:int list) = reverse_n(L,[]);
