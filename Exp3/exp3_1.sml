fun square (x:int) :int =x*x;

fun double (x:int) :int =2*x;

fun fact (0:int) :int =1
    | fact x = x*fact(x-1);

fun thenAddOne (f,x:int) =f(x)+1;
