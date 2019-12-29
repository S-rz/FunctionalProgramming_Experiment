fun square (x:int) :int =x*x;

fun double (x:int) :int =2*x;

fun fact (0:int) :int =1
    | fact x = x*fact(x-1);

fun mapList f =
    let
        fun map [] =[]
          | map (x::L) = f(x)::map(L)
    in map
    end

val flag_double = ( mapList(double)[1,2,3] = [2, 4, 6]);
val flag_square = ( mapList(square)[1,2,3] = [1, 4, 9]);
val flag_fact = ( mapList(fact)[1,2,3] = [1, 2, 6]);
