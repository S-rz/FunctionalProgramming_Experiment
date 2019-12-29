(* mult’ : int list * int -> int 			*)
(* REQUIRES: true				*)
(* ENSURES: mult’(L, a) … (* FILL IN *) 	*)

fun mult ([ ], a) = a
	  | mult (x::L, a) = mult(L, x * a);

fun Mult ( [ ], a) = a
    | Mult (r::R, a) = mult(r,a)*Mult(R,1);
