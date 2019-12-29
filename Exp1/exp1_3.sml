(* mult : int list -> int 		*)
(* REQUIRES: true		*)
(* ENSURES: mult(L) evaluates to the product of the integers in L. *)
fun mult [ ] = 1
    | mult (x::L) = x*(mult L);
(* mult : int list list -> int 	*)
(* REQUIRES: true		*)
(* ENSURES: mult(R) evaluates to the product of all the integers in the lists of R. *)
fun Mult [ ] = 1
    | Mult (r :: R) = 	mult(r)*Mult(R);
