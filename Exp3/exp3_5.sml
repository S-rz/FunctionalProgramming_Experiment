fun map f [] = []
  | map f(x::R) = (f x)::(map f R)

fun sublists [] = [[]]
  | sublists (x::R) =
  let
    val S = sublists R
  in
    S @ (map (fn A => x::A) S)
  end

fun foldr F z [] = z
  | foldr F z (x::L) = F(x, foldr F z L)

fun sum L = foldr(op+) 0 L

fun findsub([], a) = NONE
  | findsub(x::L:int list list, a) =
  if(sum(x) = a)
  then SOME(x)
  else findsub(L, a)

fun subsetSumOption(L:int list, a:int) =
  let val sub = sublists(L)
  in findsub(sub, a)
  end

val a = subsetSumOption([], 1)
val b = subsetSumOption([1,2,3], 6)
val c = subsetSumOption([1,2,3,4], 7)
