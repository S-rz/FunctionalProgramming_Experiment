fun findOdd [] = NONE
    | findOdd (x::L) =
      if((x mod 2) = 1) then SOME x
      else findOdd(L);
