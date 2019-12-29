fun interleave (L:int list,[]) = L
    | interleave ([],L:int list) = L
    | interleave (m::L,n::J) = m::n::interleave(L,J);
