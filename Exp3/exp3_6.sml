fun exists p [] = false
  | exists p (x::L) = p(x) orelse exists p L;

fun forall p [] = true
  | forall p (x::L) = p(x) andalso forall p L;

fun jishu x= (x mod 2 = 1);
fun oushu x= (x mod 2 = 0);

val a = exists jishu [1,2,4,6]; (*true*)
val b = exists jishu [2,4,6];   (*false*)
val c = forall oushu [1,2,4,6]; (*false*)
val d = forall oushu [2,4,6];   (*true*)
