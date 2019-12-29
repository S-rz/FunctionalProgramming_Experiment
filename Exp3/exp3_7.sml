datatype 'a tree = Empty | Node of 'a tree*'a*'a tree

(*求列表长度函数*)
fun length [] = 0
    | length (a::L) = 1+length(L);

fun split_help(a::R, L, 0, c) = (L, a, R)
  | split_help(a::R, L, b, c) = split_help(R, L@[a], b-1, c)
  | split_help([], L, b, c) = ([], c, []) (*不可能出现此情况*)

fun split ([x],d) = ([],x,[])
    | split ([x,y],d) = ([x],y,[])
    | split (L,d) = split_help(L,[],length(L) div 2,d);

fun listToTree ([]) = Empty
    | listToTree (x::tree_list) =
        let val (L,root,R) = split(x::tree_list,x)
        in
          Node (listToTree(L),root,listToTree(R))
        end

(*中序遍历函数*)
fun trav Empty = [ ]
  | trav (Node(t1, x, t2)) = trav t1 @ (x :: trav t2)

fun change_help p [] = []
    | change_help p (x::L) =
      if(p(x))
      then (SOME x)::(change_help p L)
      else NONE :: (change_help p L)

fun treeFilter p =
    let fun change T=
      let val tree_list = trav T
          val tree_change = change_help p tree_list
      in
        listToTree tree_change
      end
    in
      change
    end

fun jishu x = ((x mod 2)=1);
val a = treeFilter jishu (listToTree[1, 2, 3])
val b = trav a
