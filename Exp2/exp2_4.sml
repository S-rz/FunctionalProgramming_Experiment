datatype tree = Empty | Node of tree*int*tree

fun reverse [] = []
  | reverse (x::L)  = reverse(L)@[x];

(*求列表长度函数*)
fun length [] = 0
    | length (a::L) = 1+length(L);

fun split_help ([x]:int list,[]:int list,a:int) = ([],x,[])
    | split_help (x::L,J,0) = (J,x,L)
    | split_help (x::L,J:int list,a:int) = split_help(L,J@[x],a-1)
    | split_help ([],L,a) = ([],0,[]);  (*该情况不会出现*)

fun split [x] = ([],x:int,[])
    | split ([x,y]:int list) = ([x],y,[])
    | split (L:int list) = split_help(L,[],length(L) div 2);

fun listToTree ([]) = Empty
    | listToTree (tree_list) =
        let val (L,root,R) = split(tree_list)
        in
          Node (listToTree(L),root,listToTree(R))
        end

(*中序遍历函数*)
fun trav Empty = [ ]
  | trav (Node(t1, x, t2)) = trav t1 @ (x :: trav t2)

fun revT (T:tree) :tree = listToTree(reverse(trav(T)));

val flag =  (trav (revT (listToTree[1,2,3,4,5])) = reverse[1,2,3,4,5]);
