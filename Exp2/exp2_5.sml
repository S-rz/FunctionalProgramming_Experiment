datatype tree = Empty | Node of tree*int*tree

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

(*中序遍历函数  tree->list*)
fun trav Empty = [ ]
  | trav (Node(t1, x, t2)) = trav t1 @ (x :: trav t2);

fun judge ([],x) = true
    | judge (x::L,y) =
      case Int.compare(x,y) of
           LESS => false
         | EQUAL => true andalso judge(L,x)
         | GREATER => true andalso judge(L,x);

fun order [] = true
    | order(x::L) = judge(L,x);

fun find ([],x) =false
    | find (x::L,y) = (x=y) orelse find(L,y);

fun binarySearch(tree,x) =
    let val tree_list=trav tree
    in
      ( order(tree_list) andalso find(tree_list,x) )
    end


(*test*)
val test1 = listToTree([1,2,3,4,5])
val flag1_1 = binarySearch(test1,2)  (*true*)
val flag1_2 = binarySearch(test1,6)  (*false*)

val test2 = listToTree([5,4,8,9])
val flag2_1 = binarySearch(test2,4)  (*false*)
val flag2_2 = binarySearch(test2,7)  (*false*)
