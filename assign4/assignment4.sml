signature g = sig
	eqtype node
	val compare : node*node -> order
end
functor Create_Graph(gra:g) = 
struct
structure MapT = RedBlackMapFn (struct type ord_key = gra.node
		val compare = gra.compare
		end)

val graph_empty = MapT.empty

fun insert_node (graph,node) = MapT.insert(graph,node,[]:gra.node list)

fun nodes(graph) = MapT.listKeys(graph);

fun check_node_graph (graph,node) = if MapT.find(graph,node) = NONE then false else true

fun insert_edge (graph,n1,n2) = if check_node_graph(graph,n1) then 
				let val SOME(x) = MapT.find(graph,n1) in MapT.insert(graph,n1,[n2]@x)
				end
				else MapT.insert(graph,n1,[n2])

fun successor(graph,node) = let val SOME(x) = MapT.find(graph,node) in x end

fun check_other_sucessors (a :: suc_list,node) = if (node = a) then true
						else check_other_sucessors (suc_list,node)
	| check_other_sucessors ([],node) = false

fun find_predecessor (t::keys,node,pred,graph) = let val SOME(x) = MapT.find(graph,t) in 
						if check_other_sucessors(x,node) = true then pred@[t]
						else find_predecessor (keys,node,pred,graph)
						end
	|find_predecessor ([],node,pred,graph) = pred 		 

fun predecessor (graph,node) = let val a = MapT.listKeys(graph) in find_predecessor (a,node,[],graph) end

end	

structure nodes = 
	struct
		datatype node = Node of string
		val compare = fn (Node(x),Node(y)) => String.compare(x,y)
end		
structure G = Create_Graph (nodes)
				
fun check_node (a::node_list,node) = if (node = a) then true
					else check_node ( node_list, node)
	|check_node ([],node) = false

		
fun get_pred_block (graph,node) = let val a = G.predecessor(graph,node) in
					if(List.length a = 0) then [node]
					else
						if (List.length a > 1) then [node]
						else
							let val b = G.successor(graph, hd a) in 
							if (List.length b = 0) then [node]
							else
								if(List.length b >1 )then [node]
								else
									let val c = hd a in node::get_pred_block(graph,c)
									end
							end
				end

				
fun get_succ_block (graph,node) = let val a = G.successor(graph,node) in
					if(List.length a = 0) then [node]
					else
						if (List.length a > 1) then [node]
						else
							let val b = G.predecessor(graph, hd a) in 
							if (List.length b = 0) then [node]
							else
								if(List.length b >1 )then [node]
								else
									let val c = hd a in node::get_succ_block(graph,c)
									end
							end
				end
		

fun check_singularity (graph, node) = let val (a,b) = (G.predecessor(graph,node),G.successor( graph,node)) in 
					if ((List.length a = 0) orelse (List.length a > 1)) andalso ((List.length b = 0) orelse (List.length b > 1)) then true
					else false
					end
									 
fun get_basic_block (node, graph) = if (check_singularity(graph,node) = false) then
					let val p = get_pred_block(graph,node)
						val s = get_succ_block(graph,node) in p@(tl s)  					end
					else [node]	
(*fun check_repitation (y::ys :nodes.node list list, node:nodes.node list) = if (y = node) then true
																			 else
									 											check_repitation (ys:nodes.node list list, node : nodes.node list)
	| check_repitation ([]:nodes.node list list, node:nodes.node list) = false


fun remove_repitation (x::xs : ) = if check_repitation xs x then xs
								else
									x::remove_repitation xs
	|remove_repitation []	= []		
*)
fun find_all_blocks (graph , []) = []
	| find_all_blocks (graph , node_list) = let  val x = get_basic_block (hd node_list,graph) in
							x::find_all_blocks(graph, tl node_list)
						end
						
val x = G.graph_empty
val x = G.insert_node(x,(nodes.Node "a"));
val x = G.insert_node(x,(nodes.Node "b"));
val x = G.insert_node(x,(nodes.Node "c"));
val x = G.insert_node(x,(nodes.Node "d"));
val x = G.insert_node(x,(nodes.Node "e"));
val x = G.insert_node(x,(nodes.Node "f"));
val x = G.insert_node(x,(nodes.Node "g"));
val x = G.insert_node(x,(nodes.Node "h"));
val x = G.insert_edge(x,(nodes.Node "a"),(nodes.Node "b"));
val x = G.insert_edge(x,(nodes.Node "b"),(nodes.Node "c"));
val x = G.insert_edge(x,(nodes.Node "c"),(nodes.Node "d"));
val x = G.insert_edge(x,(nodes.Node "d"),(nodes.Node "e"));
val x = G.insert_edge(x,(nodes.Node "e"),(nodes.Node "f"));
val x = G.insert_edge(x,(nodes.Node "f"),(nodes.Node "g"));
val x = G.insert_edge(x,(nodes.Node "g"),(nodes.Node "h"));
val x = G.insert_edge(x,(nodes.Node "a"),(nodes.Node "e"));


val a = get_basic_block(nodes.Node "a",x)
val b = get_basic_block(nodes.Node "b",x)
val c = get_basic_block(nodes.Node "c",x)
val d = get_basic_block(nodes.Node "d",x)
val e = get_basic_block(nodes.Node "e",x)
val f = get_basic_block(nodes.Node "f",x)
val g = get_basic_block(nodes.Node "g",x)
val h = get_basic_block(nodes.Node "h",x)


val all = find_all_blocks(x,G.nodes(x))
(*val without_repeat = remove_repitation all*)						

