datatype Symbol = Terminal of string | Nonterminal of string;
datatype Rhs = Symbols of Symbol list
datatype Rule = Production of (Symbol*Rhs)
datatype Grammer = Gr of (Rule list)

 val s = Nonterminal ("s");
 val a = Terminal ("a");
 val b = Terminal ("b");
 val rule_3 = Production (s,Symbols[]);
 val rule_1 = Production (s,Symbols[a,s]);
 val rule_2 = Production (s,Symbols[b]);
 val g = Gr[rule_1,rule_2,rule_3];
 
 fun Compare (x , y) = case (x , y)  of
 			(Terminal(a) , Terminal(b)) => if (a = b) then EQUAL else GREATER
 			|(Nonterminal(a) , Nonterminal(b)) => if (a = b) then EQUAL else GREATER
 			
 structure MapT = RedBlackMapFn(struct
 			type ord_key = Symbol
 			val compare = Compare
 			end);
 			
 val map_var = MapT.empty;
 
 fun Push (map_var, Nonterminal(symbol)) = if ( MapT.find(map_var,symbol) = None) then MapT.insert(map_var,symbol,true)
 										else map_var
 										end;
 										
 	|Push(map_var , Terminal (symbol)) = map_var
 	
 fun Check_list(map_var,symbol_list) = if symbol_lost = [] then map_var
 									   else
 											let var temp = (push,hd symbol_list) in
 											 Check_list( temp, lt symbol_list) 
 										end;
 										
 fun Check_rhs (map_var,symbol_list,symbol) = if symbol_list = [] then Push(map_var, symbol)
 												else Check_list(map_var, symbol_list)
 												end;
 												
 fun Check_nullability (map_var,Production(trem,symbol_list)) =	if(symbol_list = []) then push(map_var,term)
 																else Check_rhs(map_var ,symbol_list, term);
  																end;
 fun Check_rules (g,map_var) = if g=[] then map_var
 								else  
 									let val map_obj_new = Check_nullability(map_var,hd g); in
									check_rules(map_obj_new,tl g);
								end;
 
 val new_map_var = MapT.empty;
 val nullable = check_rules(g, map_var); 
 
