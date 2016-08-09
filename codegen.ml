(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/
*)
open Llvm
open Hashtbl

open Ast
open Sast
open Semant

module L = Llvm

let context = L.global_context ()
let the_module = L.create_module context "Liva"
let builder = L.builder context

let i1_t = L.i1_type context
let i8_t = L.i8_type context;;
let i32_t = L.i32_type context;;
let i64_t = L.i64_type context;;
let f_t = L.double_type context;;

let str_t = L.pointer_type i8_t;;
let void_t = L.void_type context;;

let arr_type = Arraytype(Char_t, 1)

let local_var_table:(string, llvalue) Hashtbl.t = Hashtbl.create 100
let formals_table:(string, llvalue) Hashtbl.t = Hashtbl.create 100
let struct_typ_table:(string, lltype) Hashtbl.t = Hashtbl.create 100
let struct_field_idx_table:(string, int) Hashtbl.t = Hashtbl.create 100

(*~~~~~~~~~~~~~~~ global functions: code generator utils ~~~~~~~~~~~~~~~~~ *)
let rec get_llvm_type datatype = match datatype with 
	  Datatype(Int_t) -> i32_t
	| Datatype(Float_t) -> f_t
	| Datatype(Bool_t) -> i1_t
	| Datatype(Char_t) -> i8_t
	| Datatype(Void_t) -> void_t
	| Datatype(Null_t) -> i32_t
	| Datatype(Objecttype(name)) -> L.pointer_type(find_llvm_struct_type name)
	| Arraytype(t, i) -> get_arr_llvm_type (Arraytype(t, (i)))
	| _ -> raise(Failure ("Invalid DataType")) 

and find_llvm_struct_type name = 
	try Hashtbl.find struct_typ_table name
	with | Not_found -> raise(Failure ("undeclared struct"^ name))

and get_arr_llvm_type datatype = match datatype with
	  Arraytype(t, 0) -> get_llvm_type (Datatype(t))
	| Arraytype(t, 1) -> L.pointer_type (get_llvm_type (Datatype(t)))
	| Arraytype(t, i) -> L.pointer_type (get_arr_llvm_type (Arraytype(t, (i-1))))
	| _ -> raise(Failure ("Invalid Array Pointer Type"))

let string_of_fname = function 
	  Constructor -> "constructor"
	| FName(s)	-> s

let find_func_in_module fname = 
	match (L.lookup_function fname the_module) with
	  None -> raise (Failure("Function NotFound in module: " ^ fname))
	| Some f -> f

(*~~~~~~~~~~~~~~~~~~~~~~~~~ code generator top level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
let translate sast = 

	let classes = sast.classes in
	let functions = sast.functions in
    let main =sast.main in 
	
	let util_func () = 
		let printf_typ = L.var_arg_function_type i32_t [| pointer_type i8_t |] in
		let malloc_typ = L.function_type (str_t) [| i32_t |] in
		let lookup_typ = L.function_type (pointer_type i64_t) [| i32_t; i32_t |] in
		
		let _ = L.declare_function "printf" printf_typ the_module in
		let _ = L.declare_function "malloc" malloc_typ the_module in
		let _ = L.define_function "lookup" lookup_typ the_module in
		()	
	in
	let _ = util_func () in

	(*~~~~~~~~~~~~~~~~~~~~~~~~~ Generate class struct ~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
	let add_struct_typ_table cls =
		let struct_typ = L.named_struct_type context cls.scname in
		Hashtbl.add struct_typ_table cls.scname struct_typ
	in

	let _ = List.map add_struct_typ_table classes in

	let class_struct_gen s = 
		let struct_t = Hashtbl.find struct_typ_table s.scname in
		let type_list = List.map (function Field(d, _) -> get_llvm_type d) s.sfields in
		let name_list = List.map (function Field(_, s) -> s) s.sfields in
		let type_list = i32_t :: type_list in
		let name_list = ".key" :: name_list in
		let type_array = (Array.of_list type_list) in
		List.iteri (
			fun i f ->
	        let n = s.scname ^ "." ^ f in
	        Hashtbl.add struct_field_idx_table n i;
	    	) 
	    name_list;
		L.struct_set_body struct_t type_array true
	in
	let _ = List.map class_struct_gen classes in

	(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ define functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
	let func_define sfdecl=

		let fname = sfdecl.sfname in
		let is_var_arg =ref false in 
		let parameters = List.rev ( List.fold_left 
		(fun l -> (function 
			Formal (t,_)-> get_llvm_type t::l 
		      | _ -> is_var_arg = ref true; l))[]  sfdecl.sformals)	
		in 

		let fty = 
			if !is_var_arg 
				then L.var_arg_function_type (get_llvm_type sfdecl.sreturnType )
				(Array.of_list parameters)
			else L.function_type (get_llvm_type sfdecl.sreturnType)
				(Array.of_list parameters)			
		in 
		L.define_function (string_of_fname fname) fty the_module
	in
	let _ = List.map func_define functions in
	
	(*~~~~~~~~~~~~~~~~~~~~~ function generation utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
	(*statement gengeration*)
	let rec stmt_gen llbuilder = function 
		  SBlock sl        ->	List.hd (List.map (stmt_gen llbuilder) sl)
		| SLocal (d, s, e) ->	
			let local_gen datatype var_name expr llbuilder = 
				let t = match datatype with 
					Datatype(Objecttype(name)) -> find_llvm_struct_type name
					|  _ -> get_llvm_type datatype
				in
				let alloca = L.build_alloca t var_name llbuilder in
				Hashtbl.add local_var_table var_name alloca;
				let lhs = SId(var_name, datatype) in
				match expr with 
					SNoexpr -> alloca
				|  	_ -> assign_gen lhs expr datatype llbuilder
			in
			local_gen d s e llbuilder

		| SReturn (e, d)   ->	
			let return_gen d expr llbuilder =  
				match expr with
					  SId(name, d) ->
						(match d with 
						| Datatype(Objecttype(_)) -> build_ret (id_gen false false name d llbuilder) llbuilder
						| _ -> build_ret (id_gen true true name d llbuilder) llbuilder)
					| SObjAccess(e1, e2, d) -> build_ret (obj_access_gen true e1 e2 d llbuilder) llbuilder
					| SNoexpr -> build_ret_void llbuilder
					| _ -> build_ret (expr_gen llbuilder expr) llbuilder

			in
			return_gen d e llbuilder

		| SExpr (se, _)	   ->	expr_gen llbuilder se

		(*control flow*)
		| SIf (e, s1, s2)  ->	
			let if_gen exp then_stmt else_stmt llbuilder =
				let condition= expr_gen llbuilder exp in			
				let start_block = L.insertion_block llbuilder in
				let parent_function = L.block_parent start_block in
				let then_block = L.append_block context "then" parent_function in
				L.position_at_end then_block llbuilder;

				let then_val = stmt_gen llbuilder then_stmt in
				let new_then_block = L.insertion_block llbuilder in
				let else_block = L.append_block context "else" parent_function in
				L.position_at_end else_block llbuilder;

				let else_val= stmt_gen llbuilder else_stmt in
				let new_else_block = L.insertion_block llbuilder in			
				let merge_block = L.append_block context "ifcont" parent_function in
				L.position_at_end merge_block builder;
				
				let incoming = [(then_val, new_then_block); (else_val, new_else_block)] in
				let phi = L.build_phi incoming "iftmp" builder in		
				L.position_at_end start_block llbuilder;
				ignore (build_cond_br condition then_block else_block llbuilder);
				position_at_end new_then_block llbuilder; ignore (build_br merge_block llbuilder);
				position_at_end new_else_block llbuilder; ignore (build_br merge_block llbuilder);
				(* set the builder to the end of the merge block *)
				L.position_at_end merge_block llbuilder;
				phi
			in
			if_gen e s1 s2 llbuilder			

		| SFor (se1, se2, se3, s) -> for_gen se1 se2 se3 s llbuilder

		| SWhile (se, s)   ->	while_gen se s llbuilder

	(*expression generation*)
	and expr_gen llbuilder = function
		  SInt_Lit (i)     ->	L.const_int i32_t i
		| SBoolean_Lit (b) ->	if b then L.const_int i1_t 1 else L.const_int i1_t 0
		| SFloat_Lit (f)   ->	L.const_float f_t  f
		| SChar_Lit (c)    ->	L.const_int i8_t (Char.code c)
		| SString_Lit (s)  ->	L.build_global_stringptr s "tmp" llbuilder

		| SId (id, d)      -> id_gen true false id d llbuilder

		| SBinop (e1, op, e2, d) -> 
			let binop_gen e1 op e2 d llbuilder =
				let type1 = Semant.typOFSexpr e1 in
				let type2 = Semant.typOFSexpr e2 in
				let e1 = expr_gen llbuilder e1 in
				let e2 = expr_gen llbuilder e2 in	
				let float_ops op e1 e2 =
					match op with
					  Add 		-> L.build_fadd e1 e2 "flt_addtmp" llbuilder
					| Sub 		-> L.build_fsub e1 e2 "flt_subtmp" llbuilder
					| Mult 		-> L.build_fmul e1 e2 "flt_multmp" llbuilder
					| Div 		-> L.build_fdiv e1 e2 "flt_divtmp" llbuilder
					| Mod 		-> L.build_frem e1 e2 "flt_sremtmp" llbuilder
					| Equal 	-> L.build_fcmp Fcmp.Oeq e1 e2 "flt_eqtmp" llbuilder
					| Neq 		-> L.build_fcmp Fcmp.One e1 e2 "flt_neqtmp" llbuilder
					| Less 		-> L.build_fcmp Fcmp.Ult e1 e2 "flt_lesstmp" llbuilder
					| Leq 		-> L.build_fcmp Fcmp.Ole e1 e2 "flt_leqtmp" llbuilder
					| Greater	-> L.build_fcmp Fcmp.Ogt e1 e2 "flt_sgttmp" llbuilder
					| Geq 		-> L.build_fcmp Fcmp.Oge e1 e2 "flt_sgetmp" llbuilder
					| _ 		-> raise(Failure("Invalid operator for floats"))
				in
				let int_ops op e1 e2 = 
					match op with
					  Add 		-> L.build_add e1 e2 "addtmp" llbuilder
					| Sub 		-> L.build_sub e1 e2 "subtmp" llbuilder
					| Mult 		-> L.build_mul e1 e2 "multmp" llbuilder
					| Div 		-> L.build_sdiv e1 e2 "divtmp" llbuilder
					| Mod 		-> L.build_srem e1 e2 "sremtmp" llbuilder
					| Equal 	-> L.build_icmp Icmp.Eq e1 e2 "eqtmp" llbuilder
					| Neq 		-> L.build_icmp Icmp.Ne e1 e2 "neqtmp" llbuilder
					| Less 		-> L.build_icmp Icmp.Slt e1 e2 "lesstmp" llbuilder
					| Leq 		-> L.build_icmp Icmp.Sle e1 e2 "leqtmp" llbuilder
					| Greater	-> L.build_icmp Icmp.Sgt e1 e2 "sgttmp" llbuilder
					| Geq 		-> L.build_icmp Icmp.Sge e1 e2 "sgetmp" llbuilder
					| And 		-> L.build_and e1 e2 "andtmp" llbuilder
					| Or 		-> L.build_or  e1 e2 "ortmp" llbuilder
					| _ 		-> raise(Failure("Invalid operator for integers"))
				in	
				
				let binop_type_cast lhs rhs lhsType rhsType llbuilder = 
					match (lhsType, rhsType) with
					  Datatype(Int_t), Datatype(Int_t)	 -> (lhs, rhs), Datatype(Int_t)
					| Datatype(Int_t), Datatype(Char_t)	 -> (build_uitofp lhs i8_t "tmp" llbuilder, rhs), Datatype(Char_t)
					| Datatype(Int_t), Datatype(Float_t) -> (build_sitofp lhs f_t "tmp" llbuilder, rhs), Datatype(Float_t)
					| Datatype(Char_t), Datatype(Int_t)  -> (lhs, build_uitofp rhs i8_t "tmp" llbuilder), Datatype(Char_t)
					| Datatype(Char_t), Datatype(Char_t) -> (lhs, rhs), Datatype(Char_t)
					| Datatype(Bool_t), Datatype(Bool_t) -> (lhs, rhs), Datatype(Bool_t)
					| Datatype(Float_t), Datatype(Int_t) -> (lhs, build_sitofp rhs f_t "tmp" llbuilder), Datatype(Float_t)
					| Datatype(Float_t), Datatype(Float_t) 	-> (lhs, rhs), Datatype(Float_t)

					| Datatype(Objecttype(d)), Datatype(Null_t)	-> (lhs, rhs), lhsType
					| Datatype(Null_t), Datatype(Objecttype(d)) -> (rhs, lhs), rhsType
					| Arraytype(d, s), Datatype(Null_t)	 -> (lhs, rhs), lhsType
					| Datatype(Null_t), Arraytype(d, s)  -> (rhs, lhs), rhsType

					| 	_ -> raise (Failure("binop type not supported"))
				in

				let (e1, e2), d = binop_type_cast e1 e2 type1 type2 llbuilder in
				let type_handler d = match d with
						Datatype(Float_t)   -> float_ops op e1 e2
					|	Datatype(Int_t)	
					|   Datatype(Bool_t)
					| 	Datatype(Char_t) 	-> int_ops op e1 e2
					| 	Datatype(Objecttype(_))
					|   _ -> raise (Failure("Invalid binop type"))
				in
				type_handler d
			in
			binop_gen e1 op e2 d llbuilder

		| SUnop (op, e, d)        -> 
			let unop_gen op e d llbuilder =
				let e_typ = Semant.typOFSexpr e in
				let e = expr_gen llbuilder e in
				let unops op e_typ e = match (op, e_typ) with
					  (Sub, Datatype(Int_t)) 	->  L.build_neg e "int_unoptmp" llbuilder
					| (Sub, Datatype(Float_t)) 	-> 	L.build_fneg e "flt_unoptmp" llbuilder
					| (Not, Datatype(Bool_t)) 	->  L.build_not e "bool_unoptmp" llbuilder
					| _ -> raise (Failure("unop not supported"))	in

				let unop_type_handler d = match d with
					  Datatype(Float_t)   
					| Datatype(Int_t)		
					| Datatype(Bool_t)	-> unops op e_typ e
					| _ -> raise (Failure("invalid unop type  "))
				in	
				unop_type_handler d
			in
			unop_gen op e d llbuilder	

		| SAssign (e1, e2, d) -> assign_gen e1 e2 d llbuilder
	    | SCall (fname, expr_list, d, _) -> 
	    	let reserved_func_gen llbuilder d expr_list = function
				  "print" 	-> print_func_gen expr_list llbuilder
				| "sizeof"	-> sizeof_func_gen expr_list llbuilder
				| "cast" 	-> cast_func_gen expr_list d llbuilder
				| "malloc" 	-> malloc_func_gen "malloc" expr_list d llbuilder
				|  _ as call_name -> raise(Failure("function call not found: "^ call_name))
	    	in
	    	reserved_func_gen llbuilder d expr_list fname

	    | SObjectCreate (id, el, d) -> 
			let create_obj_gen fname el d llbuilder = 
				let f = find_func_in_module fname in
				let params = List.map (expr_gen llbuilder) el in
				let obj = L.build_call f (Array.of_list params) "tmp" llbuilder in
				obj
			in
	    	create_obj_gen id el d llbuilder

		| SObjAccess (e1, e2, d)  -> obj_access_gen true e1 e2 d llbuilder

		| SArrayCreate (t, el, d) -> 
			let arr_create_gen llbuilder t expr_type el = 
				if(List.length el > 1) then raise(Failure("array not supported"))
				else
				match expr_type with 
				 Arraytype(Char_t, 1) -> 
					let e = List.hd el in
					let size = (expr_gen llbuilder e) in
					let t = get_llvm_type t in
					let arr = L.build_array_malloc t size "tmp" llbuilder in
					let arr = L.build_pointercast arr (pointer_type t) "tmp" llbuilder in
					arr
				| _ -> 
					let e = List.hd el in
					let t = get_llvm_type t in
					let size = (expr_gen llbuilder e) in
					let size_t = L.build_intcast (size_of t) i32_t "tmp" llbuilder in
					let size = L.build_mul size_t size "tmp" llbuilder in
					let size_real = L.build_add size (const_int i32_t 1) "arr_size" llbuilder in
					
				    let arr = L.build_array_malloc t size_real "tmp" llbuilder in
					let arr = L.build_pointercast arr (pointer_type t) "tmp" llbuilder in

					let arr_len_ptr = L.build_pointercast arr (pointer_type i32_t) "tmp" llbuilder in

					ignore(build_store size_real arr_len_ptr llbuilder); 
					let init_array arr arr_len init_val start_pos llbuilder =
						let new_block label =
							let f = block_parent (insertion_block llbuilder) in
							append_block (global_context ()) label f
						in
						let bbcurr = insertion_block llbuilder in
						let bbcond = new_block "array.cond" in
						let bbbody = new_block "array.init" in
						let bbdone = new_block "array.done" in
						ignore (L.build_br bbcond llbuilder);
						position_at_end bbcond llbuilder;

						let counter = L.build_phi [const_int i32_t start_pos, bbcurr] "counter" llbuilder in
						add_incoming ((L.build_add counter (const_int i32_t 1) "tmp" llbuilder), bbbody) counter;
						let cmp = L.build_icmp Icmp.Slt counter arr_len "tmp" llbuilder in
						ignore (L.build_cond_br cmp bbbody bbdone llbuilder);
						position_at_end bbbody llbuilder;

						let arr_ptr = L.build_gep arr [| counter |] "tmp" llbuilder in
						ignore (L.build_store init_val arr_ptr llbuilder);
						ignore (L.build_br bbcond llbuilder);
						position_at_end bbdone llbuilder
					in
					init_array arr_len_ptr size_real (const_int i32_t 0) 0 llbuilder;
					arr
			in
			arr_create_gen llbuilder t d el

		| SArrayAccess (e, el, d) -> arr_access_gen false e el d llbuilder

		| SNoexpr -> L.build_add (const_int i32_t 0) (const_int i32_t 0) "nop" llbuilder
		| _  as  e -> raise(Failure("expression not match"))


	and print_func_gen expr_list llbuilder = 
		let printf = find_func_in_module "printf" in
		let tmp_count = ref 0 in
		let incr_tmp = fun x -> incr tmp_count in
		let map_expr_to_printfexpr expr =
			let exprType = Semant.typOFSexpr expr in
			match exprType with 
			Datatype(Bool_t) ->
				incr_tmp ();
				let tmp_var = "tmp" ^ (string_of_int !tmp_count) in
				let trueStr = SString_Lit("true") in
				let falseStr = SString_Lit("false") in
				let id = SId(tmp_var, arr_type) in 
				ignore(stmt_gen llbuilder (SLocal(arr_type, tmp_var, SNoexpr)));
				ignore(
					stmt_gen llbuilder 
					(
						SIf(
							expr, SExpr(SAssign(id, trueStr, arr_type), arr_type), 
							SExpr(SAssign(id, falseStr, arr_type), arr_type)
							)
					)
				);
				expr_gen llbuilder id
			| _ -> expr_gen llbuilder expr
		in
		let params = List.map map_expr_to_printfexpr expr_list in
		let param_types = List.map (Semant.typOFSexpr) expr_list in 
		let map_param_to_string = function 
		  Arraytype(Char_t, 1) 	-> "%s"
		| Datatype(Int_t) 		-> "%d"
		| Datatype(Float_t) 	-> "%f"
		| Datatype(Bool_t) 		-> "%s"
		| Datatype(Char_t) 		-> "%c"
		| _ 					-> raise (Failure("Print invalid type"))
		in
		let const_str = List.fold_left (fun s t -> s ^ map_param_to_string t) "" param_types in
		let s = expr_gen llbuilder (SString_Lit(const_str)) in
		let zero = const_int i32_t 0 in 
		let s = L.build_in_bounds_gep s [| zero |] "tmp" llbuilder in
		L.build_call printf (Array.of_list (s :: params)) "tmp" llbuilder

	and sizeof_func_gen el llbuilder =
		let type_of_sexpr = Semant.typOFSexpr (List.hd el) in
		let type_of_sexpr = get_llvm_type type_of_sexpr in
		let size_of_typ = L.size_of type_of_sexpr in	
		L.build_intcast size_of_typ i32_t "tmp" llbuilder
		
	and cast_func_gen el d llbuilder =
		let cast_malloc_to_objtype lhs currType newType llbuilder = match newType with
			  Datatype(Objecttype(x)) -> 
				let obj_type = get_llvm_type (Datatype(Objecttype(x))) in 
				L.build_pointercast lhs obj_type "tmp" llbuilder
			| _ as t -> raise (Failure("cannot cast"))
		in
		let expr = List.hd el in
		let t = Semant.typOFSexpr expr in
		let lhs = match expr with
			| Sast.SId(id, d) -> id_gen false false id d llbuilder
			| SObjAccess(e1, e2, d) -> obj_access_gen false e1 e2 d llbuilder
			| _ -> expr_gen llbuilder expr
		in
		cast_malloc_to_objtype lhs t d llbuilder

	and malloc_func_gen fname el d llbuilder = 
		let f = find_func_in_module fname in
		let params = List.map (expr_gen llbuilder) el in
		match d with
		  Datatype(Void_t) -> L.build_call f (Array.of_list params) "" llbuilder
		| _ -> L.build_call f (Array.of_list params) "tmp" llbuilder

	and id_gen deref checkParam id d llbuilder = 
		if deref then
			try Hashtbl.find formals_table id
			with | Not_found ->
				try let _val = Hashtbl.find local_var_table id in
				build_load _val id llbuilder
				with | Not_found -> raise (Failure("unknown variable id " ^ id))
		else		
			try Hashtbl.find local_var_table id
			with | Not_found ->
				try 
					let _val = Hashtbl.find formals_table id in
					if checkParam then raise (Failure("cannot assign"))
					else _val
				with | Not_found -> raise (Failure("unknown variable id "^ id))

	and assign_gen lhs rhs d llbuilder = 
		let rhs_t = Semant.typOFSexpr rhs in
		let lhs, isObjAccess = match lhs with
			| Sast.SId(id, d) -> id_gen false false id d llbuilder, false
			| SObjAccess(e1, e2, d) -> obj_access_gen false e1 e2 d llbuilder, true
			| SArrayAccess(se, sel, d) -> arr_access_gen true se sel d llbuilder, true
			| _ -> raise (Failure("Left hand side must be assignable"))
		in
		let rhs = match rhs with 
			| Sast.SId(id, d) -> id_gen false false id d llbuilder
			| SObjAccess(e1, e2, d) -> obj_access_gen true e1 e2 d llbuilder
			| _ -> expr_gen llbuilder rhs
		in
		let rhs = match d with 
			  Datatype(Objecttype(_))	-> 
				if isObjAccess then rhs
				else build_load rhs "tmp" llbuilder
			| Datatype(Null_t) -> L.const_null (get_llvm_type d)
			| _ -> rhs 
		in
		let rhs = match d, rhs_t with
			  Datatype(Char_t), Datatype(Int_t) -> L.build_uitofp rhs i8_t "tmp" llbuilder
			| Datatype(Int_t), Datatype(Char_t) -> L.build_uitofp rhs i32_t "tmp" llbuilder
			| _ -> rhs
		in 
		ignore(L.build_store rhs lhs llbuilder);
		rhs
	
	and for_gen start cond step body llbuilder = 
		let preheader_bb = L.insertion_block llbuilder in
		let the_function = L.block_parent preheader_bb  in
		let start_val= expr_gen llbuilder start in
		let loop_bb = L.append_block context "loop" the_function in
		let step_bb = L.append_block context "step" the_function in	
		let cond_bb = L.append_block context "cond" the_function in
		let after_bb = L.append_block context "afterloop" the_function in
		ignore (L.build_br cond_bb llbuilder);
		L.position_at_end loop_bb llbuilder;
		ignore (stmt_gen llbuilder body);

		let bb = L.insertion_block llbuilder in
		L.move_block_after bb step_bb;
		L.move_block_after step_bb cond_bb;
		L.move_block_after cond_bb after_bb;
		ignore(L.build_br step_bb llbuilder);
		L.position_at_end step_bb llbuilder;	
		
		let _ = expr_gen  llbuilder step  in
		ignore(L.build_br cond_bb llbuilder);
		L.position_at_end cond_bb llbuilder;

		let cond_val = expr_gen llbuilder cond in
		ignore (L.build_cond_br cond_val loop_bb after_bb llbuilder);
		L.position_at_end after_bb llbuilder;
		const_null f_t

	and while_gen pred body_stmt llbuilder = 
		let null_sexpr = SInt_Lit(0) in
		for_gen null_sexpr pred null_sexpr body_stmt llbuilder

	and obj_access_gen is_assign lhs rhs d llbuilder = 
		let obj_func_gen param_ty fptr parent_expr el d llbuilder = 
			let match_sexpr se = match se with
				  SId(id, d) -> 
				  	let deref = match d with
						  Datatype(Objecttype(_)) -> false
						| _ -> true 
					in 
					id_gen deref false id d llbuilder
				| se -> expr_gen llbuilder se
			in
			let parent_expr = build_pointercast parent_expr param_ty "tmp" llbuilder in
			let params = List.map match_sexpr el in
			match d with
				  Datatype(Void_t) -> L.build_call fptr (Array.of_list (parent_expr :: params)) "" llbuilder
				| _ -> L.build_call fptr (Array.of_list (parent_expr :: params)) "tmp" llbuilder
		in

		let check_lhs = function
			  SId(s, d) -> id_gen false false s d llbuilder
			| SArrayAccess(e, el, d) -> arr_access_gen false e el d llbuilder
			| se -> raise (Failure("check lhd error"))
		in

		let rec check_rhs parent_expr parent_type = 
			let parent_str = Ast.string_of_object parent_type in
			function
				  SId(field, d) -> 
					let search_term = (parent_str ^ "." ^ field) in
					let field_index = Hashtbl.find struct_field_idx_table search_term in
					let _val = build_struct_gep parent_expr field_index field llbuilder in
					let _val = match d with 
						Datatype(Objecttype(_)) -> 
						if not is_assign then _val
						else build_load _val field llbuilder
					| _ ->
					if not is_assign then
						_val
					else
						build_load _val field llbuilder
					in
					_val
				| SCall(fname, el, d, index) 	-> 
					let index = const_int i32_t index in
					let c_index = build_struct_gep parent_expr 0 "cindex" llbuilder in
					let c_index = build_load c_index "cindex" llbuilder in
					let lookup_func = find_func_in_module "lookup" in
					let fptr = L.build_call lookup_func [| c_index; index |] "fptr" llbuilder in
					let fptr2 = find_func_in_module fname in
					let f_ty = type_of fptr2 in
					let param1 = param fptr2 0 in
					let param_ty = type_of param1 in
					let fptr = L.build_pointercast fptr f_ty fname llbuilder in
					let ret = obj_func_gen param_ty fptr parent_expr el d llbuilder in
					let ret = ret in
					ret
				| SObjAccess(e1, e2, d) 	-> 
					let e1_type = Semant.typOFSexpr e1 in
					let e1 = check_rhs parent_expr parent_type e1 in
					let e2 = check_rhs e1 e1_type e2 in
					e2
				| _ as e -> raise (Failure("invalid access"))
		in 
		let lhs_type = Semant.typOFSexpr lhs in 
		match lhs_type with
			  Arraytype(_, _) -> 
				let lhs = expr_gen llbuilder lhs in
				let _val = build_gep lhs [| (const_int i32_t 0) |] "tmp" llbuilder in
				build_load _val "tmp" llbuilder 
			| _ -> 
			let lhs = check_lhs lhs in
			let rhs = check_rhs lhs lhs_type rhs in
			rhs

	and arr_access_gen is_assign e el d llbuilder =
		let index = expr_gen llbuilder (List.hd el) in
		let index = match d with
			  Datatype(Char_t) -> index
			| _ -> L.build_add index (const_int i32_t 1) "tmp" llbuilder
		in
	    let arr = expr_gen llbuilder e in
	    let _val = L.build_gep arr [| index |] "tmp" llbuilder in
	    if is_assign
	    	then _val
	    	else build_load _val "tmp" llbuilder 
	in

	(*~~~~~~~~~~~~~~~~~~~~~~~~~ function generation top-level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
	let build_func sfdecl =

		Hashtbl.clear local_var_table;
		Hashtbl.clear formals_table;
		let fname = string_of_fname sfdecl.sfname in 
		let f =find_func_in_module fname in 	
		let llbuilder = L.builder_at_end context (L.entry_block f) in

		let init_formals f sformals =
			let sformals = Array.of_list (sformals) in
			Array.iteri (
				fun i a ->
		        	let formal = sformals.(i) in
		        	let string_of_formal_name = function
						Formal(_, s) -> s
						| _ -> " "
					in
		        	let formal_name = string_of_formal_name formal in
		        	L.set_value_name formal_name a;
		        	Hashtbl.add formals_table formal_name a;
		    ) 
		    (params f)
		in
		let _ = init_formals f sfdecl.sformals in 

		let _ = if sfdecl.overrides then
			let this_param = Hashtbl.find formals_table "this" in
			let source = Datatype(Objecttype(sfdecl.source)) in
			let casted_param = L.build_pointercast this_param (get_llvm_type source) "casted" llbuilder in
			Hashtbl.replace formals_table "this" casted_param;
		in

		let _  = stmt_gen llbuilder (SBlock (sfdecl.sbody)) in 
		if sfdecl.sreturnType = Datatype (Void_t)
		then ignore (L.build_ret_void llbuilder);
		()
	in
	let _ = List.map build_func functions in

	(*~~~~~~~~~~~~~~~~~~~~  main function generation top-level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
	let build_main main =
		Hashtbl.clear local_var_table;
		Hashtbl.clear formals_table;
		let fname = string_of_fname main.sfname in 
	        let fty = L.function_type i32_t[||] in 
		let f = L.define_function "main" fty the_module in 	
		let llbuilder = L.builder_at_end context (L.entry_block f) in
		
		let _ = stmt_gen llbuilder (SBlock (main.sbody)) in 
		
		
		L.build_ret (L.const_int i32_t 0) llbuilder
	in
	let _ = build_main main in

	(*~~~~~~~~~~~~~~~ virtual function table generation top-level ~~~~~~~~~~~~~~~~~~~~~~~~~~*)
	let build_vftable scdecls = 
		let rt = L.pointer_type i64_t in
		let void_pt = L.pointer_type i64_t in
		let void_ppt = L.pointer_type void_pt in

		let f = find_func_in_module "lookup" in
		let llbuilder = L.builder_at_end context (entry_block f) in

		let len = List.length scdecls in
		let total_len = ref 0 in
		let scdecl_llvm_arr = L.build_array_alloca void_ppt (const_int i32_t len) "tmp" llbuilder in

		let handle_scdecl scdecl = 
			let index = Hashtbl.find Semant.strucIndexes scdecl.scname in
			let len = List.length scdecl.sfuncs in
			let sfdecl_llvm_arr = L.build_array_alloca void_pt (const_int i32_t len) "tmp" llbuilder in

			let handle_fdecl i sfdecl = 
				let fptr = find_func_in_module (Ast.string_of_fname sfdecl.sfname) in
				let fptr = L.build_pointercast fptr void_pt "tmp" llbuilder in

				let ep = L.build_gep sfdecl_llvm_arr [| (const_int i32_t i) |] "tmp" llbuilder in
				ignore(build_store fptr ep llbuilder);
			in 
			List.iteri handle_fdecl scdecl.sfuncs;
			total_len := !total_len + len;

			let ep = L.build_gep scdecl_llvm_arr [| (const_int i32_t index) |] "tmp" llbuilder in
			ignore(build_store sfdecl_llvm_arr ep llbuilder);
		in
		List.iter handle_scdecl scdecls;

		let c_index = param f 0 in
		let f_index = param f 1 in
		set_value_name "c_index" c_index;
		set_value_name "f_index" f_index;

		if !total_len == 0 then
			build_ret (const_null rt) llbuilder
		else
			let vtbl = L.build_gep scdecl_llvm_arr [| c_index |] "tmp" llbuilder in
			let vtbl = L.build_load vtbl "tmp" llbuilder in
			let fptr = L.build_gep vtbl [| f_index |] "tmp" llbuilder in
			let fptr = L.build_load fptr "tmp" llbuilder in

			L.build_ret fptr llbuilder 
	in
	let _ = build_vftable classes in

	the_module;

