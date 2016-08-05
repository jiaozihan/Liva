(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

open Llvm
open Ast
open Sast
open Semant
open Hashtbl


module L = Llvm
module A = Ast
module S= Sast
module C= Char

module StringMap = Map.Make(String)

let context = global_context ()
let the_module = create_module context "Liva"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 50
let named_params:(string, llvalue) Hashtbl.t = Hashtbl.create 50
let struct_types:(string, lltype) Hashtbl.t = Hashtbl.create 10
let struct_field_indexes:(string, int) Hashtbl.t = Hashtbl.create 50


let str_type = Arraytype(Char_t, 1)(*to do*)


let i32_t = L.i32_type context;;
let i8_t = L.i8_type context;;
let f_t = L.double_type context;;
let i1_t = L.i1_type context;;
let str_t = L.pointer_type i8_t;;
let i64_t = L.i64_type context;;
let void_t = L.void_type context;;

let find_struct name = 
	try Hashtbl.find struct_types name
	with | Not_found -> raise(Failure ("undeclared struct"^ name))


let rec get_ptr_type datatype = match datatype with
		Arraytype(t, 0) -> get_type (Datatype(t))
	|	Arraytype(t, 1) -> pointer_type (get_type (Datatype(t)))
	|	Arraytype(t, i) -> pointer_type (get_ptr_type (Arraytype(t, (i-1))))
	| 	_ -> raise(Failure ("Invalid Array Pointer Type"))


and get_type datatype = match datatype with 
		Datatype(Int_t) -> i32_t
	| 	Datatype(Float_t) -> f_t
	| 	Datatype(Bool_t) -> i1_t
	| 	Datatype(Char_t) -> i8_t
	| 	Datatype(Void_t) -> void_t
	| 	Datatype(Null_t) -> i32_t
	| 	Datatype(Objecttype(name)) -> pointer_type(find_struct name)
	| 	Arraytype(t, i) -> get_ptr_type (Arraytype(t, (i)))
	| 	d -> raise(Failure ("Invalid DataType")) 


let codegen_struct_stub s =
	let struct_t = L.named_struct_type context s.scname in
	Hashtbl.add struct_types s.scname struct_t


let codegen_struct s = 
	let struct_t = Hashtbl.find struct_types s.scname in
	let type_list = List.map (function Field(d, _) -> get_type d) s.sfields in
	let name_list = List.map (function Field(_, s) -> s) s.sfields in

	(* Add key field to all structs *)
	let type_list = i32_t :: type_list in
	let name_list = ".key" :: name_list in

	let type_array = (Array.of_list type_list) in
	List.iteri (fun i f ->
        let n = s.scname ^ "." ^ f in
        Hashtbl.add struct_field_indexes n i;
    ) name_list;
	L.struct_set_body struct_t type_array true


let string_of_formal_name = function
		Formal(_, s) -> s
	| 	_ -> " "

let string_of_fname = function 
		Constructor -> "constructor"
	|	FName(s)	-> s


let init_parameters f formals =
	let formals = Array.of_list (formals) in
	Array.iteri (fun i a ->
        let n = formals.(i) in
        let n = string_of_formal_name n in
        L.set_value_name n a;
        Hashtbl.add named_params n a;
    ) (params f)


and func_lookup fname = 
	match (L.lookup_function fname the_module) with
		None -> raise (Failure("Function NotFound in module " ^ fname))
	|  	Some f -> f


(* expression code generation*)
let rec codegen_sexpr llbuilder = function

		SInt_Lit(i) -> L.const_int i32_t i
		| 	SBoolean_Lit(b) -> let temp = L.build_global_stringptr (string_of_boolean b) "str" llbuilder in temp
		|   SFloat_Lit(f)   -> L.const_float f_t  f
		|   SChar_Lit(c)    -> const_int i8_t (Char.code c)
		| 	SString_Lit s   -> codegen_string_lit s llbuilder

		|   SId(id, d)      -> codegen_id true false id d llbuilder
		|   SBinop(e1, op, e2, d)     -> binop_gen e1 op e2 d llbuilder
	    
		|   SAssign(e1, e2, d)        	-> assign_gen e1 e2 d llbuilder
	    |   SCall(fname, expr_list, d)  -> codegen_call llbuilder d expr_list fname

	    |   SObjectCreate(id, el, d)  	-> create_obj_gen id el d llbuilder


and codegen_stmt llbuilder = function 

	  SBlock sl        ->  List.hd (List.map (codegen_stmt llbuilder) sl)
	| SExpr (se, _)	   -> codegen_sexpr llbuilder se
	| SReturn(e, d)    -> codegen_ret d e llbuilder
	| SLocal(d, s, e)  -> codegen_alloca d s e llbuilder


and codegen_print expr_list llbuilder = 
	let printf = func_lookup "printf" in

	let map_expr_to_printfexpr expr =  codegen_sexpr llbuilder expr
	in

	let params = List.map map_expr_to_printfexpr expr_list in
	let param_types = List.map (Semant.get_type_from_sexpr) expr_list in 

	let map_param_to_string = function 
		Arraytype(Char_t, 1) 	-> "%s"
	| 	Datatype(Int_t) 		-> "%d"
	| 	Datatype(Float_t) 		-> "%f"
	| 	Datatype(Bool_t) 		-> "%s"
	| 	Datatype(Char_t) 		-> "%c"
	| 	_ 						-> raise (Failure("Print invalid type"))
	in
	let const_str = List.fold_left (fun s t -> s ^ map_param_to_string t) "" param_types in

	let s = codegen_sexpr llbuilder (SString_Lit(const_str)) in
	let zero = const_int i32_t 0 in 
	let s = L.build_in_bounds_gep s [| zero |] "tmp" llbuilder in
	build_call printf (Array.of_list (s :: params)) "tmp" llbuilder

and binop_gen e1 op e2 d llbuilder =
	let type1 = Semant.get_type_from_sexpr e1 in
	let type2 = Semant.get_type_from_sexpr e2 in

	(* Generate llvalues from e1 and e2 *)

	let e1 = codegen_sexpr llbuilder e1 in
	let e2 = codegen_sexpr llbuilder e2 in
	
	let float_ops op e1 e2 =
		match op with
			Add 		-> L.build_fadd e1 e2 "flt_addtmp" llbuilder
		| 	Sub 		-> L.build_fsub e1 e2 "flt_subtmp" llbuilder
		| 	Mult 		-> L.build_fmul e1 e2 "flt_multmp" llbuilder
		| 	Div 		-> L.build_fdiv e1 e2 "flt_divtmp" llbuilder
		| 	Mod 		-> L.build_frem e1 e2 "flt_sremtmp" llbuilder
		| 	Equal 		-> L.build_fcmp Fcmp.Oeq e1 e2 "flt_eqtmp" llbuilder
		| 	Neq 		-> L.build_fcmp Fcmp.One e1 e2 "flt_neqtmp" llbuilder
		| 	Less 		-> L.build_fcmp Fcmp.Ult e1 e2 "flt_lesstmp" llbuilder
		| 	Leq 		-> L.build_fcmp Fcmp.Ole e1 e2 "flt_leqtmp" llbuilder
		| 	Greater		-> L.build_fcmp Fcmp.Ogt e1 e2 "flt_sgttmp" llbuilder
		| 	Geq 		-> L.build_fcmp Fcmp.Oge e1 e2 "flt_sgetmp" llbuilder
		| 	_ 			-> raise(Failure("Invalid operator for floats"))
	in

	let int_ops op e1 e2 = 
		match op with
			Add 		-> L.build_add e1 e2 "addtmp" llbuilder
		| 	Sub 		-> L.build_sub e1 e2 "subtmp" llbuilder
		| 	Mult 		-> L.build_mul e1 e2 "multmp" llbuilder
		| 	Div 		-> L.build_sdiv e1 e2 "divtmp" llbuilder
		| 	Mod 		-> L.build_srem e1 e2 "sremtmp" llbuilder
		| 	Equal 		-> L.build_icmp Icmp.Eq e1 e2 "eqtmp" llbuilder
		| 	Neq 		-> L.build_icmp Icmp.Ne e1 e2 "neqtmp" llbuilder
		| 	Less 		-> L.build_icmp Icmp.Slt e1 e2 "lesstmp" llbuilder
		| 	Leq 		-> L.build_icmp Icmp.Sle e1 e2 "leqtmp" llbuilder
		| 	Greater		-> L.build_icmp Icmp.Sgt e1 e2 "sgttmp" llbuilder
		| 	Geq 		-> L.build_icmp Icmp.Sge e1 e2 "sgetmp" llbuilder
		| 	And 		-> L.build_and e1 e2 "andtmp" llbuilder
		| 	Or 			-> L.build_or  e1 e2 "ortmp" llbuilder
		| 	_ 			-> raise(Failure("Invalid operator for integers"))
	in	


	let type_handler d = match d with
			Datatype(Float_t)   -> float_ops op e1 e2
		|	Datatype(Int_t)	
		|   Datatype(Bool_t)
		| 	Datatype(Char_t) 	-> int_ops op e1 e2
		|   _ -> raise (Failure("Invalid binop type"))
	in

	type_handler d

and codegen_sizeof el llbuilder =
	let type_of_sexpr = Semant.get_type_from_sexpr (List.hd el) in
	let type_of_sexpr = get_type type_of_sexpr in
	let size_of_typ = (L.size_of type_of_sexpr) in	
	L.build_intcast size_of_typ i32_t "tmp" llbuilder

	(*raise(Failure("codegen size of cas Failure"));
	build_bitcast size_of_typ i32_t "tmp" llbuilder*)
	

and codegen_call llbuilder d expr_list = function
		"print" 	-> codegen_print expr_list llbuilder
	| 	"sizeof"	-> codegen_sizeof expr_list llbuilder
	| 	"cast" 		-> codegen_cast expr_list d llbuilder
	| 	"malloc" 	-> codegen_func_call "malloc" expr_list d llbuilder
	|   _ as call_name -> raise(Failure("function call not found: "^ call_name))


and codegen_cast el d llbuilder =
	let cast_malloc_to_objtype lhs currType newType llbuilder = match newType with
		Datatype(Objecttype(x)) -> 
			let obj_type = get_type (Datatype(Objecttype(x))) in 
			L.build_pointercast lhs obj_type "tmp" llbuilder
		| 	_ as t -> raise (Failure("cannot cast"))
	in
	let expr = List.hd el in
	let t = Semant.get_type_from_sexpr expr in
	let lhs = match expr with
	| 	Sast.SId(id, d) -> codegen_id false false id d llbuilder
	(*|  	SObjAccess(e1, e2, d) -> codegen_obj_access false e1 e2 d llbuilder
	| 	SArrayAccess(se, sel, d) -> codegen_array_access true se sel d llbuilder*)(*to do*)
	| _ -> codegen_sexpr llbuilder expr
	in
	cast_malloc_to_objtype lhs t d llbuilder

and codegen_func_call fname el d llbuilder = 
	let f = func_lookup fname in
	let params = List.map (codegen_sexpr llbuilder) el in
	match d with
		Datatype(Void_t) -> build_call f (Array.of_list params) "" llbuilder
	| 	_ -> 				build_call f (Array.of_list params) "tmp" llbuilder



and codegen_id isDeref checkParam id d llbuilder = 
	if isDeref then
		try Hashtbl.find named_params id
		with | Not_found ->
		try let _val = Hashtbl.find named_values id in
			build_load _val id llbuilder
		with | Not_found -> raise (Failure("unknown variable id " ^ id))
	else		
		try Hashtbl.find named_values id
		with | Not_found ->
			try 
				let _val = Hashtbl.find named_params id in
				L.build_load _val id llbuilder
			with | Not_found -> raise (Failure("unknown var id"))

and assign_gen lhs rhs d llbuilder = 
	let rhs_t = Semant.get_type_from_sexpr rhs in
	let lhs, isObjAccess = match lhs with
		| Sast.SId(id, d) -> codegen_id false false id d llbuilder, false
		| SObjAccess(e1, e2, d) -> codegen_obj_access false e1 e2 d llbuilder, true
		| _ -> raise (Failure("Left hand side must be assignable"))
	in
	let rhs = match rhs with 
		| 	Sast.SId(id, d) -> codegen_id true false id d llbuilder
		| _ -> codegen_sexpr llbuilder rhs
	in
	let rhs = match d with 
		Datatype(Objecttype(_))	-> 
			if isObjAccess then rhs
			else build_load rhs "tmp" llbuilder
		| 	Datatype(Null_t) -> const_null (get_type d)
		| _ -> rhs 
	in
	let rhs = match d, rhs_t with
			Datatype(Char_t), Datatype(Int_t) -> L.build_uitofp rhs i8_t "tmp" llbuilder
		| 	Datatype(Int_t), Datatype(Char_t) -> L.build_uitofp rhs i32_t "tmp" llbuilder
		| 	_ -> rhs
	in 
	ignore(L.build_store rhs lhs llbuilder);
	rhs


(*to do*)
and codegen_obj_access isAssign lhs rhs d llbuilder = 
	let codegen_func_call param_ty fptr parent_expr el d llbuilder = 
		let match_sexpr se = match se with
			SId(id, d) -> let isDeref = match d with
				Datatype(Objecttype(_)) -> false
			| 	_ -> true 
			in codegen_id isDeref false id d llbuilder
		| 	se -> codegen_sexpr llbuilder se
		in
		let parent_expr = build_pointercast parent_expr param_ty "tmp" llbuilder in
		let params = List.map match_sexpr el in
		match d with
			Datatype(Void_t) -> build_call fptr (Array.of_list (parent_expr :: params)) "" llbuilder
		| 	_ -> build_call fptr (Array.of_list (parent_expr :: params)) "tmp" llbuilder
	in
	let check_lhs = function
		SId(s, d)				-> codegen_id false false s d llbuilder
	(*| 	SArrayAccess(e, el, d)	-> codegen_array_access false e el d llbuilder*)
	| 	se 	-> raise (Failure("check lhd error"))
	in
	(* Needs to be changed *)
	let rec check_rhs isLHS parent_expr parent_type = 
		let parent_str = Ast.string_of_object parent_type in
		function
			(* Check fields in parent *)
			SId(field, d) -> 
				let search_term = (parent_str ^ "." ^ field) in
				let field_index = Hashtbl.find struct_field_indexes search_term in
				let _val = build_struct_gep parent_expr field_index field llbuilder in
				let _val = match d with 
					Datatype(Objecttype(_)) -> 
					if not isAssign then _val
					else build_load _val field llbuilder
				| _ ->
				if not isAssign then
					_val
				else
					build_load _val field llbuilder
				in
				_val

		(*to do array access*)
		(*to do call*)

			(* Set parent, check if base is field *)
		| 	SObjAccess(e1, e2, d) 	-> 
				let e1_type = Semant.get_type_from_sexpr e1 in
				let e1 = check_rhs true parent_expr parent_type e1 in
				let e2 = check_rhs true e1 e1_type e2 in
				e2
		| 	_ as e -> raise (Failure("invalid access"))
	in 
	let lhs_type = Semant.get_type_from_sexpr lhs in 
	match lhs_type with
		Arraytype(_, _) -> 
			let lhs = codegen_sexpr llbuilder lhs in
			let _ = match rhs with
				SId("length", _) -> "length"
			| 	_ -> raise(Failure("cannot only acces length of array"))
			in
			let _val = build_gep lhs [| (const_int i32_t 0) |] "tmp" llbuilder in
			build_load _val "tmp" llbuilder 
	| 	_ -> 
		let lhs = check_lhs lhs in
		let rhs = check_rhs true lhs lhs_type rhs in
		rhs







and codegen_string_lit s llbuilder = 
	if s = "true" then build_global_stringptr "true" "tmp" llbuilder
	else if s = "false" then build_global_stringptr "false" "tmp" llbuilder
	else build_global_stringptr s "tmp" llbuilder


  
and codegen_alloca datatype var_name expr llbuilder = 
	let t = match datatype with 
		Datatype(Objecttype(name)) -> find_struct name
		|  _ -> get_type datatype
	in
	let alloca = build_alloca t var_name llbuilder in
	Hashtbl.add named_values var_name alloca;
	let lhs = SId(var_name, datatype) in
	match expr with 
		SNoexpr -> alloca
	|  	_ -> assign_gen lhs expr datatype llbuilder


and codegen_ret d expr llbuilder =  
	match expr with
		SId(name, d) ->
			(match d with 
			| Datatype(Objecttype(_)) -> build_ret (codegen_id false false name d llbuilder) llbuilder
			| _ -> build_ret (codegen_id true true name d llbuilder) llbuilder)
		| SObjAccess(e1, e2, d) -> build_ret (codegen_obj_access true e1 e2 d llbuilder) llbuilder
		| SNoexpr -> build_ret_void llbuilder
		| _ -> build_ret (codegen_sexpr llbuilder expr) llbuilder

	

and create_obj_gen fname el d llbuilder = 
	let f = func_lookup fname in
	let params = List.map (codegen_sexpr llbuilder) el in
	let obj = build_call f (Array.of_list params) "tmp" llbuilder in
	obj

(*function code generation*)
let codegen_func_stub sfdecl=

	let fname = sfdecl.sfname in
	let is_var_arg =ref false in 
	let parameters = List.rev ( List.fold_left 
	(fun l -> (function 
		Formal (t,_)-> get_type t::l 
	      | _ -> is_var_arg = ref true; l))[]  sfdecl.sformals)
		
	in 

	let fty = if !is_var_arg 
		then L.var_arg_function_type (get_type sfdecl.sreturnType )
	(Array.of_list parameters)
		else L.function_type (get_type sfdecl.sreturnType)
	(Array.of_list parameters)
			
	in 
	L.define_function (string_of_fname fname) fty the_module

let codegen_func sfdecl =

	Hashtbl.clear named_values;
	Hashtbl.clear named_params;
	let fname = string_of_fname sfdecl.sfname in 
	let f =func_lookup fname in 	
	let llbuilder = L.builder_at_end context (L.entry_block f) in
	let _ =init_parameters f sfdecl.sformals in 
	let _  = codegen_stmt llbuilder (SBlock (sfdecl.sbody)) in 
	if sfdecl.sreturnType = Datatype (Void_t)
	 then ignore (L.build_ret_void llbuilder);
	()

let codegen_library_functions () = 
	let printf_t = var_arg_function_type i32_t [| pointer_type i8_t |] in
	let _ = declare_function "printf" printf_t the_module in

	let malloc_ty = function_type (str_t) [| i32_t |] in
	let _ = declare_function "malloc" malloc_ty the_module in
	()

(*main funtion generation*)
let codegen_main main =

	Hashtbl.clear named_values;
	Hashtbl.clear named_params;
	let fname = string_of_fname main.sfname in 
        let fty = L.function_type i32_t[||] in 
	let f = L.define_function "main" fty the_module in 	
	let llbuilder = L.builder_at_end context (L.entry_block f) in
	
	let _ = codegen_stmt llbuilder (SBlock (main.sbody)) in 
	
	
	L.build_ret (L.const_int i32_t 0) llbuilder


let translate sast = 

	let classes = sast.classes in
	let functions = sast.functions in
    let main =sast.main in 
	
	let _ = codegen_library_functions () in
	let _ = List.map (fun s -> codegen_struct_stub s) classes in
	let _ = List.map (fun s -> codegen_struct s) classes in
	let _ = List.map (fun s-> codegen_func_stub s) functions in 
	let _ = List.map (fun s-> codegen_func s) functions in
	let _ = codegen_main main in

	the_module;
	




















 
 
