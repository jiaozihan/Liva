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




let i32_t = i32_type context;;
let i8_t = i8_type context;;
let f_t = double_type context;;
let i1_t = i1_type context;;
let str_t = pointer_type i8_t;;
let i64_t = i64_type context;;
let void_t = void_type context;;

(*control flow references*)
let br_block = ref (L.block_of_value (L.const_int i32_t 0))
and cont_block = ref (L.block_of_value (L.const_int i32_t 0))
and is_loop = ref false 



let find_struct name = 
	try Hashtbl.find struct_types name
	with | Not_found -> raise(Failure ("undeclared struct"^ name))


let rec get_ptr_type datatype = match datatype with
		Arraytype(t, 0) -> get_type (Datatype(t))
	|	Arraytype(t, 1) -> pointer_type (get_type (Datatype(t)))
	|	Arraytype(t, i) -> pointer_type (get_ptr_type (Arraytype(t, (i-1))))
	| 	_ -> raise(Failure ("Invalid Array Pointer Type"))


and get_type   datatype = match datatype with 
		Datatype(Int_t) -> i32_t
	| 	Datatype(Float_t) -> f_t
	| 	Datatype(Bool_t) -> i1_t
	| 	Datatype(Char_t) -> i8_t
	| 	Datatype(Void_t) -> void_t
	| 	Datatype(Null_t) -> i32_t
	| 	Datatype(Objecttype(name)) -> pointer_type(find_struct name)
	| 	Arraytype(t, i) -> get_ptr_type (Arraytype(t, (i)))
	| 	d -> raise(Failure ("Invalid DataType")) 

(* Declare printf(), which the print built-in function will call *)
(*
let printf_t = L.var_arg_function_type i32_t [| |] 
let printf_func = L.declare_function "printf" printf_t the_module 
*)


(* Declare and Define classes*)
let codegen_struct_stub s =
	let struct_t = named_struct_type context s.scname in
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
	struct_set_body struct_t type_array true








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
		None -> raise (Failure("LLVM Function NotFound"))
	|  	Some f -> f


(* expression code generation*)

let rec codegen_print expr_list llbuilder = 
	let printf = func_lookup "printf" in

	let map_expr_to_printfexpr expr =  codegen_sexpr expr llbuilder
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

	let s = codegen_sexpr (SString_Lit(const_str)) llbuilder in
	let zero = const_int i32_t 0 in 
	let s = L.build_in_bounds_gep s [| zero |] "tmp" llbuilder in
	build_call printf (Array.of_list (s :: params)) "tmp" llbuilder

and handle_binop e1 op e2 d llbuilder =
	let type1 = Semant.get_type_from_sexpr e1 in
	let type2 = Semant.get_type_from_sexpr e2 in

	(* Generate llvalues from e1 and e2 *)

	let e1 = codegen_sexpr e1 llbuilder in
	let e2 = codegen_sexpr e2 llbuilder in
	
	let float_ops op e1 e2 =
		match op with
			Add 		-> L.build_fadd e1 e2 "flt_addtmp" llbuilder
		| 	Sub 		-> L.build_fsub e1 e2 "flt_subtmp" llbuilder
	in

	let int_ops op e1 e2 = 
		match op with
			Add 		-> L.build_add e1 e2 "addtmp" llbuilder
		| 	Sub 		-> L.build_sub e1 e2 "subtmp" llbuilder
	in	

	(*let (e1, e2), d = cast e1 e2 type1 type2 llbuilder in*)

	let type_handler d = match d with
			Datatype(Float_t)   -> float_ops op e1 e2
		|	Datatype(Int_t)	
		|   Datatype(Bool_t)
		| 	Datatype(Char_t) 	-> int_ops op e1 e2
		|   _ -> raise (Failure("Invalid binop type"))
	in

	type_handler d


 (*L.build_call printf_func [| print_format e ; (codegen_sexpr e llbuilder) |]
	    "printf" llbuilder*)
and codegen_sizeof el llbuilder =
	let type_of = Semant.get_type_from_sexpr (List.hd el) in
	let type_of = get_type type_of in
	let size_of = size_of type_of in
	build_bitcast size_of i32_t "tmp" llbuilder

and codegen_call llbuilder d expr_list = function
		"print" 	-> codegen_print expr_list llbuilder
	| 	"sizeof"	-> codegen_sizeof expr_list llbuilder

and codegen_id isDeref checkParam id d llbuilder = 
	if isDeref then
		try Hashtbl.find named_params id
		with | Not_found ->
		try let _val = Hashtbl.find named_values id in
			build_load _val id llbuilder
		with | Not_found -> raise (Failure("unknown variable id"))
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
		| _ -> raise (Failure("Left hand side must be assignable"))
	in
	let rhs = match rhs with 
		| 	Sast.SId(id, d) -> codegen_id true false id d llbuilder
		| _ -> codegen_sexpr rhs llbuilder
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
	(* Lookup the name. *)
	ignore(L.build_store rhs lhs llbuilder);
	rhs

and codegen_string_lit s llbuilder = 
	if s = "true" then build_global_stringptr "true" "tmp" llbuilder
	else if s = "false" then build_global_stringptr "false" "tmp" llbuilder
	else build_global_stringptr s "tmp" llbuilder

and codegen_sexpr sexpr llbuilder = 
	
	match sexpr with 
		    SInt_Lit(i) -> L.const_int i32_t i
		|   SBoolean_Lit(b) -> if b then const_int i1_t 1 else const_int i1_t 0
		|   SFloat_Lit(f)   -> L.const_float f_t  f
		|   SChar_Lit(c)    -> const_int i8_t (Char.code c)
		|   SString_Lit s   -> codegen_string_lit s llbuilder

		|   SId(id, d)      -> codegen_id true false id d llbuilder
		|   SBinop(e1, op, e2, d)     	-> handle_binop e1 op e2 d llbuilder
	    
		|   SAssign(e1, e2, d)        	-> assign_gen e1 e2 d llbuilder
	        |   SCall(fname, expr_list, d)  -> codegen_call llbuilder d expr_list fname
  
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
		| SNoexpr -> build_ret_void llbuilder
		| _ -> L.build_ret (codegen_sexpr expr llbuilder) llbuilder

and codegen_continue llbuilder =
	let b= fun() -> !cont_block in
	L.build_br (b()) llbuilder

and codegen_break llbuilder =
  	let b= fun() -> !br_block in
	L.build_br (b ()) llbuilder



(*statement code generation*)
and codegen_stmt llbuilder = function 

	  SBlock sl        -> List.hd (List.map (codegen_stmt llbuilder) sl)
	| SExpr (se, _)	   -> codegen_sexpr se llbuilder
	| SReturn(e, d)    -> codegen_ret d e llbuilder
	| SLocal(d, s, e)  -> codegen_alloca d s e llbuilder
	| SBreak 	   -> codegen_break llbuilder 
	| SContinue    	   -> codegen_continue llbuilder
	| SIf (se, s1, s2)  -> codegen_if_stmt se s1 s2 llbuilder



and codegen_for_stmt init cond increase body llbuilder = "t0 do"

		



and codegen_while_stmt cond body illbuilder = "to do"








and codegen_if_stmt pred then_stmt else_stmt llbuilder =
	let cond= codegen_sexpr pred llbuilder in

	let start_bb = insertion_block llbuilder in
	let the_function = block_parent start_bb in

	let then_bb = append_block context "then" the_function in

	position_at_end then_bb llbuilder;
	let then_val = codegen_stmt llbuilder then_stmt in


	let new_then_bb = insertion_block llbuilder in

	let else_bb = append_block context "else" the_function in
	position_at_end else_bb llbuilder;
	let else_val= codegen_stmt llbuilder else_stmt in


	let new_else_bb = insertion_block llbuilder in

	
	let merge_bb = append_block context "ifcont" the_function in
	position_at_end merge_bb builder;
	let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
	let phi = build_phi incoming "iftmp" builder in
	

	
	position_at_end start_bb llbuilder;
	ignore (build_cond_br cond then_bb else_bb llbuilder);


	position_at_end new_then_bb llbuilder; ignore (build_br merge_bb llbuilder);
	position_at_end new_else_bb llbuilder; ignore (build_br merge_bb llbuilder);

	(* Finally, set the builder to the end of the merge block. *)
	position_at_end merge_bb llbuilder;

	phi


	
	
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
	()

(*main funtion generation*)
let codegen_main main =

	Hashtbl.clear named_values;
	Hashtbl.clear named_params;
	let fname = string_of_fname main.sfname in 
        let fty = L.function_type i32_t[||] in 
	let f = L.define_function "main" fty the_module in 	
	let llbuilder = L.builder_at_end context (L.entry_block f) in

	(*let argc = L.param f 0 in 
	let argv =L.param f 1 in 
	L.set_value_name "argc" argc;
	L.set_value_name "argv" argv;
	
	Hashtbl.add named_params "argc"  argc;
	Hashtbl.add named_params "argv"  argv;*)
	
	let _ = codegen_stmt llbuilder (SBlock (main.sbody)) in 
	
	
	L.build_ret (L.const_int i32_t 0) llbuilder


(***********************************************************)
(* Entry point for translating Ast.program to LLVM module *)
(***********************************************************)

(* Return the module*)
let translate sast = 

	let classes = sast.classes in
	let functions = sast.functions in
    let main =sast.main in 
	
	let _ = codegen_library_functions () in
	let _ = List.map (fun s -> codegen_struct_stub s) classes in
	let _ = List.map (fun s -> codegen_struct s) classes in
	let _ = codegen_main main in
	let _ = List.map (fun s-> codegen_func_stub s) functions in 
	let _ = List.map (fun s-> codegen_func s) functions in
	

	the_module;
	




















 
 
