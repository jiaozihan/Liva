(* Semantic checking for the Liva compiler *)

open Ast
open Sast


(* Semantic checking of a program. Returns Sast if successful,
   throws an exception if something is wrong. *)

module StringMap = Map.Make(String)
module StringSet = Set.Make (String)

module SS = Set.Make(
	struct
		let compare = Pervasives.compare
		type t = datatype
	end )

type class_map = {
	field_map               : Ast.field StringMap.t;
	func_map                : Ast.func_decl StringMap.t;
	constructor_map         : Ast.func_decl StringMap.t;
	reserved_functions_map 	: sfunc_decl StringMap.t;
	cdecl 			        : Ast.class_decl;
}

type env ={

	env_class_maps: class_map StringMap.t;
	env_name: string;
	env_cmap: class_map;
	env_locals: datatype StringMap.t;
	env_parameters: Ast.formal StringMap.t;
	env_returnType:datatype;
	env_in_for: bool;
	env_in_while: bool;
	env_reserved:sfunc_decl list;
}

let update_env_name env env_name = 
{
	env_class_maps = env.env_class_maps;
	env_name       = env_name;
	env_cmap 	   = env.env_cmap;
	env_locals     = env.env_locals;
	env_parameters = env.env_parameters;
	env_returnType = env.env_returnType;
	env_in_for     = env.env_in_for;
	env_in_while   = env.env_in_while;
	env_reserved   = env.env_reserved;
}

let struct_indexes: (string, int) Hashtbl.t =  Hashtbl.create 10

let build_struct_indexes cdecls= 
	let cdecls_handler index cdecl=
	Hashtbl.add struct_indexes cdecl.cname index in 
	List.iteri cdecls_handler cdecls

let default_c = 
{
	fname      = Ast.Constructor;
	returnType = Datatype(ConstructorType);
	formals    = [];
	body       = [];
}

let default_sc cname = 
{
	sfname 		= Ast.FName (cname ^ "." ^ "constructor");
	sreturnType = Datatype(Objecttype(cname));
	sformals 	= [];
	sbody 		= [] (*TODO*);
	func_type	= Sast.User;
	source 		= "NA";
}

let get_name cname fdecl = (*get the name of function,cname.constructor-> constructor / cname.xxx-> normal_function / main*)
	let name = string_of_fname fdecl.fname (*tell constructor from normal function*)
	in
	if name = "main" 
		then "main"
	else cname ^ "." ^ name

let get_equality_binop_type type1 type2 se1 se2 op =
	if (type1 = Datatype(Float_t) || type2 = Datatype(Float_t)) (*unqualified types*)
		then raise (Failure ("InvalidBinopExpression: " ^ "Equality operation is not supported for Float types"))
	else
		match type1, type2 with (*qualified types*)
			  Datatype(Char_t), Datatype(Int_t) 
			| Datatype(Int_t), Datatype(Char_t)
			| Datatype(Objecttype(_)), Datatype(Null_t)
			| Datatype(Null_t), Datatype(Objecttype(_))	 -> SBinop(se1, op, se2, Datatype(Bool_t))
			| _                                          -> if type1 = type2
																then SBinop(se1, op, se2, Datatype(Bool_t))
															else raise (Failure ("InvalidBinopExpression: " ^ "Equality operator can't operate on different types, with the exception of Int_t and Char_t"))

let get_logical_binop_type se1 se2 op = function (*check operants and conver to sbinop*)
	 (Datatype(Bool_t), Datatype(Bool_t)) -> SBinop(se1, op, se2, Datatype(Bool_t))
	| _                                   -> raise (Failure ("InvalidBinopExpression: " ^ "Logical operators only operate on Bool_t types"))

let get_comparison_binop_type type1 type2 se1 se2 op =
	let numerics = SS.of_list [Datatype(Int_t); Datatype(Char_t); Datatype(Float_t)](*qualified operant types*)
	in
		if SS.mem type1 numerics && SS.mem type2 numerics
			then SBinop(se1, op, se2, Datatype(Bool_t))
		else raise (Failure ("InvalidBinopExpression: " ^ "Comparison operators operate on numeric types only"))

let get_arithmetic_binop_type se1 se2 op = function
	(*qualified combination of operant type*)
	  (Datatype(Int_t), Datatype(Float_t)) 
	| (Datatype(Float_t), Datatype(Int_t)) 
	| (Datatype(Float_t), Datatype(Float_t)) -> SBinop(se1, op, se2, Datatype(Float_t))
	| (Datatype(Int_t), Datatype(Char_t)) 
	| (Datatype(Char_t), Datatype(Int_t)) 
	| (Datatype(Char_t), Datatype(Char_t)) 	 -> SBinop(se1, op, se2, Datatype(Char_t))
	| (Datatype(Int_t), Datatype(Int_t)) 	 -> SBinop(se1, op, se2, Datatype(Int_t))
	| _ -> raise (Failure ("Arithmetic operators don't support these types"))

let rec get_ID_type env s = 
	try
		StringMap.find s env.env_locals
	with
		| Not_found -> try let formal = StringMap.find s env.env_parameters
						   in (function   Formal(t, _) -> t 
										| Many t -> t) formal
					   with  | Not_found -> raise (Failure ("UndefinedID: " ^ s))

and check_array_init env d el = 
	let array_complexity = List.length el(*get the size of array*)
	in
	let check_index_type e = (*check whether the type of index is int*)
		let sexpr, _ = expr_to_sexpr env e (*convert expression to sexpression*)
		in
			let sexpr_type = get_type_from_sexpr sexpr (*get the type of sexpression*)
			in
				if sexpr_type = Datatype(Int_t) 
					then sexpr
				else raise (Failure ("MustPassIntegerTypeToArrayCreate"))
	in
		let convert_dtyp_to_arraytyp = function(*check whether the type is array*)
			   Datatype(x) -> Arraytype(x, array_complexity)
			|  _ as t      -> raise (Failure ("ArrayInitTypeInvalid: " ^ (string_of_datatype t)))
		in
			let sexpr_type = convert_dtyp_to_arraytyp d
			in
				let sel = List.map check_index_type el
				in
					SArrayCreate(d, sel, sexpr_type)

and check_array_access env e el = 
	let array_dimensions = List.length el (*get the size of array*)
	in
		let check_index_type arg = (*check whether the type of index is int*)
			let sexpr, _ = expr_to_sexpr env arg (*convert expression to sexpression*)
			in
				let sexpr_type = get_type_from_sexpr sexpr (*get the type of sexpression*)
				in
					if sexpr_type = Datatype(Int_t) 
						then sexpr
					else raise (Failure ("MustPassIntegerTypeToArrayAccess"))
		in
			let se, _ = expr_to_sexpr env e (*convert expression to sexpression*)
			in
				let se_type = get_type_from_sexpr se (*get the type of sexpression*)
				in
					let check_array_dim num_params = function
						   Arraytype(t, n) -> if num_params < n
												then Arraytype(t, (n-num_params))(*remain, now a smaller array*)
											  else if num_params = n
												then Datatype(t)
											  else raise (Failure ("ArrayAccessInvalidParamLength: " ^ (string_of_int num_params) ^ " > " ^ (string_of_int n)))
						|  _ as t          -> raise (Failure ("ArrayAccessExpressionNotArray: " ^ (string_of_datatype t)))	
					in
						let sexpr_type = check_array_dim array_dimensions se_type
						in
							let sel = List.map check_index_type el
							in SArrayAccess(se, sel, sexpr_type)

and check_obj_access env lhs rhs =
	let check_lhs = function(*check the expression before ‘.’ and get sexpression*)
		This 			-> SId("this", Datatype(Objecttype(env.env_name)))
	|	Id s 			-> SId(s, get_ID_type env s)
	| 	ArrayAccess(e, el)	-> check_array_access env e el 
	| 	_ as e 	-> raise (Failure ("LHSofRootAccessMustBeIDorFunc: " ^ string_of_expr e))
	in
	let ptype_name parent_type = match parent_type with (*get the type of the expression before ‘.’, i.e. class name*)
			Datatype(Objecttype(name)) 	-> name
		| 	_ as d						-> raise (Failure ("ObjAccessMustHaveObjectType: " ^ string_of_datatype d))
	in
	let rec check_rhs (env) parent_type=
		let pt_name = ptype_name parent_type in(*get the class name*)
		let get_id_type_from_object env (id) cname=
			let cmap = StringMap.find cname env.env_class_maps in(*get the class map of current class*)
			let match_field f = match f with (*get datatype of the expression after ‘.’*)
				Field(d, n) -> d
			in
			try match_field (StringMap.find id cmap.field_map)
			with | Not_found -> raise (Failure ("UnknownIdentifierForClass: " ^ id ^ " -> " ^ cname))
		in
		function
			Id s 				-> SId(s, (get_id_type_from_object env s pt_name )), env (* Check fields*)
		| 	Call(fname, el) 	-> (* Check functions*)
				let env = update_env_name env pt_name in
				check_call_type env fname el, env
			(* Set parent, check if base is field *)
		| 	ObjAccess(e1, e2) 	-> (*多个 '.'*)(*nested? 删?问问问问问*)
				let old_env = env in
				let lhs, env = check_rhs env parent_type e1 in
				let lhs_type = get_type_from_sexpr lhs in

				let pt_name = ptype_name lhs_type in
				let lhs_env = update_env_name env pt_name in

				let rhs, env = check_rhs lhs_env lhs_type e2 in
				let rhs_type = get_type_from_sexpr rhs in
				SObjAccess(lhs, rhs, rhs_type), old_env
		| 	_ as e				-> raise (Failure ("InvalidAccessLHS: " ^ string_of_expr e))
	in 
	let arr_lhs, _ = expr_to_sexpr env lhs in(*convert expression to sexpression*)
	let arr_lhs_type = get_type_from_sexpr arr_lhs in (*get the type of sexpression*)
		let lhs = check_lhs lhs in
		let lhs_type = get_type_from_sexpr lhs in 

		let ptype_name = ptype_name lhs_type in
		let lhs_env = update_env_name env ptype_name in

		let rhs, _ = check_rhs lhs_env lhs_type rhs in
		let rhs_type = get_type_from_sexpr rhs in
		SObjAccess(lhs, rhs, rhs_type)

and check_call_type env fname el =
	let sel, env = exprl_to_sexprl env el(*convert expression list to sexpression list*)
	in
		let cmap = try StringMap.find env.env_name env.env_class_maps (*check whether the class has been defined*)
				   with | Not_found -> raise (Failure ("UndefinedClass: " ^ env.env_name))
		in(*check type*)
			let check_pa_onebyone formal param = (*get the type of formal parameter*)
				let ftyp = match formal with
					  Formal(d, _) -> d
					| _            -> Datatype(Void_t)
				in
					let ptyp = get_type_from_sexpr param(*get the type of actual parameter*)
					in
						if ftyp = ptyp
							then param
						else raise (Failure ("IncorrectTypePassedToFunction: " ^ string_of_datatype ptyp ^ " -> " ^ fname))
			in
				let check_params (formals) params = match formals, params with (*check parameter according to amount*)
														  [Many(Any)], _ -> params
														| [], []         -> []
														| [], _
														| _, []          -> raise (Failure ("IncorrectTypePassedToFunction: " ^ string_of_datatype (Datatype(Void_t)) ^ " -> " ^ fname))
														| _              -> let len1 = List.length formals
																			in
																				let len2 = List.length params
																				in
																					if len1 <> len2
																						then raise (Failure ("IncorrectNumberOfArguments: " ^ fname))
																					else List.map2 check_pa_onebyone formals sel
				in
					try
						let func = StringMap.find fname cmap.reserved_functions_map
						in
							let actuals = check_params func.sformals sel
							in SCall(fname, actuals, func.sreturnType)
					with | Not_found -> let sfname = env.env_name ^ "." ^ fname
										in
											try let f = StringMap.find sfname cmap.func_map
												in
													let actuals = check_params f.formals sel
													in SCall(sfname, actuals, f.returnType)
											with | Not_found -> raise (Failure ("IncorrectNumberOfArguments: " ^ sfname))
												 | _ as ex   -> raise ex

and check_object_constructor env s el =
	let sel, env = exprl_to_sexprl env el
	in
		let cmap = try StringMap.find s env.env_class_maps(*find the class*)
				   with | Not_found -> raise (Failure ("UndefinedClass: " ^ s))
		in
			let params = List.fold_left (fun s e -> s ^ "." ^ (string_of_datatype (get_type_from_sexpr e))) "" sel
			in
				let constructor_name = s ^ "." ^ "constructor" ^ params
				in
					let _ = try StringMap.find constructor_name cmap.constructor_map(*find constructor with type check*)
							with  | Not_found -> raise (Failure ("ConstructorNotFound: " ^ constructor_name))
					in
						let obtyp = Datatype(Objecttype(s))
						in
							SObjectCreate(constructor_name, sel, obtyp)

and check_assign env e1 e2 =(*后面的检查类型和表达式 还有类型和类型 的组合部分 没大搞懂,不明白为什么就合法了*)
	let se1, env = expr_to_sexpr env e1(*convert expression to sexpression*)
	in
		let se2, env = expr_to_sexpr env e2
		in
			let type1 = get_type_from_sexpr se1(*get the type of sexpression*)
			in
				let type2 = get_type_from_sexpr se2
				in 
					match (type1, se2) with
						  Datatype(Objecttype(_)), SNull -> SAssign(se1, se2, type1)
						| _                              -> 
															match type1, type2 with
																 Datatype(Objecttype(d)), Datatype(Objecttype(t)) ->
																		if d = t 
																			then SAssign(se1, se2, type1)
																		else raise (Failure ("AssignmentTypeMismatch: " ^ string_of_datatype type1 ^ " <-> " ^ string_of_datatype type2))
																| _ -> if type1 = type2 
																		then SAssign(se1, se2, type1)
																		else raise (Failure ("AssignmentTypeMismatch: " ^ string_of_datatype type1 ^ " <-> " ^ string_of_datatype type2))

and check_unop env op e = 
	let check_num_unop t = function(*operator for number*)
		   Sub -> t
		|  _   -> raise (Failure ("InvalidUnaryOperation"))
	in 
		let check_bool_unop = function(*operator for bool*)
			   Not -> Datatype(Bool_t)
			|  _   -> raise (Failure ("InvalidUnaryOperation"))
		in
			let se, env = expr_to_sexpr env e (*convert expression to sexpression*)
			in
				let t = get_type_from_sexpr se (*get the type of sexpression*)
				in
					match t with (*check the type of operand*)
						   Datatype(Int_t) 	
						|  Datatype(Float_t) -> SUnop(op, se, check_num_unop t op)
						|  Datatype(Bool_t)  -> SUnop(op, se, check_bool_unop op)
						|  _                 -> raise (Failure ("InvalidUnaryOperation"))

and check_binop env e1 op e2 =
	let se1, env = expr_to_sexpr env e1 (*convert expression to sexpression*)
	in
		let se2, env = expr_to_sexpr env e2 (*convert expression to sexpression*)
		in
			let type1 = get_type_from_sexpr se1(*get the type of sexpression*)
			in
				let type2 = get_type_from_sexpr se2(*get the type of sexpression*)
				in
					match op with(*check and convert binopexpression according to binopexpression type*)
						    Equal | Neq                  -> get_equality_binop_type type1 type2 se1 se2 op
						|	And | Or                     -> get_logical_binop_type se1 se2 op (type1, type2)
						|	Less | Leq | Greater | Geq   -> get_comparison_binop_type type1 type2 se1 se2 op
						|	Add | Mult | Sub | Div | Mod -> get_arithmetic_binop_type se1 se2 op (type1, type2) 
						| 	_                            -> raise (Failure ("InvalidBinopExpression: " ^ (string_of_op op) ^ " is not a supported binary op"))

and expr_to_sexpr env = function
	Int_Lit i           -> SInt_Lit(i), env
|   Boolean_Lit b       -> SBoolean_Lit(b), env
|   Float_Lit f         -> SFloat_Lit(f), env
|   String_Lit s        -> SString_Lit(s), env
|   Char_Lit c          -> SChar_Lit(c), env
|   This                -> SId("this", Datatype(Objecttype(env.env_name))), env
|   Id s                -> SId(s, get_ID_type env s), env
|   Null                -> SNull, env
|   Noexpr              -> SNoexpr, env
|   ObjAccess(e1, e2)   -> check_obj_access env e1 e2, env
|   ObjectCreate(s, el) -> check_object_constructor env s el, env
|   Call(s, el)         -> check_call_type env s el, env
|   ArrayCreate(d, el)  -> check_array_init env d el, env
|   ArrayAccess(e, el)  -> check_array_access env e el, env
|   Assign(e1, e2)      -> check_assign env e1 e2, env
|   Unop(op, e)         -> check_unop env op e, env
|   Binop(e1, op, e2)   -> check_binop env e1 op e2, env

and get_type_from_sexpr = function(*get the type of sexpression*)
	SInt_Lit(_)				-> Datatype(Int_t)
| 	SBoolean_Lit(_)			-> Datatype(Bool_t)
| 	SFloat_Lit(_)			-> Datatype(Float_t)
| 	SString_Lit(_) 			-> Arraytype(Char_t, 1)
| 	SChar_Lit(_) 			-> Datatype(Char_t)
| 	SId(_, d) 				-> d
| 	SBinop(_, _, _, d) 		-> d
| 	SAssign(_, _, d) 		-> d
| 	SNoexpr 				-> Datatype(Void_t)
| 	SArrayCreate(_, _, d)	-> d
| 	SArrayAccess(_, _, d) 	-> d
| 	SObjAccess(_, _, d)		-> d
| 	SCall(_, _, d)		-> d
|   SObjectCreate(_, _, d) 	-> d
| 	SArrayPrimitive(_, d)	-> d
|  	SUnop(_, _, d) 			-> d
| 	SNull					-> Datatype(Null_t)

and exprl_to_sexprl env el = (*convert expression list to sexpression list*)
	let env_ref = ref env
	in
		let rec assistant = function
			h :: t -> let new_h, env = expr_to_sexpr !env_ref h
					  in(
						env_ref := env;
						new_h :: (assistant t))
		  | [] -> []
		in
			(assistant el), !env_ref

(*TODO: Includes files,  now it process a  NUll inlcudes*)
let process_includes reserved classess = classess







(*TODO:add more buit-in functions *)

let store_reserved_functions = 
	let i32_t = Datatype(Int_t) and 
	    void_t = Datatype(Void_t) and
	    str_t = Arraytype( Char_t, 1) in 
	let m t s = Formal(t, s) in
	let reserved_stub fname return_type formals = 
	      { sfname = FName (fname);
		sreturnType = return_type;
		sformals= formals;	
		func_type= Sast.Reserved;
		sbody=[];
		source= "NA"
		}
	in

	let reserved_functions =[

		reserved_stub "print" (void_t) ([Many(Any)])

		]

	in reserved_functions











(***********)
(*  TASK3 : Jiafei *)
(***********)
		
let get_constructor_name cname fdecl =
	let params = List.fold_left 
		(fun s -> 
		(function   Formal(t, _) -> s ^ "." ^ string_of_datatype t 
				  | _ -> "" )) 
		"" fdecl.formals 
	in
		let name = string_of_fname fdecl.fname
		in cname ^ "." ^ name ^ params

(*stringmap：
	                / field_map : field name --> Field(d, n)
				    | func_map : cname.constructor/cname.xxx/main --> func_decl
	class name --> <  constructor_map : class name.constructor.parameter types --> func_decl
				    | reserved_functions_map : function name --> func_decl
				    \ class_decl
*)
let build_class_maps reserved_functions cdecls =
	let reserved_functions_map = List.fold_left (fun mp sfun -> StringMap.add (string_of_fname sfun.sfname) sfun mp) StringMap.empty reserved_functions
	in
		let assistant mp cdecl =
			let fieldpart mp = function Field(d,n) ->
				if (StringMap.mem n mp)
					then raise (Failure ("DuplicateField: " ^ n))(*exception:DuplicateField *)
				else (StringMap.add n (Field(d, n)) mp)
			in
				let constructorpart condecl = 
					if List.length condecl > 1
						then raise (Failure ("DuplicateConstructor"))(*exception:DuplicateConstructor*)
					else if List.length condecl = 0 (*default constructor*)
						then StringMap.add (get_constructor_name cdecl.cname default_c) default_c StringMap.empty
					else StringMap.add (get_constructor_name cdecl.cname (List.hd condecl)) (List.hd condecl) StringMap.empty
				in
					let funcpart mp fdecl = 
						let funname = get_name cdecl.cname fdecl
						in
							if (StringMap.mem funname mp)
								then raise (Failure ("DuplicateFunction: " ^ funname))(*exception:DuplicateFunction*)
							else let strfunname = string_of_fname fdecl.fname
								 in
									if (StringMap.mem strfunname reserved_functions_map)
										then raise (Failure ("CannotUseReservedFuncName: " ^ strfunname))(*exception:CannotUseReservedFuncName*)
									else (StringMap.add (get_name cdecl.cname fdecl) fdecl mp)
					in
						(if (StringMap.mem cdecl.cname mp)
							then raise (Failure ("DuplicateClassName: " ^ cdecl.cname))(*exception:DuplicateClassName*)
						else
							StringMap.add cdecl.cname
							{
								field_map = List.fold_left fieldpart StringMap.empty cdecl.cbody.fields;
								constructor_map = constructorpart cdecl.cbody.constructors;
								func_map = List.fold_left funcpart StringMap.empty cdecl.cbody.methods;
								reserved_functions_map = reserved_functions_map; 
								cdecl = cdecl
							} mp)
		in List.fold_left assistant StringMap.empty cdecls


(* to-do handle_inheritance *)
let handle_inheritance cdecls class_maps = class_maps, cdecls
		

(***********)
(*  TASK4: Yanan *)
(***********)

(* convert statement is Ast to statement in Sast*)
let rec check_sblock sl env = match sl with
		[] -> SBlock([SExpr(SNoexpr, Datatype(Void_t))]), env
	| 	_  -> 
		let sl, _ = convert_stmt_list_to_sstmt_list env sl in
		SBlock(sl), env

and check_expr_stmt e env = 
	let se, env = expr_to_sexpr env e in
	let t = get_type_from_sexpr se in 
	SExpr(se, t), env

(* convert the constructor in ast to the function in Sast *)
and  parse_stmt env = function
		Block sl 		-> check_sblock sl env (*TODO*)
	| 	Expr e 			-> check_expr_stmt e env (*TODO*)

and  convert_stmt_list_to_sstmt_list env stmt_list =
	let env_ref = ref(env) in
	let rec iter = function
	  head::tail ->
		let a_head, env = parse_stmt !env_ref head in
		env_ref := env;
		a_head::(iter tail)
	| [] -> []
	in 
	let sstmt_list = (iter stmt_list), !env_ref in
	sstmt_list

let convert_constructor_to_sfdecl class_maps reserved class_map cname constructor = 
	let env = {
		env_class_maps 	= class_maps;
		env_name     	= cname;
		env_cmap 	= class_map;
		env_locals    	= StringMap.empty;
		env_parameters	= List.fold_left (fun m f -> match f with Formal(d, s) -> (StringMap.add s f m) | _ -> m) StringMap.empty constructor.formals;
		env_returnType	= Datatype(Objecttype(cname));
		env_in_for 	= false;
		env_in_while 	= false;
		env_reserved 	= reserved;
	} in 
	let fbody = fst (convert_stmt_list_to_sstmt_list env constructor.body) in
	{
		sfname 		= Ast.FName (get_constructor_name cname constructor);
		sreturnType 	= Datatype(Objecttype(cname));
		sformals 	= constructor.formals;
		sbody 		= []; (*TODO*)
		func_type	= Sast.User;
		source 		= "NA";
	}

let check_fbody fname fbody returnType =
	let len = List.length fbody in
	if len = 0 then () else 
	let final_stmt = List.hd (List.rev fbody) in
	match returnType, final_stmt with
		Datatype(Void_t), _ -> ()
	| 	_, SReturn(_, _) -> ()
	| 	_ -> raise(Failure ("AllNonVoidFunctionsMustEndWithReturn"))


let convert_fdecl_to_sfdecl class_maps reserved class_map cname fdecl=

	let root_cname = cname in
	let class_formal = Ast.Formal(Datatype(Objecttype(cname)), "this")

	in 
	let env_param_helper m fname = match fname with 
			Formal(d, s) -> (StringMap.add s fname m) 
		| 	_ -> m
	in
	let env_params = List.fold_left env_param_helper StringMap.empty (class_formal :: fdecl.formals) in
	let env = {
		env_class_maps 	= class_maps;
		env_name     	= cname;
		env_cmap 	= class_map;
		env_locals    	= StringMap.empty;
		env_parameters	= env_params;
		env_returnType	= fdecl.returnType;
		env_in_for 	= false;
		env_in_while 	= false;
		env_reserved 	= reserved;
	}
	    
	in
	let fbody = fst (convert_stmt_list_to_sstmt_list env fdecl.body) in
	let fname = (get_name cname fdecl) in
	(*ignore(check_fbody fname fbody fdecl.returnType);*)
	
	let fbody = if fname = "main" 
		then fbody (*TODO*) 
		else fbody 
	in
	(* We add the class as the first parameter to the function for codegen *)
	{
		sfname 		= Ast.FName (get_name cname fdecl);
		sreturnType 	= fdecl.returnType;
		sformals 	= class_formal :: fdecl.formals;
		sbody 		= fbody;
		func_type	= Sast.User;
		source 		= cname;
	}
	

(*convert the class in ast to sast type*)
let convert_cdecl_to_sast sfdecls (cdecl: Ast.class_decl) =
	{scname =cdecl.cname;
	 sfields =cdecl.cbody.fields;
	 sfuncs= sfdecls;
	}

(* Convert ast to sast*)
let convert_ast_to_sast class_maps reserved_functions cdecls = 


	let check_main = (fun f -> match f.sfname with FName s -> s="main") in

	let get_main_func fdecls = 
	    let main_funs = (List.filter check_main fdecls) in

	    if List.length main_funs> 1 then 
		raise (Failure("Multiple main functions are found"))
	    else if List.length main_funs <1 then
 		raise (Failure("Main function is not found!"))
	    else List.hd main_funs

	in let remove_main func_list=
		 List.filter (fun f -> not(check_main f)) func_list 
	
	in let check_default_constructor cdecl clist =
		let default_cname = cdecl.cname ^ "." ^ "Constructor" 			in let check_default_c f=  match f.sfname with FName n -> 				n=default_cname
		in try let _ = List.find check_default_c clist in clist
		   with | Not_found -> clist
		   
			    
	in 

	let handle_cdecl cdecl =
		let class_map =StringMap.find cdecl.cname class_maps in
		let sconstructor_list = List.fold_left (fun l c -> (convert_constructor_to_sfdecl class_maps reserved_functions class_map cdecl.cname c)::l) [] cdecl.cbody.constructors in

		let sconstructor_list = check_default_constructor cdecl sconstructor_list in 
 		let func_list = List.fold_left (fun l f -> (convert_fdecl_to_sfdecl class_maps reserved_functions class_map cdecl.cname f):: l) [] cdecl.cbody.methods in 
		let sfunc_list = remove_main  func_list in
		let scdecl = convert_cdecl_to_sast sfunc_list cdecl in 
		(scdecl, func_list @sconstructor_list)
	in 
	let iter_cdecls t c = 
		let scdecl =handle_cdecl c in 
		(fst scdecl::fst t, snd scdecl @ snd t)
	in
	let scdecl_list, func_list =List.fold_left  iter_cdecls ([],[]) cdecls 		in
	let main = get_main_func func_list in 
	let funcs=  remove_main func_list in

	{ classes = scdecl_list;
	  functions = funcs;
	  main = main;
    	  reserved = reserved_functions;
	}
 





(***********************************************************)
(* Entry point for translating Ast to Sast *)
(***********************************************************)

let check program = match program with 

	Program (includes, classes) ->
 	let cdecls = process_includes includes classes in 
	ignore (build_struct_indexes  cdecls);


	(* add reserved built-in functions*)
	let reserved_functions = store_reserved_functions in 

	let class_maps = build_class_maps reserved_functions cdecls in 

        let class_maps, cdecls = handle_inheritance cdecls class_maps in 


	let sast = convert_ast_to_sast class_maps reserved_functions cdecls in 

	sast 




	

  
