(* Semantic checking for the Liva compiler *)

open Ast
open Sast


(* Semantic checking of a program. Returns Sast if successful,
   throws an exception if something is wrong. *)

(*global variables and helpfer functions*)
module StringMap = Map.Make(String)
module StringSet = Set.Make (String)

module SS = Set.Make(
	struct
		let compare = Pervasives.compare
		type t = datatype
	end )

type classMap = {
	fieldMap               : Ast.field StringMap.t;
	functionMap               : Ast.func_decl StringMap.t;
	constructorMap         : Ast.func_decl StringMap.t;
	_functions_map 	: sfunc_decl StringMap.t;
	cdecl 			        : Ast.class_decl;
}

type env ={

	envClassMaps: classMap StringMap.t;
	envName: string;
	envClassMap: classMap;
	envLocals: datatype StringMap.t;
	envParams: Ast.formal StringMap.t;
	envReturnType:datatype;
	envInFor: bool;
	envInWhile: bool;
	envBuiltIn:sfunc_decl list;
}

let updateEnv env envName = 
{
	envClassMaps = env.envClassMaps;
	envName       = envName;
	envClassMap 	   = env.envClassMap;
	envLocals     = env.envLocals;
	envParams = env.envParams;
	envReturnType = env.envReturnType;
	envInFor     = env.envInFor;
	envInWhile   = env.envInWhile;
	envBuiltIn   = env.envBuiltIn;
}

let struct_indexes: (string, int) Hashtbl.t =  Hashtbl.create 10

let predecessors:(string, string list) Hashtbl.t = Hashtbl.create 10

let createStructIndexes cdecls= 
	let cdecls_handler index cdecl=
	Hashtbl.add struct_indexes cdecl.cname index in 
	List.iteri cdecls_handler cdecls

let defaultC = 
{
	fname      = Ast.Constructor;
	returnType = Datatype(ConstructorType);
	formals    = [];
	body       = [];
	overrides 		= false;
	root_cname 		= None;
}

let getName cname fdecl = (*get the name of function,cname.constructor-> constructor / cname.xxx-> normal_function / main*)
	let name = string_of_fname fdecl.fname (*tell constructor from normal function*)
	in
	match name with
	 "main" -> "main"
	| _ 	-> cname ^ "." ^ name

		
let get_type_from_sexpr = function(*get the type of sexpression*)
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
	| 	SCall(_, _, d,_)		-> d
	|   SObjectCreate(_, _, d) 	-> d
	| 	SArrayElements(_, d)	-> d
	|  	SUnop(_, _, d) 			-> d
	| 	SNull					-> Datatype(Null_t) 
											
(***********************************************************)
(* Entry point for translating Ast to Sast *)
(***********************************************************)

let check program = 

	(* add reserved built-in functions*)	
	let storeBuiltinFunctions = 
		let i32_t = Datatype(Int_t) and 
			void_t = Datatype(Void_t) and
			str_t = Arraytype( Char_t, 1) 
		in 
		let mf t s = Formal(t, s) 
		in
		let builtinStub fname returnType formals = 
			{ 
				sfname = FName (fname);
				sreturnType = returnType;
				sformals= formals;	
				func_type= Sast.Reserved;
				sbody=[];
				overrides 		= false;
				source= "NA"
			}
		in
		let builtinFunctions =[
		
			builtinStub "print" 	(void_t) 	([Many(Any)]);
			builtinStub "malloc" 	(str_t) 	([mf i32_t "size"]);
			builtinStub "cast" 	(Any) 		([mf Any "in"]);
			
			]
		in builtinFunctions	
	in
	
	let builtinFunctions = storeBuiltinFunctions 
	in
	
	(* create class maps*)
	let getConstructorName cname fdecl =
		let params = List.fold_left 
			(fun s f -> match f with
							   Formal(t, _) -> s ^ "." ^ string_of_datatype t
							|  _ -> "" ) "" fdecl.formals 
		in
			let name = string_of_fname fdecl.fname
			in cname ^ "." ^ name ^ params
	in
	
	let mappingClass builtinFunctions cdecls =
		let builtFuncMap = 
			List.fold_left (fun mp sfunc -> StringMap.add (string_of_fname sfunc.sfname) sfunc mp) StringMap.empty builtinFunctions
		in
		
		let assistant mp cdecl =
			let fieldpart mp = function Field(d,n) ->
				if (StringMap.mem n mp)
					then raise (Failure ("Duplicated Field: " ^ n))(*exception:DuplicateField *)
				else (StringMap.add n (Field(d, n)) mp)
			in
			
			let constructorpart condecl = 
				if List.length condecl > 1
					then raise (Failure ("Duplicated Constructor"))(*exception:DuplicateConstructor*)
				else if List.length condecl = 0 (*default constructor*)
					then StringMap.add (getConstructorName cdecl.cname defaultC) defaultC StringMap.empty
				else
					StringMap.add (getConstructorName cdecl.cname (List.hd condecl)) (List.hd condecl) StringMap.empty

			in
						
			let funcpart m fdecl = 
				let funname = getName cdecl.cname fdecl
				in

				if (StringMap.mem funname mp)
					then raise (Failure ("Duplicated Function: " ^ funname))(*exception:DuplicateFunction*)
				else 
					let strfunname = string_of_fname fdecl.fname
					in
					
					if (StringMap.mem strfunname builtFuncMap)
						then raise (Failure ("Cannot use the reserved buit-in function name: " ^ strfunname))(*exception:CannotUseReservedFuncName*)
					else (StringMap.add (getName cdecl.cname fdecl) fdecl m)
			in

			(if (StringMap.mem cdecl.cname mp)
				then raise (Failure ("Duplicated class name: " ^ cdecl.cname))(*exception:DuplicateClassName*)
			else
				StringMap.add cdecl.cname
				{
					fieldMap = List.fold_left fieldpart StringMap.empty cdecl.cbody.fields;
					constructorMap = constructorpart cdecl.cbody.constructors;
					functionMap = List.fold_left funcpart StringMap.empty cdecl.cbody.methods;
					builtFuncMap = builtFuncMap; 
					cdecl = cdecl
				} mp)
		in List.fold_left assistant StringMap.empty cdecls
	
	in
	
	match program with 
	Program (classes) -> ignore (createStructIndexes classes);
	
	
	let classMaps = mappingClass builtinFunctions classes
	in 
	
	(* convert statement in Ast to statement in Sast*)
	let rec exprToSexpr env = function
		Int_Lit i           -> SInt_Lit(i), env
	|   Boolean_Lit b       -> SBoolean_Lit(b), env
	|   Float_Lit f         -> SFloat_Lit(f), env
	|   String_Lit s        -> SString_Lit(s), env
	|   Char_Lit c          -> SChar_Lit(c), env
	|   This                -> SId("this", Datatype(Objecttype(env.envName))), env
	|   Id s                -> SId(s, getIDType env s), env
	|   Null                -> SNull, env
	|   Noexpr              -> SNoexpr, env
	|   ObjAccess(e1, e2)   -> checkObjAccess env e1 e2, env
	|   ObjectCreate(s, el) -> checkConstructor env s el, env
	|   Call(s, el)         -> checkCallType env s el, env
	|   ArrayCreate(d, el)  -> checkArrayInitialize env d el, env
	|   ArrayAccess(e, el)  -> checkArrayAccess env e el, env
	|   Assign(e1, e2)      -> checkAssign env e1 e2, env
	|   Unop(op, e)         -> checkUnop env op e, env
	|   Binop(e1, op, e2)   -> checkBinop env e1 op e2, env

	and exprsToSexprs env el = (*convert expression list to sexpression list*)
		let env_ref = ref env
		in
			let rec assistant = function
				h :: t -> let new_h, env = exprToSexpr !env_ref h
						  in(
							env_ref := env;
							new_h :: (assistant t))
			  | [] -> []
			in
				(assistant el), !env_ref 

	and getIDType env s = 
		try
			StringMap.find s env.envLocals
		with
			| Not_found -> try let formal = StringMap.find s env.envParams
							   in (function   Formal(t, _) -> t 
											| Many t -> t) formal
						   with  | Not_found -> raise (Failure ("ID is undefined: " ^ s))

	and checkArrayInitialize env d el = 
		let arraySize = List.length el(*get the dimention of array*)
		in
		let checkIndexType e = (*check whether the type of index is int*)
			let sexpr, _ = exprToSexpr env e (*convert expression to sexpression*)
			in
				let sexpr_type = get_type_from_sexpr sexpr (*get the type of sexpression*)
				in
					if sexpr_type = Datatype(Int_t) 
						then sexpr
					else raise (Failure ("Invalid index type for array initialization: " ^ string_of_datatype sexpr_type))
		in
			let check_dtyp = function(*check whether the type can be array type*)
				   Datatype(x) -> Arraytype(x, arraySize)
				|  _ as t      -> raise (Failure ("Invalid array type: " ^ (string_of_datatype t)))
			in
				let sexpr_type = check_dtyp d
				in
					let sel = List.map checkIndexType  el
					in
						SArrayCreate(d, sel, sexpr_type)

	and checkArrayAccess env e el = 
		let arraySize = List.length el (*get the size of array*)
		in
			let checkIndexType arg = (*check whether the type of index is int*)
				let sexpr, _ = exprToSexpr env arg (*convert expression to sexpression*)
				in
					let sexpr_type = get_type_from_sexpr sexpr (*get the type of sexpression*)
					in
						if sexpr_type = Datatype(Int_t) 
							then sexpr
						else raise (Failure ("Invalid index type for array access: " ^ string_of_datatype sexpr_type))
			in
				let se, _ = exprToSexpr env e (*convert expression to sexpression*)
				in
					let se_type = get_type_from_sexpr se (*get the type of sexpression*)
					in
						let checkArraySize num_params = function
							   Arraytype(t, n) -> if num_params < n
													then Arraytype(t, (n-num_params))(*remain, now a smaller array*)
												  else if num_params = n
													then Datatype(t)
												  else raise (Failure ("Invalid demention for array access: " ^ (string_of_int num_params) ^ " > " ^ (string_of_int n)))
							|  _ as t          -> raise (Failure ("Invalid type for array access: " ^ (string_of_datatype t)))	
						in
							let sexpr_type = checkArraySize arraySize se_type
							in
								let sel = List.map checkIndexType el
								in SArrayAccess(se, sel, sexpr_type)

	and checkObjAccess env lhs rhs =

		let check_lhs = function(*check the expression before ‘.’ and get sexpression*)
			   This 	-> SId("this", Datatype(Objecttype(env.envName)))
			|  Id s 	-> SId(s, getIDType env s)
			|  ArrayAccess(e, el)	-> checkArrayAccess env e el
			|  _ as e 	-> raise (Failure ("LHS of object access must be an instance of certain class"))
		in

		let get_cname lhs_datatyp = match lhs_datatyp with (*get the type of the expression before ‘.’, i.e. class name*)
				Datatype(Objecttype(name)) 	-> name
			| 	_ as d						-> raise (Failure ("Object access must have ObjectType: " ^ string_of_datatype d))
		in
		let rec check_rhs (env) lhs_datatyp=
			let class_name = get_cname lhs_datatyp (*get the class name*)
			in
				let search_classfield env (id) cname=
				let cmap = StringMap.find cname env.envClassMaps (*get the class map of current class*)
				in
					let match_field  = function Field(d, _) -> d (*get datatype of the expression after ‘.’*)
					in
						try match_field (StringMap.find id cmap.fieldMap)
						with | Not_found -> raise (Failure ("Unknown field identifier for class: " ^ id ^ " -> " ^ cname))
				in
					function
					   Id s 		   -> SId(s, (search_classfield env s class_name )), env (* Check fields*)
					|  Call(fname, el) -> let env = updateEnv env class_name (* Check functions*)
										  in checkCallType env fname el, env
					|  _ as e		   -> raise (Failure ("Invalid object access: " ^ string_of_expr e))
		in

		let s_lhs = check_lhs lhs in
			
		let s_lhs_type = get_type_from_sexpr s_lhs in 
		
		let l_cname = get_cname s_lhs_type in
				
		let lhs_env = updateEnv env l_cname in
		
		let s_rhs, _ = check_rhs lhs_env s_lhs_type (*env*) rhs in
		
		let s_rhs_type = get_type_from_sexpr s_rhs in 

		SObjAccess(s_lhs, s_rhs, s_rhs_type)

	and checkCallType env fname el =
		let sel, env = exprsToSexprs env el(*convert expression list to sexpression list*)
		in
			let cmap = try StringMap.find env.envName env.envClassMaps (*check whether the class has been defined*)
					   with | Not_found -> raise (Failure ("Undefined class: " ^ env.envName))
			in(*check type*)
			
			let check_pa_onebyone formal param = (*check parameter according to type*)
				let ftyp = match formal with
					Formal(d, _) -> d
					| _            -> Datatype(Void_t)
				in
				
				let ptyp = get_type_from_sexpr param(*get the type of actual parameter*)
				in
					if ftyp = ptyp
						then param
					else raise (Failure ("Incompatible type for function: " ^ fname ^ "  " ^ string_of_datatype ptyp ^ " -> " ^ string_of_datatype ftyp))
			in


			let getIndex func funcName =
				let cdecl = cmap.cdecl in

				let fns = List.rev cdecl.cbody.methods in
					let rec find x lst =
						match lst with
						| [] -> raise (Failure ("Could not find " ^ fname))
						| fdecl :: t -> 
							let search_name = (getName env.envName func) in
							if x = search_name then 0 
							else if search_name = "main" then find x t 
							else 1 + find x t
					in
					find funcName fns
			in

			let check_params formals params = match formals, params with (*check parameter according to amount*)
				[Many(Any)], _ 		-> params
				| [], []         	-> []
				| _              	-> if List.length formals <> List.length params
										then raise (Failure ("Incorrect argument number for function: " ^ fname))
										else List.map2 check_pa_onebyone formals sel
			in
			try
				let func = StringMap.find fname cmap.builtFuncMap
				in
				let actuals = check_params func.sformals sel
				in SCall(fname, actuals, func.sreturnType,0)
			with | Not_found -> let sfname = env.envName ^ "." ^ fname
	in
		try let f = StringMap.find sfname cmap.functionMap
			in
			let actuals = check_params f.formals sel in
			let index = getIndex f sfname in
			SCall(sfname, actuals, f.returnType, index)
		with | Not_found -> raise (Failure ("Function is not found: " ^ sfname))

	and checkConstructor env s el =
		let sel, env = exprsToSexprs env el
		in
		
		let cmap = try StringMap.find s env.envClassMaps(*find the class*)
					with | Not_found -> raise (Failure ("Undefined class: " ^ s))
		in
		
		let params = List.fold_left 
			(fun s e -> s ^ "." ^ (string_of_datatype (get_type_from_sexpr e))) "" sel
		in
					
		let constructor_name = s ^ "." ^ "constructor" ^ params
		in
						
		let _ = 
			try StringMap.find constructor_name cmap.constructorMap(*find constructor with type check*)
			with  | Not_found -> raise (Failure ("Constructor is not found: " ^ constructor_name))
						(*let _ = raise(Failure("" ^ constructor_name))*)
		in
		
		let objectTyp = Datatype(Objecttype(s)) in

		SObjectCreate(constructor_name, sel, objectTyp)

	and checkAssign env e1 e2 =
		let se1, env = exprToSexpr env e1(*convert expression to sexpression*)
		in
		let se2, env = exprToSexpr env e2
		in
		let type1 = get_type_from_sexpr se1(*get the type of sexpression*)
		in
		let type2 = get_type_from_sexpr se2
		in 
		
		match (type1, se2) with
			 Datatype(Objecttype(_)), SNull -> SAssign(se1, se2, type1)
			| _  -> 
				match type1, type2 with
					 Datatype(Objecttype(d)), Datatype(Objecttype(t)) ->
							if d = t 
								then SAssign(se1, se2, type1)
							else raise (Failure ("Assignment types are mismatched: " ^ string_of_datatype type1 ^ " <-> " ^ string_of_datatype type2))
					|  _ -> if type1 = type2 
							then SAssign(se1, se2, type1)
							else raise (Failure ("Assignment types are mismatched: " ^ string_of_datatype type1 ^ " <-> " ^ string_of_datatype type2))

	and checkUnop env op e = 
		let check_num_unop t = function(*operator for number*)
			   Sub     -> t
			|  _ as o  -> raise (Failure ("Invalid unary operation: " ^ string_of_op o))
		in 
		let check_bool_unop = function(*operator for bool*)
			   Not    -> Datatype(Bool_t)
			|  _ as o -> raise (Failure ("Invalid unary operation: " ^ string_of_op o))
		in
		let se, env = exprToSexpr env e (*convert expression to sexpression*)
		in
		let st = get_type_from_sexpr se (*get the type of sexpression*)
		in
			match st with (*check the type of operand*)
				   Datatype(Int_t) 	
				|  Datatype(Float_t) -> SUnop(op, se, check_num_unop st op)
				|  Datatype(Bool_t)  -> SUnop(op, se, check_bool_unop op)
				|  _ as o            -> raise (Failure ("Invalid operant type for unary operation: " ^ string_of_datatype o))

	and checkBinop env e1 op e2 =	
		let get_sbinop_equal type1 type2 se1 se2 op =
			if (type1 = Datatype(Float_t) || type2 = Datatype(Float_t)) (*unqualified types*)
				then raise (Failure ("Equality operation is not supported for Float types"))
			else
				match type1, type2 with (*qualified types*)
					  Datatype(Objecttype(_)), Datatype(Null_t)
					| Datatype(Null_t), Datatype(Objecttype(_))	 -> SBinop(se1, op, se2, Datatype(Bool_t))
					| _                                          -> if type1 = type2
																		then SBinop(se1, op, se2, Datatype(Bool_t))
																	else raise (Failure ("Invalid equality operator for these types: " ^ (string_of_datatype type1) ^ " <-> " ^ (string_of_datatype type2)))
		in
		
		let get_sbinop_logic type1 type2 se1 se2 op =(*check operants and conver to sbinop*)
			match type1, type2 with
				   Datatype(Bool_t), Datatype(Bool_t) -> SBinop(se1, op, se2, Datatype(Bool_t))
				|  _                                  -> raise (Failure ("Invalid type for logical operator" ^ (string_of_datatype type1) ^ "<->" ^ (string_of_datatype type2)))
		in
		
		let get_sbinop_compa type1 type2 se1 se2 op =
			match type1, type2 with
				   Datatype(Int_t), Datatype(Float_t)
				|  Datatype(Float_t), Datatype(Int_t) -> SBinop(se1, op, se2, Datatype(Bool_t))
				|  _								  -> if type1 = type2
															then SBinop(se1, op, se2, Datatype(Bool_t))
														 else raise (Failure ("Invalid type for comparison operator: " ^ (string_of_datatype type1) ^ "<->" ^ (string_of_datatype type2)))
		in
		
		let get_sbinop_arith type1 type2 se1 se2 op =
			match type1, type2 with (*qualified combination of operant type*)
				   Datatype(Int_t), Datatype(Float_t)
				|  Datatype(Float_t), Datatype(Int_t) 
				|  Datatype(Float_t), Datatype(Float_t) -> SBinop(se1, op, se2, Datatype(Float_t))
				|  Datatype(Int_t), Datatype(Int_t) 	-> SBinop(se1, op, se2, Datatype(Int_t))
				|  _ -> raise (Failure ("Invalid type for arithmetic operator: " ^ (string_of_datatype type1) ^ "<->" ^ (string_of_datatype type2)))
		in
			
		let se1, env = exprToSexpr env e1 (*convert expression to sexpression*)
		in
		let se2, env = exprToSexpr env e2 (*convert expression to sexpression*)
		in
		let type1 = get_type_from_sexpr se1(*get the type of sexpression*)
		in
		let type2 = get_type_from_sexpr se2(*get the type of sexpression*)
		in
		
			match op with(*check and convert binopexpression according to binopexpression type*)
					Equal | Neq                  -> get_sbinop_equal type1 type2 se1 se2 op
				|	And | Or                     -> get_sbinop_logic type1 type2 se1 se2 op 
				|	Less | Leq | Greater | Geq   -> get_sbinop_compa type1 type2 se1 se2 op
				|	Add | Mult | Sub | Div | Mod -> get_sbinop_arith type1 type2 se1 se2 op 
				| 	_                            -> raise (Failure ("Invalid binop operator: " ^ (string_of_op op)))
				
	in
	
	let rec convertStmtsToSstmts env stmts =
		let env_ref = ref(env) in
			let rec iter = function
				  h::t -> let newh, newenv = checkStmt !env_ref h in
									(env_ref := newenv; newh::(iter t))
				| [] -> []
			in 
			let sstmts = (iter stmts), !env_ref in
			sstmts 
			

	and checkExprStmt e env = 
		let se, env = exprToSexpr env e
		in
			let typ = get_type_from_sexpr se
			in 
			SExpr(se, typ), env
				

	and checkIfStmt e s1 s2 env = 
		let se, _ = exprToSexpr env e and	
			ifbody, _ = checkStmt env s1 and
			elsebody, _ = checkStmt env s2 
		in
		let typ = get_type_from_sexpr se 
		in
		match typ with
				Datatype(Bool_t) 	-> SIf(se, ifbody, elsebody), env
			|   _ 					-> raise (Failure("invalid if type"))

	and checkForStmt e1 e2 e3 s env = 

		let se1, _ = exprToSexpr env e1 and
			se2, _ = exprToSexpr env e2 and
			se3, _ = exprToSexpr env e3 and
			forBodyStmt, env = checkStmt env s 
		in
		let cond = get_type_from_sexpr se2 
		in 
		match cond with 
				Datatype(Bool_t) 	-> SFor(se1, se2, se3, forBodyStmt), env
			|	Datatype(Void_t)	-> SFor(se1, se2, se3, forBodyStmt), env
			|	_ 					-> raise (Failure("Invalid for statement type"))


	and checkWhileStmt e s env =
		let se, _ = exprToSexpr env e and 
			sstmt, _ = checkStmt env s
		in	    
		let typ = get_type_from_sexpr se 
		in 
		match typ with 
				Datatype(Bool_t) 	-> SWhile(se, sstmt), env
			| 	Datatype(Void_t) 	-> SWhile(se, sstmt), env
			| 	_ 					-> raise (Failure("Invalid while Statement Type"))

	and checkSblock sl env = match sl with
		  [] -> SBlock([SExpr(SNoexpr, Datatype(Void_t))]), env
		|  _ -> let sl, _ = convertStmtsToSstmts env sl
				in SBlock(sl), env

	and checkReturn e env = 
		let se, env = exprToSexpr env e in
		let	typ = get_type_from_sexpr se 
		in
		match typ, env.envReturnType with
			   Datatype(Null_t), Datatype(Objecttype(_)) -> SReturn(se, typ), env
			|  _										 -> 
				if typ = env.envReturnType 
					then SReturn(se, typ), env
					else raise (Failure ("Return type is mismatched!"))
					
	and checkLocal d s e env = 
		if StringMap.mem s env.envLocals 
			then raise (Failure ("Duplicate Local variable defined: " ^ s))
		else
			let se, env = exprToSexpr env e 
			in
			let typ = get_type_from_sexpr se 
			in
			let update_env = {
					envClassMaps = env.envClassMaps;
					envName = env.envName;
					envClassMap = env.envClassMap;
					envLocals = StringMap.add s d env.envLocals; (* add new locals *)
					envParams = env.envParams;
					envReturnType = env.envReturnType;
					envInFor = env.envInFor;
					envInWhile = env.envInWhile;
					envBuiltIn = env.envBuiltIn;
				} 
			in
			if typ = Datatype(Void_t) || typ = Datatype(Null_t) || typ = d (* || (inherited d t) *)
				then
					match d with
						  Datatype(Objecttype(x)) ->
							if not (StringMap.mem (Ast.string_of_object d) env.envClassMaps) 
								then raise (Failure ("Undefined Class: " ^ string_of_object d )) 
							else
								let local = SLocal(d, s, se) (*To Do about inherited *)
								in local, update_env
						|  _ -> SLocal(d, s, se), update_env

			else 
				raise (Failure("Local assignment type mismatch: " ^ Ast.string_of_datatype d ^ " <-> " ^ Ast.string_of_datatype typ))	
				

	and  checkStmt env = function

			Expr e 				-> checkExprStmt e env
		| 	If(e, s1, s2) 		-> checkIfStmt e s1 s2	env
		|   While( e, s) 		-> checkWhileStmt e s env
		| 	For(e1, e2, e3, e4) -> checkForStmt e1 e2 e3 e4 env
		|	Block sl 			-> checkSblock sl env
		| 	Return e 			-> checkReturn e env
		|   Local(d, s, e) 		-> checkLocal d s e env
		
	in
	
	(* handle class inheritance*) (*TODO*)
	let rec handle_inheritance cdecls class_maps =
		let predecessors = build_inheritance_forest cdecls class_maps in
		let cdecls_inherited = inherit_fields_cdecls cdecls predecessors in
		let functionMaps_inherited = build_functionMap_inherited_lookup cdecls_inherited in
		(*to do*)
		let cmaps_with_inherited_fields = inherit_fields class_maps predecessors in
		let cmaps_inherited = add_inherited_methods cmaps_with_inherited_fields cdecls_inherited functionMaps_inherited in
		cmaps_inherited, cdecls_inherited

	and build_inheritance_forest cdecls cmap = 
		let handler a cdecl =
			match cdecl.extends with 
				Parent(s) 	-> 
					let new_list = if (StringMap.mem s a) then
						cdecl.cname::(StringMap.find s a)
					else
						[cdecl.cname]
					in
					Hashtbl.add predecessors s new_list; 
					(StringMap.add s new_list a) 
			| 	NoParent 	-> a
		in
		let forest = List.fold_left handler StringMap.empty cdecls in

		let handler key value = 
			if not (StringMap.mem key cmap) then 
				raise (Failure("undefined class"))
		in
		ignore(StringMap.iter handler forest);
		forest

	and inherit_fields_cdecls cdecls inheritance_forest = 
		(* iterate through cdecls to make a map for lookup *)
		let cdecl_lookup = List.fold_left (fun a litem -> StringMap.add litem.cname litem a) StringMap.empty cdecls in
		let add_key key pred maps = 
			let elem1 = StringSet.add key (fst maps) in
			let accum acc child = StringSet.add child acc in
			let elem2 = List.fold_left (accum) (snd maps) pred in
			(elem1, elem2)
		in
		let empty_s = StringSet.empty in
		let res = StringMap.fold add_key inheritance_forest (empty_s, empty_s) in
		let roots = StringSet.diff (fst res) (snd res) in
		let rec add_inherited_fields predec desc map_to_update = 
			let merge_fields accum descendant = 
				let updated_predec_cdecl = StringMap.find predec accum in 
				let descendant_cdecl_to_update = StringMap.find descendant cdecl_lookup in
				let merged = merge_cdecls updated_predec_cdecl descendant_cdecl_to_update in 
				let updated = (StringMap.add descendant merged accum) in 
				if (StringMap.mem descendant inheritance_forest) then 
					let descendants_of_descendant = StringMap.find descendant inheritance_forest in
					add_inherited_fields descendant descendants_of_descendant updated
				else updated
			in
			List.fold_left merge_fields map_to_update desc
		in
		(* map class name of every class_decl in `cdecls` to its inherited cdecl *)
		let inherited_cdecls = 
			let traverse_tree tree_root accum = 
				let tree_root_descendant = StringMap.find tree_root inheritance_forest in 
				let accum_with_tree_root_mapping = StringMap.add tree_root (StringMap.find tree_root cdecl_lookup) accum in
				add_inherited_fields tree_root tree_root_descendant accum_with_tree_root_mapping
			in
			StringSet.fold traverse_tree roots StringMap.empty 
		in
		(* build a list of updated cdecls corresponding to the sequence of cdecls in `cdecls` *)
		let add_inherited_cdecl cdecl accum = 
			let inherited_cdecl = 
				try StringMap.find cdecl.cname inherited_cdecls 
				with | Not_found -> cdecl
			in
			inherited_cdecl::accum
		in
		let result = List.fold_right add_inherited_cdecl cdecls [] in
		result


	and merge_cdecls base_cdecl child_cdecl = 
	(* return a cdecl in which cdecl.cbody.fields contains the fields of 
	the extended class, concatenated by the fields of the child class *)
		let child_cbody = 
			{
				fields = base_cdecl.cbody.fields @ child_cdecl.cbody.fields;
				 constructors = child_cdecl.cbody.constructors;
				 methods = merge_methods base_cdecl.cname base_cdecl.cbody.methods child_cdecl.cbody.methods
			}
			in
			{
				cname = child_cdecl.cname;
				extends = child_cdecl.extends;
				cbody = child_cbody
			}

	and merge_methods base_cname base_methods child_methods =
		let check_overrides child_fdecl accum = 
			let base_checked_for_overrides = 
				replace_fdecl_in_base_methods base_cname (fst accum) child_fdecl 
			in
			if (fst accum) = base_checked_for_overrides
				then ((fst accum), child_fdecl::(snd accum)) 
				else (base_checked_for_overrides, (snd accum))
		in
		let updated_base_and_child_fdecls = 
			List.fold_right check_overrides child_methods (base_methods, [])
		in
		(fst updated_base_and_child_fdecls) @ (snd updated_base_and_child_fdecls)

	and replace_fdecl_in_base_methods base_cname base_methods child_fdecl = 
		let replace base_fdecl accum = 
			let get_root_cname = function
				None -> Some(base_cname)
				| Some(x) -> Some(x)
			in
			let modify_child_fdecl = 
				{
					fname = child_fdecl.fname;
					returnType = child_fdecl.returnType;
					formals = child_fdecl.formals;
					body = child_fdecl.body;
					overrides = true;
					root_cname = get_root_cname base_fdecl.root_cname;
				} 
			in
			if (getName_without_class base_fdecl) = (getName_without_class child_fdecl) 
				then modify_child_fdecl::accum 
				else base_fdecl::accum
		in
		List.fold_right replace base_methods []

	and getName_without_class fdecl = 

		let params = List.fold_left (fun s -> (function Formal(t, _) -> s ^ "." ^ Ast.string_of_datatype t | _ -> "" )) "" fdecl.formals in
		let name = Ast.string_of_fname fdecl.fname in
		let ret_type = Ast.string_of_datatype fdecl.returnType in
		ret_type ^ "." ^ name ^ "." ^ params



	and build_functionMap_inherited_lookup cdecls_inherited = 
		let build_functionMap cdecl =
			let add_func m fdecl = StringMap.add (getName cdecl.cname fdecl) fdecl m in
			List.fold_left add_func StringMap.empty cdecl.cbody.methods
		in
		let add_class_functionMap m cdecl = StringMap.add cdecl.cname (build_functionMap cdecl) m in
		List.fold_left add_class_functionMap StringMap.empty cdecls_inherited

	and inherit_fields class_maps predecessors =
		(* Get basic inheritance map *)
		let add_key key pred map = StringMap.add key pred map in
		let cmaps_inherit = StringMap.fold add_key class_maps StringMap.empty in
		(* Perform accumulation of child classes *)
		let add_key key pred maps = 
			let elem1 = StringSet.add key (fst maps) in
			let accum acc child = StringSet.add child acc in
			let elem2 = List.fold_left (accum) (snd maps) pred in
			(elem1, elem2)
		in
		let empty_s = StringSet.empty in
		let res = StringMap.fold add_key predecessors (empty_s, empty_s) in
		let roots = StringSet.diff (fst res) (snd res) in
		(*in let _ = print_set_members roots*)
		let rec add_inherited_fields predec desc cmap_to_update = 
			let cmap_inherit accum descendant = 
				let predec_fieldMap = (StringMap.find predec accum).fieldMap in
				let desc_fieldMap = (StringMap.find descendant accum).fieldMap in 
				let merged = merge_maps predec_fieldMap desc_fieldMap in 
				let updated = update_class_maps "fieldMap" merged descendant accum in
				if (StringMap.mem descendant predecessors) then 
					let descendants_of_descendant = StringMap.find descendant predecessors in
					add_inherited_fields descendant descendants_of_descendant updated 
				else updated
			in
			List.fold_left cmap_inherit cmap_to_update desc
			(* end of add_inherited_fields *)
		in 
		let result = StringSet.fold (fun x a -> add_inherited_fields x (StringMap.find x predecessors) a) roots cmaps_inherit
		(*in let _ = print_map result*)
		in result

	and merge_maps m1 m2 = 
		StringMap.fold (fun k v a -> StringMap.add k v a) m1 m2

	and update_class_maps map_type cmap_val cname cmap_to_update = 
		let update m map_type = 
			if map_type = "fieldMap" then
				{
					fieldMap = cmap_val;
					functionMap = m.functionMap;
					constructorMap = m.constructorMap;
					builtFuncMap = m.builtFuncMap;
					cdecl = m.cdecl;
				}
			else m
		in
		let updated = StringMap.find cname cmap_to_update in
		let updated = update updated map_type in
		let updated = StringMap.add cname updated cmap_to_update in
		updated

	and add_inherited_methods cmaps cdecls functionMaps_inherited = 
		let find_cdecl cname = 
			try List.find (fun cdecl -> cdecl.cname = cname) cdecls
			with | Not_found -> raise Not_found
		in
		let update_with_inherited_methods cname cmap = 
			let fmap = StringMap.find cname functionMaps_inherited in
			let cdecl = find_cdecl cname in
			{
				fieldMap = cmap.fieldMap;
				functionMap = fmap;
				constructorMap = cmap.constructorMap;
				builtFuncMap = cmap.builtFuncMap;
				cdecl = cdecl;
			}
		in
		let add_updated_cmap cname cmap accum = StringMap.add cname (update_with_inherited_methods cname cmap) accum in
		StringMap.fold add_updated_cmap cmaps StringMap.empty
		
			
	
	in
	
    let classMaps, cdecls = handle_inheritance classes classMaps 
	in
	
	let appendConstructor fbody cname returnType =
		let key = Hashtbl.find struct_indexes cname 
		in 
		let thisInit = [SLocal(
					returnType,
					"this",
					SCall(	"cast", 
							[SCall("malloc", 
								[	
									SCall("sizeof", [SId("ignore", returnType)], Datatype(Int_t),0)
								], 
								Arraytype(Char_t, 1),0)
							],
							returnType,
							0
						)
					);
					SExpr(
						SAssign(
							SObjAccess(
								SId("this", returnType),
								SId(".key", Datatype(Int_t)),
								Datatype(Int_t)
							),
							SInt_Lit(key),
							Datatype(Int_t)
						),
						Datatype(Int_t)
					)
				]
		in
		let returnThis = 
					[
						SReturn(
							SId("this", returnType),
							returnType
						)
					]
		in
				(* Need to check for duplicate default constructs *)
				(* Also need to add malloc around other constructors *)
				thisInit @ fbody @ returnThis

	in
	
	(* convert Ast functions to Sast functions*)
	let convertFuncToSfunc classMaps reserved classMap cname func=
	
		let appendMain fbodyStmt cname returnType = 
			let key = Hashtbl.find struct_indexes cname in 
			let thisInit = [SLocal(
				returnType,
				"this",
				SCall(	"cast", 
						[SCall("malloc", 
							[	
								SCall("sizeof", [SId("ignore", returnType)], Datatype(Int_t), 0)
							], 
							Arraytype(Char_t, 1), 0)
						],
						returnType, 0
					)
				);
				SExpr(
					SAssign(
						SObjAccess(
							SId("this", returnType),
							SId(".key", Datatype(Int_t)),
							Datatype(Int_t)
						),
						SInt_Lit(key),
						Datatype(Int_t)
					),
					Datatype(Int_t)
				)
			]
			in 
			thisInit  @ fbodyStmt		
		in
		
		let rootClassName = match func.root_cname with 
			Some(x) -> x
			| None -> cname
		in

		let classFormal = 
			if func.overrides then 
				Ast.Formal(Datatype(Objecttype(rootClassName)), "this")
			else 
				Ast.Formal(Datatype(Objecttype(cname)), "this")
		in

		let env_param_helper m fname = match fname with 
				Formal(d, s) -> (StringMap.add s fname m) 
			| 	_ -> m
		in
		let env_params = List.fold_left env_param_helper StringMap.empty (classFormal :: func.formals) in
		let env = {
			envClassMaps 	= classMaps;
			envName     	= cname;
			envClassMap 		= classMap;
			envLocals    	= StringMap.empty;
			envParams	= env_params;
			envReturnType	= func.returnType;
			envInFor 		= false;
			envInWhile 	= false;
			envBuiltIn 	= reserved;
		}
			
		in
		let fbody = fst (convertStmtsToSstmts env func.body) in
		let funcName = (getName cname func) in
		(*ignore(check_fbody fname fbody fdecl.returnType);*)
		
		let fbody = if funcName= "main" 
			then (appendMain fbody cname (Datatype(Objecttype(cname)))) 
			else fbody 
		in
		{
			sfname 		= Ast.FName (getName cname func);
			sreturnType = func.returnType;
			sformals 	= classFormal :: func.formals;
			sbody 		= fbody;
			func_type	= Sast.User;
			overrides   = func.overrides;
			source 		= cname;
		}
		
	in 
	
	(*convert constructors to functions*)
	let convertConstructorToSfunc classMaps reserved classMap cname constructor = 	

	
		let env = {
			envClassMaps 	= classMaps;
			envName     	= cname;
			envClassMap 		= classMap;
			envLocals    	= StringMap.empty;
			envParams	= List.fold_left (fun m f -> match f with Formal(d, s) -> (StringMap.add s f m) | _ -> m) StringMap.empty constructor.formals;
			envReturnType	= Datatype(Objecttype(cname));
			envInFor 		= false;
			envInWhile 	= false;
			envBuiltIn 	= reserved;}
		in 
		
		let fbody = fst (convertStmtsToSstmts env constructor.body) 
		in
				{
					sfname 		= Ast.FName(getConstructorName cname constructor);
					sreturnType = Datatype(Objecttype(cname));
					sformals 	= constructor.formals;
					sbody 		= appendConstructor fbody cname (Datatype(Objecttype(cname)));
					func_type	= Sast.User;
					overrides 	= false;
					source 		= "NA";
				}
	in

	
	

	(* Convert ast to sast*)
	let converttosast classMaps builtinFunctions cdecls = 
	
		let deConstructorBody cname = 
			let ret_type = Datatype(Objecttype(cname)) in
			let fbody = [] in
			appendConstructor fbody cname ret_type
		in

		let defaultSc cname = 
		{
			sfname 		= Ast.FName (cname ^ "." ^ "constructor");
			sreturnType = Datatype(Objecttype(cname));
			sformals 	= [];
			sbody 		= deConstructorBody  cname;
			func_type	= Sast.User;
			overrides   = false;
			source 		= "NA";
		}
		in
		
		(*convert the class in ast to sast type*)
		let convertClassToSast sfuncs cdecl =
			{scname =cdecl.cname;
			 sfields =cdecl.cbody.fields;
			 sfuncs= sfuncs;
			}
		in 
		
		let isMain fdecl = fdecl.sfname = FName("main")
		in
		let checkMain fdecls = 
			let mainFuncs = (List.filter isMain fdecls) in
				if List.length mainFuncs > 1
					then raise (Failure("Multiple main functions are defined!"))
				else if List.length mainFuncs < 1
					then raise (Failure("Main function is not defined!"))
				else List.hd mainFuncs
		in
		let removeMainFunc funcs = List.filter (fun func -> not(isMain func)) funcs 
		in
		let handleClass cdecl =
		let classMap =StringMap.find cdecl.cname classMaps 
		in
		let sConstructors = match cdecl.cbody.constructors with
											   [] -> (defaultSc cdecl.cname) :: [](*no user defined constructor*)
											|  _  -> List.map (convertConstructorToSfunc classMaps builtinFunctions classMap cdecl.cname) cdecl.cbody.constructors
		in
		let funcs = List.fold_left (fun l f -> (convertFuncToSfunc classMaps builtinFunctions classMap cdecl.cname f):: l) [] cdecl.cbody.methods
		in 
			let sfuncs = removeMainFunc funcs 
			in
				let scdecl = convertClassToSast sfuncs cdecl 
				in
					(scdecl, funcs @sConstructors)
	in 
		let loopClass t c = 
		let scdecl =handleClass c
		in (fst scdecl::fst t, snd scdecl @ snd t) 
		in
			let scdecls, funcs =List.fold_left loopClass ([],[]) cdecls 
			in
				let mainFunc = checkMain funcs in 
					let funcs=  removeMainFunc funcs in
						{ 
							classes = scdecls;
							functions = funcs;
							main = mainFunc;
							reserved = builtinFunctions;
						}
	in 					
	let sast = converttosast classMaps builtinFunctions cdecls in 
		sast 
