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
	builtFuncMap 	: sfunc_decl StringMap.t;
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

let strucIndexes: (string, int) Hashtbl.t =  Hashtbl.create 10

let inheritanceRelation:(string, string list) Hashtbl.t = Hashtbl.create 10

let createStructIndexes cdecls= 
	let classHandler index cdecl=
	Hashtbl.add strucIndexes cdecl.cname index in 
	List.iteri classHandler cdecls

let defaultC = 
{
	fname      = Ast.Constructor;
	returnType = Datatype(ConstructorType);
	formals    = [];
	body       = [];
	overrides 		= false;
	rootcname 		= None;
}

let getName cname fdecl = (*get the name of function,cname.constructor-> constructor / cname.xxx-> normal_function / main*)
	let name = string_of_fname fdecl.fname (*tell constructor from normal function*)
	in
	match name with
	 "main" -> "main"
	| _ 	-> cname ^ "." ^ name

		
let typOFSexpr = function(*get the type of sexpression*)
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
											

(**** Entry point for translating Ast to Sast *****)

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
				functype= Sast.Reserved;
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
					then raise (Failure ("Duplicated Field: " ^ n))
				else (StringMap.add n (Field(d, n)) mp)
			in
			
			let constructorpart condecl = 
				if List.length condecl > 1
					then raise (Failure ("Duplicated Constructor"))
				else if List.length condecl = 0 (*default constructor*)
					then StringMap.add (getConstructorName cdecl.cname defaultC) defaultC StringMap.empty
				else
					StringMap.add (getConstructorName cdecl.cname (List.hd condecl)) (List.hd condecl) StringMap.empty

			in
						
			let funcpart m fdecl = 
				let funname = getName cdecl.cname fdecl
				in

				if (StringMap.mem funname mp)
					then raise (Failure ("Duplicated Function: " ^ funname))
				else 
					let strfunname = string_of_fname fdecl.fname
					in
					
					if (StringMap.mem strfunname builtFuncMap)
						then raise (Failure ("Cannot use the reserved buit-in function name: " ^ strfunname))
					else (StringMap.add (getName cdecl.cname fdecl) fdecl m)
			in

			(if (StringMap.mem cdecl.cname mp)
				then raise (Failure ("Duplicated class name: " ^ cdecl.cname))
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
		let envref = ref env
		in
			let rec assistant = function
				h :: t -> let newh, env = exprToSexpr !envref h
						  in(
							envref := env;
							newh :: (assistant t))
			  | [] -> []
			in
				(assistant el), !envref 

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
				let typ= typOFSexpr sexpr (*get the type of sexpression*)
				in
					if typ = Datatype(Int_t) 
						then sexpr
					else raise (Failure ("Invalid index type for array initialization: " ^ string_of_datatype typ))
		in
			let checkTyp = function(*check whether the type can be array type*)
				   Datatype(x) -> Arraytype(x, arraySize)
				|  _ as t      -> raise (Failure ("Invalid array type: " ^ (string_of_datatype t)))
			in
				let typ = checkTyp d
				in
					let sel = List.map checkIndexType  el
					in
						SArrayCreate(d, sel, typ)

	and checkArrayAccess env e el = 
		let arraySize = List.length el (*get the size of array*)
		in
			let checkIndexType arg = (*check whether the type of index is int*)
				let sexpr, _ = exprToSexpr env arg (*convert expression to sexpression*)
				in
					let typ = typOFSexpr sexpr (*get the type of sexpression*)
					in
						if typ = Datatype(Int_t) 
							then sexpr
						else raise (Failure ("Invalid index type for array access: " ^ string_of_datatype typ))
			in
				let se, _ = exprToSexpr env e (*convert expression to sexpression*)
				in
					let typ = typOFSexpr se (*get the type of sexpression*)
					in
						let checkArraySize num = function
							   Arraytype(t, n) -> if num = n
													then Datatype(t)
												  else raise (Failure ("Invalid demention for array access: " ^ (string_of_int num) ^ " > " ^ (string_of_int n)))
							|  _ as t          -> raise (Failure ("Invalid type for array access: " ^ (string_of_datatype t)))	
						in
							let typ = checkArraySize arraySize typ
							in
								let sel = List.map checkIndexType el
								in SArrayAccess(se, sel, typ)

	and checkObjAccess env lhs rhs =

		let checkLHS = function(*check the expression before ‘.’ and get sexpression*)
			   This 	-> SId("this", Datatype(Objecttype(env.envName)))
			|  Id s 	-> SId(s, getIDType env s)
			|  ArrayAccess(e, el)	-> checkArrayAccess env e el
			|  _     	-> raise (Failure ("LHS of object access must be an instance of certain class"))
		in

		let getCname lhsTyp = match lhsTyp with (*get the type of the expression before ‘.’, i.e. class name*)
				Datatype(Objecttype(name)) 	-> name
			| 	_ as d						-> raise (Failure ("Object access must have ObjectType: " ^ string_of_datatype d))
		in
		let rec checkRHS (env) lhsTyp=
			let classname = getCname lhsTyp (*get the class name*)
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
					   Id s 		   -> SId(s, (search_classfield env s classname )), env (* Check fields*)
					|  Call(fname, el) -> let env = updateEnv env classname (* Check functions*)
										  in checkCallType env fname el, env
					|  _ as e		   -> raise (Failure ("Invalid object access: " ^ string_of_expr e))
		in

		let slhs= checkLHS lhs in
			
		let slhsTyp = typOFSexpr slhs in 
		
		let lcname = getCname slhsTyp in
				
		let lhsenv = updateEnv env lcname in
		
		let srhs, _ = checkRHS lhsenv slhsTyp (*env*) rhs in
		
		let srhsTyp = typOFSexpr srhs in 

		SObjAccess(slhs, srhs, srhsTyp )

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
				
				let ptyp = typOFSexpr param(*get the type of actual parameter*)
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
							let searchName = (getName env.envName func) in
							if x = searchName then 0 
							else if searchName = "main" then find x t 
							else 1 + find x t
					in
					find funcName fns
			in

			let checkParams formals params = match formals, params with (*check parameter according to amount*)
				[Many(Any)], _ 		-> params
				| [], []         	-> []
				| _              	-> if List.length formals <> List.length params
										then raise (Failure ("Incorrect argument number for function: " ^ fname))
										else List.map2 check_pa_onebyone formals sel
			in
			try
				let func = StringMap.find fname cmap.builtFuncMap
				in
				let actuals = checkParams func.sformals sel
				in SCall(fname, actuals, func.sreturnType,0)
			with | Not_found -> let sfname = env.envName ^ "." ^ fname
	in
		try let f = StringMap.find sfname cmap.functionMap
			in
			let actuals = checkParams f.formals sel in
			let index = getIndex f sfname in
			SCall(sfname, actuals, f.returnType, index)
		with | Not_found -> raise (Failure ("Function is not found: " ^ sfname))

	and checkConstructor env s el =
		let sel, env = exprsToSexprs env el
		in
		
		let params = List.fold_left 
			(fun s e -> s ^ "." ^ (string_of_datatype (typOFSexpr e))) "" sel
		in
					
		let constructorName = s ^ "." ^ "constructor" ^ params
		in
		
		let objectTyp = Datatype(Objecttype(s)) in

		SObjectCreate(constructorName, sel, objectTyp)

	and checkAssign env e1 e2 =
		let se1, env = exprToSexpr env e1(*convert expression to sexpression*)
		in
		let se2, env = exprToSexpr env e2
		in
		let type1 = typOFSexpr se1(*get the type of sexpression*)
		in
		let type2 = typOFSexpr se2
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
		let checkNum t = function(*operator for number*)
			   Sub     -> t
			|  _ as o  -> raise (Failure ("Invalid unary operation: " ^ string_of_op o))
		in 
		let checkBool = function(*operator for bool*)
			   Not    -> Datatype(Bool_t)
			|  _ as o -> raise (Failure ("Invalid unary operation: " ^ string_of_op o))
		in
		let se, env = exprToSexpr env e (*convert expression to sexpression*)
		in
		let st = typOFSexpr se (*get the type of sexpression*)
		in
			match st with (*check the type of operand*)
				   Datatype(Int_t) 	
				|  Datatype(Float_t) -> SUnop(op, se, checkNum st op)
				|  Datatype(Bool_t)  -> SUnop(op, se, checkBool op)
				|  _ as o            -> raise (Failure ("Invalid operant type for unary operation: " ^ string_of_datatype o))

	and checkBinop env e1 op e2 =	
		let getequal type1 type2 se1 se2 op =
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
		
		let getlogic type1 type2 se1 se2 op =(*check operants and conver to sbinop*)
			match type1, type2 with
				   Datatype(Bool_t), Datatype(Bool_t) -> SBinop(se1, op, se2, Datatype(Bool_t))
				|  _                                  -> raise (Failure ("Invalid type for logical operator" ^ (string_of_datatype type1) ^ "<->" ^ (string_of_datatype type2)))
		in
		
		let getcomp type1 type2 se1 se2 op =
			match type1, type2 with
				   Datatype(Int_t), Datatype(Float_t)
				|  Datatype(Float_t), Datatype(Int_t) -> SBinop(se1, op, se2, Datatype(Bool_t))
				|  _								  -> if type1 = type2
															then SBinop(se1, op, se2, Datatype(Bool_t))
														 else raise (Failure ("Invalid type for comparison operator: " ^ (string_of_datatype type1) ^ "<->" ^ (string_of_datatype type2)))
		in
		
		let getarith type1 type2 se1 se2 op =
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
		let type1 = typOFSexpr se1(*get the type of sexpression*)
		in
		let type2 = typOFSexpr se2(*get the type of sexpression*)
		in
		
			match op with(*check and convert binopexpression according to binopexpression type*)
					Equal | Neq                  -> getequal type1 type2 se1 se2 op
				|	And | Or                     -> getlogic type1 type2 se1 se2 op 
				|	Less | Leq | Greater | Geq   -> getcomp type1 type2 se1 se2 op
				|	Add | Mult | Sub | Div | Mod -> getarith type1 type2 se1 se2 op 
				| 	_                            -> raise (Failure ("Invalid binop operator: " ^ (string_of_op op)))
				
	in
	
	let rec convertStmtsToSstmts env stmts =
		let envref = ref(env) in
			let rec iter = function
				  h::t -> let newh, newenv = checkStmt !envref h in
									(envref := newenv; newh::(iter t))
				| [] -> []
			in 
			let sstmts = (iter stmts), !envref in
			sstmts 
			

	and checkExprStmt e env = 
		let se, env = exprToSexpr env e
		in
			let typ = typOFSexpr se
			in 
			SExpr(se, typ), env
				

	and checkIfStmt e s1 s2 env = 
		let se, _ = exprToSexpr env e and	
			ifbody, _ = checkStmt env s1 and
			elsebody, _ = checkStmt env s2 
		in
		let typ = typOFSexpr se 
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
		let cond = typOFSexpr se2 
		in 
		match cond with 
				Datatype(Bool_t) 	-> SFor(se1, se2, se3, forBodyStmt), env
			|	Datatype(Void_t)	-> SFor(se1, se2, se3, forBodyStmt), env
			|	_ 					-> raise (Failure("Invalid for statement type"))


	and checkWhileStmt e s env =
		let se, _ = exprToSexpr env e and 
			sstmt, _ = checkStmt env s
		in	    
		let typ = typOFSexpr se 
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
		let	typ = typOFSexpr se 
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
			let typ = typOFSexpr se 
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
			if typ = Datatype(Void_t) || typ = Datatype(Null_t) || typ = d
				then
					match d with
						  Datatype(Objecttype(x)) ->
							if not (StringMap.mem (Ast.string_of_object d) env.envClassMaps) 
								then raise (Failure ("Undefined Class: " ^ string_of_object d )) 
							else
								let local = SLocal(d, s, se)
								in local, update_env
						|  _ -> SLocal(d, s, se), update_env

			else 
				raise (Failure("Local assignment type mismatch: " ^ Ast.string_of_datatype d ^ " <-> " ^ Ast.string_of_datatype typ))	
				

	and  checkStmt env = function

			Expr e 				-> checkExprStmt e env
		| 	If(e, s1, s2) 		-> checkIfStmt e s1 s2	env
		|   While(e, s) 		-> checkWhileStmt e s env
		| 	For(e1, e2, e3, e4) -> checkForStmt e1 e2 e3 e4 env
		|	Block sl 			-> checkSblock sl env
		| 	Return e 			-> checkReturn e env
		|   Local(d, s, e) 		-> checkLocal d s e env
		
	in
	
	(* about inheritance*)
	let rec manageInheritance classes classMaps =
		let inheritanceMap = getInheritanceMap classes classMaps in(*forest: father class name -> [son class name]*)
		let allClassesM = getClassesForM classes inheritanceMap in(*all classes including inherited classes which have been dealed with according to methods*)
		let classMethodMap = getClassMethodMap allClassesM in
		let allClassmapsF = getClassmapForF classMaps inheritanceMap in(* classmap including inherited classes which have been dealed with according to field *)
		let finalMap = getFinalMap allClassmapsF allClassesM classMethodMap in
		finalMap, allClassesM

	and getInheritanceMap cdecls cmap = 
		let handler a cdecl =
			match cdecl.extends with 
				Parent(s) 	-> 
					let new_list = if (StringMap.mem s a) then
						cdecl.cname::(StringMap.find s a)
					else
						[cdecl.cname]
					in
					Hashtbl.add inheritanceRelation s new_list; 
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

	and getClassesForM cdecls inheritanceMap = 
		let cDataBase = List.fold_left (fun a litem -> StringMap.add litem.cname litem a) StringMap.empty cdecls (*class name -> class declaration*)
		in
		let seperateInheritanceMap fathers sons sets = 
			let fatherSet = StringSet.add fathers (fst sets) in
			let addSonList sndSet son = StringSet.add son sndSet in
			let sonSet = List.fold_left addSonList (snd sets) sons in
			(fatherSet, sonSet)
		in
		let sSet = StringSet.empty in
		let fatherAndson = StringMap.fold seperateInheritanceMap inheritanceMap (sSet, sSet) in
		let noFather = StringSet.diff (fst fatherAndson) (snd fatherAndson) in
		let rec getNewClasses newMap father sons = 
			let assistant newMap oneson = 
				let fatherCdecl = StringMap.find father newMap in (* class declaration of father*)
				let sonCdecl = StringMap.find oneson cDataBase in (* class declaration of one son of father *)
				let newSonCdecl = getNewSonCdecl fatherCdecl sonCdecl in 
				let newNewMap = (StringMap.add oneson newSonCdecl newMap) in 
				if (StringMap.mem oneson inheritanceMap) then 
					let sonsOFOneSon = StringMap.find oneson inheritanceMap in
					getNewClasses newNewMap oneson sonsOFOneSon
				else newNewMap
			in
			List.fold_left assistant newMap sons
		in
		let newClassMap = 
			let assistant noFather mp = 
				let sonOfSameFather = StringMap.find noFather inheritanceMap in (*list: son classes for a certain father class*)
				let noFatherMap = StringMap.add noFather (StringMap.find noFather cDataBase) mp in(*stringmap: root class name -> class declaration*)
				getNewClasses noFatherMap noFather sonOfSameFather 
			in
			StringSet.fold assistant noFather StringMap.empty 
		in
		let completeClasses cdecl mp = 
			let halfCompletedCMp = 
				try StringMap.find cdecl.cname newClassMap 
				with | Not_found -> cdecl
			in
			halfCompletedCMp::mp
		in
		let completedClassesM = List.fold_right completeClasses cdecls [] in
		completedClassesM


	and getNewSonCdecl fatherCdecl sonCdecl = 
		let sonCBody = 
			{
				fields = fatherCdecl.cbody.fields @ sonCdecl.cbody.fields;
				constructors = sonCdecl.cbody.constructors;
				methods = getFatherMethods fatherCdecl.cname fatherCdecl.cbody.methods sonCdecl.cbody.methods
			}
			in
			{
				cname = sonCdecl.cname;
				extends = sonCdecl.extends;
				cbody = sonCBody
			}

	and getFatherMethods father fatherMethods sonMethods =
		let checkMethod sonMethod lists = 
			let newSonMethods = 
				getNewSonMethod father (fst lists) sonMethod 
			in
			if (fst lists) = newSonMethods
				then ((fst lists), sonMethod::(snd lists)) 
			else (newSonMethods, (snd lists))
		in
		let allMethod = 
			List.fold_right checkMethod sonMethods (fatherMethods, [])
		in
		(fst allMethod) @ (snd allMethod)

	and getNewSonMethod father fatherMethods sonMethod = 
		let replace fatherMethod sonMethodList = 
			let getClassName = function
				None -> Some(father)
				| Some(x) -> Some(x)
			in
			let newSonMethod = 
				{
					fname = sonMethod.fname;
					returnType = sonMethod.returnType;
					formals = sonMethod.formals;
					body = sonMethod.body;
					overrides = true;
					rootcname = getClassName fatherMethod.rootcname;
				} 
			in
			if (getMethodName fatherMethod) = (getMethodName sonMethod) 
				then newSonMethod::sonMethodList 
				else fatherMethod::sonMethodList
		in
		List.fold_right replace fatherMethods []

	and getMethodName fdecl = 

		let params = List.fold_left 
			(fun s -> 
				(function 	  Formal(t, _) -> s ^ "." ^ Ast.string_of_datatype t
							| _            -> "" )) 
			"" fdecl.formals
		in
		let name = Ast.string_of_fname fdecl.fname in
		let ret_type = Ast.string_of_datatype fdecl.returnType in
		ret_type ^ "." ^ name ^ "." ^ params



	and getClassMethodMap allClasses = 
		let getMethodMap cdecl = (*stringmap: method name -> function declaration*)
			let addMethod mp fdecl = StringMap.add (getName cdecl.cname fdecl) fdecl mp in
			List.fold_left addMethod StringMap.empty cdecl.cbody.methods
		in
		let addClassMethodMap mp cdecl = StringMap.add cdecl.cname (getMethodMap cdecl) mp in (*stringmap: class name -> function map (upper)*)
		List.fold_left addClassMethodMap StringMap.empty allClasses

	and getClassmapForF classMaps inheritanceMap = 
		
		let seperateInheritanceMap fathers sons sets = 
			let fatherSet = StringSet.add fathers (fst sets) in
			let addSonList sndSet son = StringSet.add son sndSet in
			let sonSet = List.fold_left addSonList (snd sets) sons in
			(fatherSet, sonSet)
		in
		let sSet = StringSet.empty in
		let fatherAndson = StringMap.fold seperateInheritanceMap inheritanceMap (sSet, sSet) in
		let noFather = StringSet.diff (fst fatherAndson) (snd fatherAndson) in
		let rec getNewClassMap oldMap father sons = 
			let assistant oldMap son = 
				let fatherFieldMap = (StringMap.find father oldMap).fieldMap in (* field_map of father *)
				let sonFieldMap = (StringMap.find son oldMap).fieldMap in (* field_map son *)
				let newSonFieldMap = getNewSonFieldMap fatherFieldMap sonFieldMap in 
				let newMap = getNewMap newSonFieldMap son oldMap in
				if (StringMap.mem son inheritanceMap) then 
					let sonOfSon = StringMap.find son inheritanceMap in
					getNewClassMap newMap son sonOfSon 
				else newMap
			in
			List.fold_left assistant oldMap sons
		in
		let result = StringSet.fold (fun ns clp -> getNewClassMap clp ns (StringMap.find ns inheritanceMap)) noFather classMaps
		in result

	and getNewSonFieldMap fatherFieldMap sonFieldMap = (* add father's fields to son *)
		StringMap.fold (fun fa fi sonmp -> StringMap.add fa fi sonmp) fatherFieldMap sonFieldMap

	and getNewMap sonFieldMap son oldMap = 
		let assistant m = 
			{
					fieldMap = sonFieldMap;
					functionMap = m.functionMap;
					constructorMap = m.constructorMap;
					builtFuncMap = m.builtFuncMap;
					cdecl = m.cdecl;
			}

		in
		let sonMap = StringMap.find son oldMap in
		let newSonMap = assistant sonMap in
		let newMap = StringMap.add son newSonMap oldMap in
		newMap

	and getFinalMap allClassmapsF allClassesM classMethodMap =
		let getCdecl cname = 
			try List.find (fun cdecl -> cdecl.cname = cname) allClassesM
			with | Not_found -> raise (Failure("Class not found!")) (*impossible, has been checked before*)
		in
		let assistant cname cmap = 
			let mMap = StringMap.find cname classMethodMap in
			let cdecl = getCdecl cname in
			{
				fieldMap = cmap.fieldMap;
				functionMap = mMap;
				constructorMap = cmap.constructorMap;
				builtFuncMap = cmap.builtFuncMap;
				cdecl = cdecl;
			}
		in
		let updateCmap cname cmap mp = StringMap.add cname (assistant cname cmap) mp in
		StringMap.fold updateCmap allClassmapsF StringMap.empty
		
			
	
	in
	
    let classMaps, cdecls = manageInheritance classes classMaps 
	in
	
	let appendConstructor fbody cname returnType =
		let key = Hashtbl.find strucIndexes cname 
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
				thisInit @ fbody @ returnThis

	in
	
	let convertFuncToSfunc classMaps reserved classMap cname func=
	
		let appendMain fbodyStmt cname returnType = 
			let key = Hashtbl.find strucIndexes cname in 
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
		
		let rootClassName = match func.rootcname with 
			Some(x) -> x
			| None -> cname
		in

		let classFormal = 
			if func.overrides then 
				Ast.Formal(Datatype(Objecttype(rootClassName)), "this")
			else 
				Ast.Formal(Datatype(Objecttype(cname)), "this")
		in

		let envAssistant m fname = match fname with 
				Formal(d, s) -> (StringMap.add s fname m) 
			| 	_ -> m
		in
		let env_params = List.fold_left envAssistant StringMap.empty (classFormal :: func.formals) in
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
		
		let fbody = if funcName= "main" 
			then (appendMain fbody cname (Datatype(Objecttype(cname)))) 
			else fbody 
		in
		{
			sfname 		= Ast.FName (getName cname func);
			sreturnType = func.returnType;
			sformals 	= classFormal :: func.formals;
			sbody 		= fbody;
			functype	= Sast.User;
			overrides   = func.overrides;
			source 		= cname;
		}
		
	in 
	
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
					functype	= Sast.User;
					overrides 	= false;
					source 		= "NA";
				}
	in

	let converttosast classMaps builtinFunctions cdecls = 
	
		let deConstructorBody cname = 
			let retyp = Datatype(Objecttype(cname)) in
			let fbody = [] in
			appendConstructor fbody cname retyp
		in

		let defaultSc cname = 
		{
			sfname 		= Ast.FName (cname ^ "." ^ "constructor");
			sreturnType = Datatype(Objecttype(cname));
			sformals 	= [];
			sbody 		= deConstructorBody  cname;
			functype	= Sast.User;
			overrides   = false;
			source 		= "NA";
		}
		in
		
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
