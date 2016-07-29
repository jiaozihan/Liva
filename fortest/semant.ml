(* Semantic checking for the Liva compiler *)

open Ast
open Sast


(* Semantic checking of a program. Returns Sast if successful,
   throws an exception if something is wrong. *)

module StringMap = Map.Make(String)
module StringSet = Set.Make (String)

type class_map ={

	field_map: Ast.field StringMap.t;
	func_map: Ast.func_decl StringMap.t;
	constructor_map: Ast.func_decl StringMap.t;
	reserved_map: sfunc_decl StringMap.t;
	cdecl: Ast.class_decl;

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






let struct_indexes: (string, int) Hashtbl.t =  Hashtbl.create 10

let build_struct_indexs cdecls= 
	let cdecls_handler index cdecl=
	Hashtbl.add struct_indexes cdecl.cname index in 
	List.iteri cdecls_handler cdecls

let default_c cname = 
{

	fname 			= Ast.Constructor;
	returnType 		= Datatype(ConstructorType);
	formals 		= [];
	body 			= [];
	
}

let default_sc cname = 
{
	sfname 		= Ast.FName (cname ^ "." ^ "constructor");
	sreturnType 	= Datatype(Objecttype(cname));
	sformals 	= [];
	sbody 		= [] (*TODO*);
	func_type	= Sast.User;
	source 		= "NA";
}


let string_of_fname = function 
		Constructor -> "constructor"
	|	FName(s)	-> s

let string_of_primitive = function 
		Int_t 						-> "int"
	| 	Float_t 					-> "float"
	| 	Void_t						-> "void"
	| 	Bool_t 						-> "bool"
	| 	Char_t 						-> "char"
	| 	Objecttype(s)				-> "class " ^ s
	| 	ConstructorType				-> "constructor"
	|  	Null_t 						-> "null"
        |       String_t                                        -> "String"



let string_of_datatype = function 
		
	| 	Datatype(p)		-> (string_of_primitive p)
        |       _                       -> "not implemented"
	

let get_name cname fdecl = 
	let name = string_of_fname fdecl.fname in
	if name = "main" 
		then "main"
		else cname ^ "." ^ name(*  ^ params *)

let get_constructor_name cname fdecl = 
	let params = List.fold_left (fun s -> (function Formal(t, _) -> s ^ "." ^ string_of_datatype t | _ -> "" )) "" fdecl.formals in
	let name = string_of_fname fdecl.fname in
	cname ^ "." ^ name ^ params








let rec get_ID_type env s = 
	try StringMap.find s env.env_locals
	with | Not_found -> 
	try let formal = StringMap.find s env.env_parameters in
		(function Formal(t, _) -> t | Many t -> t) formal
	with | Not_found -> raise (Failure("UndefinedID"))

and get_type_from_sexpr = function
		SInt_Lit(_)			-> Datatype(Int_t)
	| 	SBoolean_Lit(_)			-> Datatype(Bool_t)
	| 	SFloat_Lit(_)			-> Datatype(Float_t)
	| 	SString_Lit(_) 			-> Arraytype(Char_t, 1)
	| 	SChar_Lit(_) 			-> Datatype(Char_t)
	| 	SCall(_, _, d, _)		-> d
	|       _                               -> Datatype (String_t)



let rec expr_to_sexpr env = function
	    Int_Lit i           -> SInt_Lit(i), env
	|   Boolean_Lit b       -> SBoolean_Lit(b), env
	|   Float_Lit f         -> SFloat_Lit(f), env
	|   String_Lit s        -> SString_Lit(s), env
	|   Char_Lit c          -> SChar_Lit(c), env
	|   Call(s, el)         -> let sel=expr_list_to_sexpr_list el env in SCall(s,  sel, Datatype (Void_t), 0), env 
        |   _                   -> SInt_Lit(1), env

	
and  expr_list_to_sexpr_list el env = match el with
    hd :: tl ->
        let (se, env) = expr_to_sexpr env hd in
        se :: expr_list_to_sexpr_list tl env
  | [] -> []







(*TODO: Includes files,  now it process a  NUll inlcudes*)
let process_includes includes classess = classess







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
let build_class_maps reserved_functions cdecls = 

	let reserved_map = List.fold_left (fun m f -> StringMap.add (string_of_fname f.sfname) f m ) StringMap.empty reserved_functions in 

	let helper m (cdecl: Ast.class_decl) =
		let fieldfun =(fun m -> (function Field(d , n) -> if  (StringMap.mem (n) m) then raise (Failure ("Yanan"))  else (StringMap.add n (Field(d,n)) m )   )) 
		in
		let funcname = get_name cdecl.cname in
		let funcfun m fdecl = 
			if (StringMap.mem (funcname fdecl) m)
				then raise (Failure ("YANANAN"))
			else if (StringMap.mem (string_of_fname fdecl.fname) reserved_map)
				then raise (Failure ("YANANAN"))
			else (StringMap.add (funcname fdecl) fdecl m)

		in 

		let constructor_name = get_constructor_name cdecl.cname in
		let constructorfun m fdecl = 
			if fdecl.formals = [] then m
			else if StringMap.mem (constructor_name fdecl) m 
				then raise(Failure"DuplicateConstructor") 
				else (StringMap.add (constructor_name fdecl) fdecl m)
		in

		let default_c = default_c cdecl.cname in 
		
		let constructor_map = StringMap.add (get_constructor_name cdecl.cname default_c) default_c StringMap.empty in
		(if (StringMap.mem cdecl.cname m) then raise (Failure("DuplicateClassName(cdecl.cname)")) else
			StringMap.add cdecl.cname 
			{ 	field_map = List.fold_left fieldfun StringMap.empty cdecl.cbody.fields; 
				func_map = List.fold_left funcfun StringMap.empty cdecl.cbody.methods;
				constructor_map = List.fold_left constructorfun constructor_map cdecl.cbody.constructors; 
				reserved_map = reserved_map; 
				cdecl = cdecl } 
										 m) in
	List.fold_left helper StringMap.empty cdecls 



		









(* TODO: handle_inheritance *)
let handle_inheritance cdecls class_maps = class_maps, cdecls 


(***********)
(*  TASK4: Conversion of Sat to Sast *)
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




(* convert the constructor in sat to the function in Sast *)
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




(* convert methods in sat to the functions in Sast*)
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



















(*convert the class in sat to sast type*)
let convert_cdecl_to_sast sfdecls (cdecl: Ast.class_decl) =
	{scname =cdecl.cname;
	 sfields =cdecl.cbody.fields;
	 sfuncs= sfdecls;
	}




(* Convert sat to sast*)
let convert_sat_to_sast class_maps reserved_functions cdecls = 


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
	ignore (build_struct_indexs  cdecls);


	(* add reserved built-in functions*)
	let reserved_functions = store_reserved_functions in 

	let class_maps = build_class_maps reserved_functions cdecls in 

        let class_maps, cdecls = handle_inheritance cdecls class_maps in 


	let sast = convert_sat_to_sast class_maps reserved_functions cdecls in 

	sast 




	

  
