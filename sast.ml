open Ast

type sexpr =
		SInt_Lit of int
	| 	SBoolean_Lit of bool
	| 	SFloat_Lit of float
	| 	SString_Lit of string
	| 	SChar_Lit of char
	| 	SId of string * datatype
	| 	SBinop of sexpr * op * sexpr * datatype
	| 	SAssign of sexpr * sexpr * datatype
	| 	SNoexpr
	| 	SArrayCreate of datatype * sexpr list * datatype
	| 	SArrayAccess of sexpr * sexpr list * datatype
	| 	SObjAccess of sexpr * sexpr * datatype
	| 	SCall of string * sexpr list * datatype * int
	|   SObjectCreate of string * sexpr list * datatype
	| 	SArrayElements of sexpr list * datatype
	|  	SUnop of op * sexpr * datatype
	| 	SNull

type sstmt =
		SBlock of sstmt list
	| 	SExpr of sexpr * datatype
	| 	SReturn of sexpr  * datatype
	| 	SIf of sexpr * sstmt * sstmt
	| 	SFor of sexpr * sexpr * sexpr * sstmt
	| 	SWhile of sexpr * sstmt
	|       SLocal of datatype * string * sexpr

type func_type = User | Reserved

type sfunc_decl = {
	sfname : fname;
	sreturnType : datatype;
	sformals : formal list;
	sbody : sstmt list;
	func_type : func_type;
	source : string;
	overrides : bool;
}

type sclass_decl = {
	scname : string;
	sfields : field list;
	sfuncs: sfunc_decl list;
}

(* Class Declarations | All method declarations | Main entry method *)
type sprogram =  {
	classes : sclass_decl list;
	functions : sfunc_decl list;
	main : sfunc_decl;
	reserved : sfunc_decl list;
}



let rec string_of_sexpr = function 
		SInt_Lit(i)					-> string_of_int i
	|	SBoolean_Lit(b)				-> if b then "true" else "false"
	|	SFloat_Lit(f)				-> string_of_float f
	|	SString_Lit(s)				-> "\"" ^ (String.escaped s) ^ "\""
	|	SChar_Lit(c)				-> Char.escaped c
	|	SId(s, _)					-> s
	|	SBinop(e1, o, e2, _)		-> (string_of_sexpr e1) ^ " " ^ (string_of_op o) ^ " " ^ (string_of_sexpr e2)
	|	SAssign(e1, e2, _)			-> (string_of_sexpr e1) ^ " = " ^ (string_of_sexpr e2)
	|	SNoexpr						-> "Noexpr"
	|	SObjAccess(e1, e2, _)		-> (string_of_sexpr e1) ^ "." ^ (string_of_sexpr e2)
	|	SCall(f, el, _, _)			-> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
	(*to do*)
	|  	SUnop(op, e, _)				-> (string_of_op op) ^ "(" ^ string_of_sexpr e ^ ")"
	|	SNull						-> "null"
	(*todo*)
	(*to do*)
	|   SObjectCreate(s, el, _) 	-> "new " ^ s ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"

	|   SArrayCreate(d, el, _)  	-> "new " ^ string_of_datatype d ^ string_of_bracket_sexpr el
	|   SArrayAccess(e, el, _)  	-> (string_of_sexpr e) ^ (string_of_bracket_sexpr el)
	|	SArrayElements(el, _)		-> "{" ^ (string_of_sarray_primitive el) ^ "}"

and string_of_bracket_sexpr = function
		[] 				-> ""
	| 	head :: tail 	-> "[" ^ (string_of_sexpr head) ^ "]" ^ (string_of_bracket_sexpr tail)

and string_of_sarray_primitive = function
		[] 				-> ""
	|   [last]			-> (string_of_sexpr last)
	| 	head :: tail 	-> (string_of_sexpr head) ^ ", " ^ (string_of_sarray_primitive tail)






