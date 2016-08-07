type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Not | Or | Mod

type primitive = Int_t | Float_t | Void_t | Bool_t | Char_t | String_t | Objecttype of string | ConstructorType | Null_t 

type datatype = Arraytype of primitive * int | Datatype of primitive |Any

type extends = NoParent | Parent of string
type fname = Constructor | FName of string
type formal = Formal of datatype * string | Many of datatype

type expr =
		Int_Lit of int
	| 	Boolean_Lit of bool
	| 	Float_Lit of float
	| 	String_Lit of string
	| 	Char_Lit of char
	| 	This (*need to be implemented*)
	| 	Id of string
	| 	Binop of expr * op * expr
	| 	Assign of expr * expr
	| 	Noexpr
	| 	ArrayCreate of datatype * expr list
	| 	ArrayAccess of expr * expr list	
	| 	ObjAccess of expr * expr
	| 	Call of string * expr list  
	|       ObjectCreate of string * expr list
	|  	Unop of op * expr	
	| 	Null

type stmt =
		Block of stmt list
	| 	Expr of expr
	| 	Return of expr
	| 	If of expr * stmt * stmt
	| 	For of expr * expr * expr * stmt
	| 	While of expr * stmt
	|  	Break
	|       Continue
	|       Local of datatype * string * expr

type field = Field of datatype * string

type func_decl = {
	fname : fname;
	returnType : datatype;
	formals : formal list;
	body : stmt list;
	overrides : bool;
	root_cname : string option;
	
}

type cbody = {
	fields : field list;
	constructors : func_decl list;
	methods : func_decl list;
}

type class_decl = {
	cname : string;
	extends : extends;
	cbody: cbody;
}

type program = Program of class_decl list

(*get function name,tell constructor from ordinary functions*)
let string_of_fname = function
		Constructor -> "constructor"
	|	FName(s)	-> s

let string_of_primitive = function (*primitive type*)
		Int_t 			-> "int"
	| 	Float_t 		-> "float"
	| 	Void_t			-> "void"
	| 	Bool_t 			-> "bool"
	| 	Char_t 			-> "char"
	| 	Objecttype(s)	-> "class" ^ " " ^ s
	| 	ConstructorType	-> "constructor"
	|  	Null_t 			-> "null"
	|   String_t        -> "String"


let string_of_object = function
		Datatype(Objecttype(s))	-> s
	| 	_ -> ""




let rec print_brackets = function
		1 -> "[]"
	| 	a -> "[]" ^ print_brackets (a - 1)

let string_of_expr e = "remain to be completed"

let string_of_datatype = function (*datatype*)
		Arraytype(p, i)	-> (string_of_primitive p) ^ (print_brackets i)
	| 	Datatype(p)		-> (string_of_primitive p)
	|  	Any 			-> "Any"
	
let string_of_op = function(*operator*)
		Add			-> "+"	
	 | 	Sub			-> "-"	
	 | 	Mult		-> "*"	
	 | 	Div			-> "/"	
	 | 	Equal		-> "=="		
	 | 	Neq			-> "!="	
	 | 	Less		-> "<"	
	 | 	Leq			-> "<="	
	 | 	Greater		-> ">"			
	 | 	Geq			-> ">="	
	 | 	And			-> "and"	
	 | 	Not			-> "not"	
	 | 	Or			-> "or"
	 | 	Mod 		-> "%"


let string_of_boolean b = match b with
	  true -> "true"
	| false -> "false"






