(*其他文件也有调用,改名的话 一起改...*)
open Ast

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

let rec print_brackets = function
		1 -> "[]"
	| 	a -> "[]" ^ print_brackets (a - 1)

let string_of_datatype = function (*datatype*)
		Arraytype(p, i)	-> (string_of_primitive p) ^ (print_brackets i)
	| 	Datatype(p)		-> (string_of_primitive p)