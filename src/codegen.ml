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





let i32_t = i32_type context;;
let i8_t = i8_type context;;
let f_t = double_type context;;
let i1_t = i1_type context;;
let str_t = pointer_type i8_t;;
let i64_t = i64_type context;;
let void_t = void_type context;;

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





(* Declare and Define classes*)
let codegen_struct_stub s =
	let struct_t = named_struct_type context s.cname in
	Hashtbl.add struct_types s.cname struct_t


let codegen_struct s = 
	let struct_t = Hashtbl.find struct_types s.cname in
	let type_list = List.map (function Field(d, _) -> get_type d) s.cbody.fields in
	let name_list = List.map (function Field(_, s) -> s) s.cbody.fields in

	(* Add key field to all structs *)
	let type_list = i32_t :: type_list in
	let name_list = ".key" :: name_list in

	let type_array = (Array.of_list type_list) in
	List.iteri (fun i f ->
        let n = s.cname ^ "." ^ f in
        Hashtbl.add struct_field_indexes n i;
    ) name_list;
	struct_set_body struct_t type_array true




























(***********************************************************)
(* Entry point for translating Ast.program to LLVM module *)
(***********************************************************)

(* extract classes from AST*)
let process program = match program with
	Program(includes, classes) ->
 
	let ast_classes = classes in 

	ast_classes


(* Return the module*)
let translate program = 

	let classes = process program in

	let _ = List.map (fun s -> codegen_struct_stub s) classes in
	let _ = List.map (fun s -> codegen_struct s) classes in

	the_module;
	




















 
  the_module
