(* Semantic checking for the Liva compiler *)

open Ast
open Sast


(* Semantic checking of a program. Returns Sast if successful,
   throws an exception if something is wrong. *)

module StringMap = Map.Make(String)
module StringSet = Set.Make (String)





let struct_indexes: (string, int) Hashtle.t =  Hashtbl.create 10

let generate_struct_indexs cdecls= 
	let cdecls_handler index cdecl=
	Hashtbl.add struct_indexes cdecl.cname index in 
	List.iteri cdecl_handler cdecls







(*TODO: Includes files,  now it process a  NUll inlcudes*)
let process_includes reserved classess = classess







(*TODO:add more buit-in functions *)

let store_reserved_functions = 
	let i32_t =Datatype(Int_t) and 
	    void_t = Datatype(Void_t) and
	    str_t = Arraytype( Char_t, 1) in 
	let m t s = Formal(t, n) in
	let reserved_stub fname return_type formals = 
	      { sfname = FName (fname);
		sreturntType = return_type;
		sformals= formals;	
		func_type= Sast.Reserved;
		sbody=[];
		source= "None"
		}
	 in

	 let reserved_functions =[

		reserved_stub "print" (void_t) ([Many(Formal(Datatype, String))])

		]

	 in reserved_functions











(***********)
(*  TASK3 : Jiafei *)
(***********)
let build_class_maps reserved_functions cdecls = class_maps








(* to-do handle_inheritance *)
let handle_inheritance cdecls class_maps = class_maps, cdecls 


(***********)
(*  TASK4: Yanan *)
(***********)
let convert_sat_to_sast class_maps reserved_functions cdecls = sast 












(***********************************************************)
(* Entry point for translating Ast to Sast *)
(***********************************************************)

let check program = match program with 

	program (inlcudes, classes) ->
 	let cdecls = process_includes includes classess in 
	ignore (generate_struct_indexes  cdecls);


	(* add reserved built-in functions*)
	let reserved_functions = store_reserved_functions in 

	let class_maps = build_class_maps reserved_functions cdecls in 

        let class_maps, cdecls = handle_inheritance cdecls class_maps in 


	let sast = convert_sat_to_sast class_maps reserved_functions cdecls in 

	sast 




	

  
