(* Top-level of the Liva compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)
open Ast
open Sast


type action = Ast | LLVM_IR | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);	(* Print the AST only *)
			      ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
			      ("-c", Compile) ] (* Generate, check LLVM IR *)
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let program = fun() -> Parser.program Scanner.token lexbuf in
  let sprogram = fun () -> Semant.check (program()) in
  let llvm = fun() -> Codegen.translate (sprogram());

  match action with
    Ast -> print_string ("Not completed yet")
  | LLVM_IR -> print_string (Llvm.string_of_llmodule llvm())
  | Compile -> let m = llvm() in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)
