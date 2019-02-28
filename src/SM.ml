open GT       
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
 let instruct config evt =
	  let (stack, f) = config in
	  let (st, input, output) = f in
	    match evt with
	      | ST var   -> (match stack with
                        | x::rest -> rest, (Syntax.Expr.update var x st, input, output))
        | LD var   -> [st var] @ stack, f
        | BINOP op -> (match stack with
	                      | y::x::rest -> [Syntax.Expr.action op x y] @ rest, f)
	      | READ     -> (match input with
		                    | x::rest -> [x] @ stack, (st, rest, output))
	      | WRITE    -> (match stack with
		                    | x::rest -> rest, (st, input, output @ [x]))
	      | CONST x  -> [x] @ stack, f
  let eval config prog = List.fold_left instruct config prog;;

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

 let rec comp_expr exp = match exp with  
    | Syntax.Expr.Var x                   -> [LD x]
    | Syntax.Expr.Binop (op, left, right) -> (comp_expr left) @ (comp_expr right) @ [BINOP op]
    | Syntax.Expr.Const x                 -> [CONST x];;


let rec compile program = match program with
    | Syntax.Stmt.Write exp               -> (comp_expr exp) @ [WRITE]
    | Syntax.Stmt.Seq (f, s)        -> (compile f) @ (compile s)
    | Syntax.Stmt.Assign (curr, exp)      -> (comp_expr exp) @ [ST curr]
    | Syntax.Stmt.Read c                -> [READ; ST c];;
