(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let int_val op l r= if (op l r) then 1 else 0;;
    let bool_val v = v<>0;;
    let disj_conj op = fun l r -> int_val op (bool_val l) (bool_val r);;

    let eval_operation oper = match oper with
      | "+" -> ( + )
      | "-" -> ( - )
      | "*" -> ( * )
      | "/" -> ( / )
      | "%" -> ( mod )
      | "!!" -> disj_conj( || )
      | "&&" -> disj_conj( && )
      | "==" -> int_val( = )
      | "!=" -> int_val( <> )
      | "<=" -> int_val( <= )
      | "<" -> int_val( < )
      | ">=" -> int_val( >= )
      |  ">" -> int_val( > );;


    let rec eval state expr = match expr with
      | Const n -> n
      | Var x -> state x
      | Binop (oper, left, right) -> 
          eval_operation oper (eval state left) (eval state right);;

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval config statement =
      let (state, input, output) = conf in
      match statement with
        | Read x -> (match input with
                      | head::tail -> (Expr.update x head state), tail, output)
        | Write expr -> (state, input, output @ [Expr.eval state expr])
        | Assign (x, expr) -> (Expr.update x (Expr.eval state expr) state, input, output)
        | Seq (st1, st2) -> 
            let st = eval conf st1
            in eval st st2;; 
                                                         
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : int list -> t -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval i p =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
