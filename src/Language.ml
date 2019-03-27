(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
       
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
    let pBinop oper = ostap(- $(oper)), (fun x y -> Binop (oper, x, y))

    let operator oper = match oper with
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
          operator oper (eval state left) (eval state right)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (
      expr:
        !(Ostap.Util.expr
          (fun x -> x)
          (Array.map (fun (a, operator) -> a, List.map pBinop operator)
            [|
              `Lefta, ["!!"];
              `Lefta, ["&&"];
              `Nona , ["<="; "<"; ">="; ">"; "=="; "!="];
              `Lefta, ["+"; "-"];
              `Lefta, ["*"; "/"; "%"];
            |]
          )
          primary
          );
      primary:
          c: DECIMAL {Const c}
        | x: IDENT {Var x}
        | -"(" expr -")"
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) |RepeatUntil of t * Expr.t  with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval conf statement =
      let (state, input, output) = conf in
      match statement with
        | Read x -> (match input with
                      | head::tail -> (Expr.update x head state), tail, output)
        | Write expr -> (state, input, output @ [Expr.eval state expr])
        | Assign (x, expr) -> (Expr.update x (Expr.eval state expr) state, input, output)
        | Seq (st1, st2) -> 
            let st = eval conf st1
            in eval st st2
        | If (e, thS, eS) -> eval (state, input, output) (if Expr.bool_val (Expr.eval state e) then thS else eS)
        | While (e, wS) -> if Expr.bool_val (Expr.eval state e) then eval (eval (state, input, output) wS) statement else (state, input, output)
        | RepeatUntil (rS, e) -> 
          let (sN, iN, oN) = eval (state, input, output) rS in
                       if not (Expr.bool_val (Expr.eval sN e)) then eval (sN, iN, oN) statement else (sN, iN, oN)
        | Skip -> (state, input, output)
                               
    (* Statement parser *)

    ostap (
      simple:
			  x:IDENT ":=" e:!(Expr.expr) {Assign(x, e)}
			  | "read" "(" x:IDENT ")" {Read x}
        | "write" "(" e:!(Expr.expr) ")" {Write e};
      ifStmt:
        "if" e:!(Expr.expr) "then" thenBody:parse
      elifBranches: (%"elif" elifE:!(Expr.expr) %"then" elifBody:!(parse))*
      elseBranch: (%"else" elseBody:!(parse))?
        "fi" {
           let elseBranch' = match elseBranch with
             | Some x -> x
                 | None   -> Skip in
           let expandedElseBody = List.fold_right (fun (e', body') else' -> If (e', body', else')) elifBranches elseBranch' in
           If (e, thenBody, expandedElseBody)
         };
      whileStmt:
        "while" e:!(Expr.expr) "do" body:parse "od" {While (e, body)};
      forStmt:
        "for" initStmt:stmt "," whileCond:!(Expr.expr) "," forStmt:stmt
        "do" body:parse "od" {Seq (initStmt, While (whileCond, Seq (body, forStmt)))};
      repeatUntilStmt:
        "repeat" body:parse "until" e:!(Expr.expr) {RepeatUntil (body, e)};
      control:
          ifStmt
        | whileStmt
        | forStmt
        | repeatUntilStmt
        | "skip" {Skip};
      stmt:
          simple
        | control;
  		parse: s:stmt ";" rest:parse {Seq(s, rest)} | stmt
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
