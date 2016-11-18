exception UndefSemantics

type program = exp
and exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
  | CALLREF of exp * var
  | SET of var * exp
  | SEQ of exp * exp
  | BEGIN of exp
and var = string

type value = 
    Int of int 
  | Bool of bool 
  | Closure of var * exp * env 
  | RecClosure of var * var * exp * env
and env = var -> loc
and loc = int
and mem = loc -> value

(*********************************)
(* implementation of environment *)
(*********************************)
(* empty env *)
let empty_env = fun x -> raise (Failure "Environment is empty")
(* extend the environment e with the binding (x, l), where x is a varaible and l is a location *)
let extend_env (x,l) e = fun y -> if x = y then l else (e y)
(* look up the environment e for the variable x *)
let apply_env e x = e x

(*********************************)
(* implementation of memory      *)
(*********************************)
let empty_mem = fun _ -> raise (Failure "Memory is empty")
(* extend the memory m with the binding (l, v), where l is a location and v is a value *)
let extend_mem (l,v) m = fun y -> if l = y then v else (m y)
let apply_mem m l = m l

(* NOTE: you don't need to understand how env and mem work *)

let counter = ref 0

(* calling 'new_location' produces a fresh memory location *)
let new_location () = counter:=!counter+1;!counter

let value2str v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Closure (x,e,env) -> "Closure "
  | RecClosure (f,x,e,env) -> "RecClosure "^f


(* TODO: Implement this function *)
let rec eval : exp -> env -> mem -> value * mem
=fun exp env mem -> 
  match exp with
  | CONST n -> (Int n, mem)
  | VAR x ->  (apply_mem mem (apply_env env x), mem)
  | ADD (e1, e2) ->
    let (v1, m1) = eval e1 env mem in
    let (v2, m2) = eval e2 env m1 in
      (match v1, v2 with
      | Int n1, Int n2 -> (Int (n1 + n2), m2)
      | _ -> raise UndefSemantics)
  | SUB (e1, e2) ->
    let (v1, m1) = eval e1 env mem in
    let (v2, m2) = eval e2 env m1 in
      (match v1, v2 with
      | Int n1, Int n2 -> (Int (n1 - n2), m2)
      | _ -> raise UndefSemantics)
  | MUL (e1, e2) ->
    let (v1, m1) = eval e1 env mem in
    let (v2, m2) = eval e2 env m1 in
      (match v1, v2 with
      | Int n1, Int n2 -> (Int (n1 * n2), m2)
      | _ -> raise UndefSemantics)
  | DIV (e1, e2) ->
    let (v1, m1) = eval e1 env mem in
    let (v2, m2) = eval e2 env m1 in
      (match v1, v2 with
      | Int n1, Int n2 -> (Int (n1 / n2), m2)
      | _ -> raise UndefSemantics)
  | ISZERO e ->
    let (v, m) = eval e env mem in
      (match v with
      | Int n -> if n = 0 then (Bool true, m) else (Bool false, m)
      | _ -> raise UndefSemantics)
  | READ -> (Int (read_int()), mem)
  | IF (e1, e2, e3) ->
    let (v1, m1) = eval e1 env mem in
      (match v1 with
      | Bool true -> eval e2 env m1
      | Bool false -> eval e3 env m1
      | _ -> raise UndefSemantics)
  | LET (x, e1, e2) ->
    let loc = new_location() in
    let (v1, m1) = eval e1 env mem in
    let env' = extend_env (x, loc) env in
    let m1' = extend_mem (loc, v1) m1 in
      eval e2 env' m1'
  | LETREC (f, x, e1, e2) ->
    let loc = new_location() in
    let env' = extend_env (f, loc) env in
    let mem' = extend_mem (loc, RecClosure (f, x, e1, env)) mem in
      eval e2 env' mem'
  | PROC (x, e) -> (Closure (x, e, env), mem)
  | CALL (e1, e2) ->
    let loc = new_location() in
    let (v1, m1) = eval e1 env mem in
    let (v2, m2) = eval e2 env m1 in
      (match v1 with
      | Closure (x, e, env') ->
        let env'' = extend_env (x, loc) env' in
        let m2' = extend_mem (loc, v2) m2 in
          eval e env'' m2'
      | RecClosure (f, x, e, env') ->
        let loc' = new_location() in
        let env'' = extend_env (f, loc') (extend_env (x, loc) env') in
        let m2' = extend_mem (loc', v1) (extend_mem (loc, v2) m2) in
          eval e env'' m2'
      | _ -> raise UndefSemantics)
  | CALLREF (e1, y) ->
    let (v, m1) = eval e1 env mem in
      (match v with
      | Closure (x, e, env') ->
        let env'' = extend_env (x, apply_env env y) env' in
          eval e env'' m1
      | RecClosure (f, x, e, env') ->
        let loc = new_location() in
        let env'' = extend_env (f, loc) (extend_env (x, apply_env env y) env') in
        let m1' = extend_mem (loc, v) m1 in
          eval e env'' m1'
      | _ -> raise UndefSemantics)
  | SET (x, e) ->
    let (v, m1) = eval e env mem in
      (v, extend_mem (apply_env env x, v) m1)
  | SEQ (e1, e2) ->
    let (v1, m1) = eval e1 env mem in
      eval e2 env m1
  | BEGIN e -> eval e env mem

let run : program -> value
=fun pgm -> 
  let (v,m) = eval pgm empty_env empty_mem in
    v
