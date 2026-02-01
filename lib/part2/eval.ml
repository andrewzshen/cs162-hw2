open Ast

let todo () = failwith "TODO"
let bonus () = failwith "BONUS"

exception Stuck of string
(** Exception indicating that evaluation is stuck *)

(** Raises an exception indicating that evaluation got stuck. *)
let im_stuck msg = raise (Stuck msg)

(** Computes the set of free variables in the given expression *)
let rec free_vars (e : expr) : Vars.t =
    (* This line imports the functions in Vars, so you can write [diff .. ..]
       instead of [Vars.diff .. ..] *)
    let open Vars in
    (* Your code goes here *)
    match e with
    | Num _ -> empty
    | Binop (_, e1, e2) -> union (free_vars e1) (free_vars e2)
    | Var x -> singleton x
    | Lambda binder ->
        let x, body = binder in
            diff (free_vars body) (singleton x)
    | App (e1, e2) -> union (free_vars e1) (free_vars e2)
    | Let (e1, binder) ->
        union (free_vars e1)
            (let x, body = binder in
                diff (free_vars body) (singleton x))

(** Perform substitution c[x -> e], i.e., substituting x with e in c *)
let rec subst (x : string) (e : expr) (c : expr) : expr =
    match c with
    | Num n -> Num n
    | Binop (op, c1, c2) -> Binop (op, subst x e c1, subst x e c2)
    | Var y ->
        if x = y then
            e
        else
            Var y
    | Lambda binder ->
        let y, body = binder in
            Lambda
                (y,
                if String.equal x y then
                    body
                else
                    subst x e body )
    | App (c1, c2) -> App (subst x e c1, subst x e c2)
    | Let (c1, binder) ->
        Let
            (subst x e c1,
            let y, body = binder in
                ( y,
                    if String.equal x y then
                    body
                    else
                    subst x e body))

(** Evaluate expression e *)
let rec eval (e : expr) : expr =
    try
        match e with
        | Num n -> Num n
        | Binop (op, e1, e2) -> 
            let e1' = eval e1 in
            let e2' = eval e2 in 
            (match e1', e2' with
            | Num x, Num y -> 
                (match op with
                | Add -> Num (x + y)
                | Sub -> Num (x - y)
                | Mul -> Num (x * y))
            | _, _ -> im_stuck (Fmt.str "Invalid binop: %a" Pretty.expr e))
        | Var _ -> im_stuck (Fmt.str "Unassigned: %a" Pretty.expr e) 
        | Lambda binder -> Lambda binder 
        | App (e1, e2) -> 
            let e1' = eval e1 in
            let e2' = eval e2 in 
            (match e1' with
            | Lambda (x, body) -> eval (subst x e2' body) 
            | _ -> im_stuck (Fmt.str "Application to non lambda: %a" Pretty.expr e))
        | Let (e1, (x, e2)) -> eval (subst x (eval e1) e2)
        | _ -> im_stuck (Fmt.str "Ill-formed expression: %a" Pretty.expr e)
    with Stuck msg ->
        im_stuck (Fmt.str "%s\nin expression %a" msg Pretty.expr e)

type sigma = (string * expr) list
(** Substitution  *)

(** Perform simultaneous substitution c[sigma], i.e., substituting variables in c according to sigma *)
let rec subst_multi (sigma : sigma) (c : expr) : expr = 
    match c with
    | Num n -> Num n
    | Binop (op, c1, c2) -> Binop (op, subst_multi sigma c1, subst_multi sigma c2)
    | Var y ->
        let rec lookup (x : string) (s : sigma) : expr option = 
            match s with
            | [] -> None
            | (y, e)::rest -> 
                if String.equal x y then Some e else lookup x rest
        in
        (match lookup y sigma with
        | Some e -> e
        | None -> Var y)
    | Lambda binder ->
        let x, body = binder in
        let sigma' = List.filter (fun (y, _) -> y <> x) sigma in
        Lambda (x, subst_multi sigma' body)
    | App (c1, c2) -> App (subst x e c1, subst x e c2)
    | Let (c1, binder) ->
        Let
            (subst x e c1,
            let y, body = binder in
                ( y,
                    if String.equal x y then
                    body
                    else
                    subst x e body))

(** Alpha-equivalence *)
let alpha_equiv (e1 : expr) (e2 : expr) : bool = bonus ()
