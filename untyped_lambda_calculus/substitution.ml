open Syntax
open Debruijn

let shift_with_cutoff (tm : 'a expr) (shift : int) (cutoff : int) : 'a expr =
  let onVar c info v = match v >= c with
    | true -> Var (v + shift, info)
    | false -> Var (v, info)
  in
  map_var succ onVar cutoff tm;;

let shift (shift : int) (tm : 'a expr) : 'a expr = shift_with_cutoff tm shift 0;;

let subst (j : int) (sub : 'a expr) (tm : 'a expr) =
  let onVar c info v = if v == j + c then shift c sub else Var(v, info) in
  map_var succ onVar 0 tm;;

(* beta-step in term reduction with shifts:
   (λ.t) v -→ ↑−1 ([0 ,↑1 v] t)  *)

let beta_step (body : 'a expr) (sub : 'a expr) : 'a expr =
  shift (- 1) (subst 0 (shift 1 sub) body)

let is_val (tm : info expr) : bool = match tm with
  | Lam(_) -> true
  | _ -> false ;;

exception NoReductionStep

let rec eval_step (tm : 'a expr) : 'a expr =
  let rec go = function
    | App((Lam(body, _), v), _info) when is_val v -> beta_step body v
    | App((tm1, tm2), _info) when is_val tm1 ->
      App ((tm1, go tm2), _info)

    | App((tm1, tm2), _info) ->
      App((go tm1, tm2), _info)
    | expr -> raise NoReductionStep
  in
  go tm

let rec normalize (tm : 'a expr) : 'a expr =
  match tm with
  | Var _ -> tm
  | Lam (body, info) -> Lam (normalize body, info)
  | App ((tm1, tm2), info) ->
      let tm1' = normalize tm1 in
      let tm2' = normalize tm2 in
      match tm1' with
      | Lam (body, _) -> normalize (beta_step body tm2')
      | _ -> App ((tm1', tm2'), info)

let rec whnf_eval (tm : 'a expr) : 'a expr =
  try
    let tm' = eval_step tm in
    whnf_eval tm'
  with NoReductionStep -> tm
