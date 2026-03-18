open Syntax
open Debruijn

type debruijn_level = Level of int;;

(* in a context of length l = n + 1 we should have that:
   0 |-> n = l - 1
   1 |-> n-1 = l - 2
     ...
   k |-> n - k = l - k - 1
     ...
   n |-> 0 = l - n - 1
*)
let level_to_index (ctxt_size : int) (idx : debruijn_level) : debruijn_index =
  let Level idx = idx in
  (ctxt_size - idx) - 1;;

type closure =
  | Closure of env *  unit expr
and neutral =
  | NVar of debruijn_level
  | NApp of value * value
and value =
  | VFun of closure
  | VNeu of neutral
and env =
  | Empty
  | Push of (env * value)

exception VariableOutOfScope

let vvar lvl : value = VNeu (NVar (Level lvl))
let vapp v1 v2 : value = VNeu (NApp(v1, v2))

let rec lookup (idx : debruijn_index) (env : env) =
  match (idx, env) with
  | (_, Empty) -> raise VariableOutOfScope
  | (0, Push(_, value)) -> value
  | (n, Push(env', _)) -> lookup (n - 1) env'

let rec eval (env : env) (tm : 'a expr) : value =
  match tm with
  | Var(v, info) -> lookup v env
  | App ((rator, rand), info) ->
    (match (eval env rator, eval env rand) with
         | (VFun(closure), arg) -> apply_closure closure arg
         | (nfun, arg) -> VNeu(NApp(nfun, arg))
    )
  | Lam (body, info) -> VFun(Closure(env, body))
and apply_closure (clos : closure) (arg : value) =
  let Closure(clos_env, body) = clos in
  eval (Push (clos_env, arg)) body

let rec lift (ctxt_size : int) (value : value) : unit expr =
  match value with
  | VNeu(NVar(lvl)) ->
    let idx = level_to_index ctxt_size lvl in
    Var(idx, ())
  | VNeu(NApp(rator, rand)) ->
    Debruijn.app (lift ctxt_size rator) (lift ctxt_size rand)
  | VFun(clos) ->
    Debruijn.lam (lift (succ ctxt_size) (apply_closure clos (vvar ctxt_size)))

let rec normalize (tm : 'a expr) : unit expr =
  lift 0 (eval Empty tm)
