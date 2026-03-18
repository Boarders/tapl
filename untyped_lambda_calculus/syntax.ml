let pp_position fmt pos =
  Format.fprintf fmt "%s:%d:%d" pos.Lexing.pos_fname pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

type src_info = {
  start_pos : Lexing.position [@printer pp_position];
  end_pos : Lexing.position [@printer pp_position];
}
[@@deriving show]

type info = src_info
[@@deriving show]

type var = string
[@@deriving show]

module Raw = struct
  type 'a expr =
    | Var of var * 'a
    | Lam of (var * 'a expr) * 'a
    | App of ('a expr * 'a expr) * 'a
  [@@deriving show]
end

module Debruijn = struct
  type debruijn_index = int
    [@@deriving show]
  type 'a expr =
    | Var of debruijn_index * 'a
    | Lam of 'a expr * 'a (* first argument is the size of the context *)
    | App of ('a expr * 'a expr) * 'a
  [@@deriving show, map]


  let var i : unit expr = Var (i, ())
  let lam body : unit expr = Lam (body, ())
  let app rator rand : unit expr = App ((rator, rand), ())


  let rec map_var
      (onBinder : 'a -> 'a)
      (onVar : 'a -> 'b -> int -> 'b expr)
      (start : 'a)
      (tm : 'b expr) : 'b expr =
    let rec go s = function
      | Var (v, inf) -> onVar s inf v
      | App ((rator, rand), inf) -> App ((go s rator, go s rand), inf)
      | Lam (body, inf) -> Lam(go (onBinder s) body, inf)
    in
    go start tm

  open QCheck.Gen
  let expr_gen =
    (* rec: depth -> ctxt_size -> expr Gen.t *)
    let rec gen depth scope =
      if depth <= 0 then                                                                        (* Base Case *)
          oneof [
            map (fun s -> var s) (int_range 0 scope);
          ]
        else
          oneof [
            QCheck.Gen.map2 (fun l r -> app l r)
              (gen (depth - 1) scope)
              (gen (depth - 1) scope);
             map (fun body -> lam body)
               (gen (depth - 1) (scope + 1));
          ]
      in
      map (fun g -> lam g) (gen 5 0)


  let rec rec_expr
            (onVar : info -> int -> 'a)
            (onApp : info -> 'a -> 'a -> 'a)
            (onLam : info -> 'a -> 'a)
            (tm : info expr) : 'a  =
    let go = rec_expr onVar onApp onLam in
    match tm with
    | Var (v, info) -> onVar info v
    | App((rator, rand), info) -> onApp info (go rator) (go rand)
    | Lam (body, info) -> onLam info (go body)


end

type raw_expr = info Raw.expr
[@@deriving show]

type 'a expr = 'a Debruijn.expr
[@@deriving show]

exception UnboundVariable of var * src_info

let rec index_of var ctx =
  match ctx with
  | [] -> None
  | h :: t -> if h = var then Some 0 else Option.map (fun i -> i + 1) (index_of var t)

let raw_to_debruijn (raw : raw_expr) : src_info expr * var list =
  let free_vars = ref [] in
  let add_free v =
    match index_of v !free_vars with
    | Some i -> i
    | None ->
        let i = List.length !free_vars in
        free_vars := !free_vars @ [v];
        i
  in
  let rec go ctx = function
    | Raw.Var (v, info) ->
        (match index_of v ctx with
         | Some i -> Debruijn.Var (i, info)
         | None ->
             let i = add_free v in
             Debruijn.Var (List.length ctx + i, info))
    | Raw.Lam ((v, body), info) ->
        Debruijn.Lam (go (v :: ctx) body, info)
    | Raw.App ((e1, e2), info) ->
        let e1' = go ctx e1 in
        let e2' = go ctx e2 in
        Debruijn.App ((e1', e2'), info)
  in
  let res = go [] raw in
  (res, !free_vars)

let rec pp_raw_expr (p : int) (fmt : Format.formatter) (raw : raw_expr) =
  match raw with
  | Raw.Var (v, _) -> Format.fprintf fmt "%s" v
  | Raw.App ((e1, e2), _) ->
      if p > 1 then Format.fprintf fmt "(";
      pp_raw_expr 1 fmt e1;
      Format.fprintf fmt " ";
      pp_raw_expr 2 fmt e2;
      if p > 1 then Format.fprintf fmt ")"
  | Raw.Lam ((v, body), _) ->
      if p > 0 then Format.fprintf fmt "(";
      Format.fprintf fmt "lambda %s. " v;
      pp_raw_expr 0 fmt body;
      if p > 0 then Format.fprintf fmt ")"

let string_of_raw_expr raw =
  Format.asprintf "%a" (pp_raw_expr 0) raw

let rec pp_expr (ctx : var list) (p : int) (fmt : Format.formatter) (tm : src_info expr) =
  match tm with
  | Debruijn.Var (i, _) ->
      let name = try List.nth ctx i with _ -> "x" ^ string_of_int i in
      Format.fprintf fmt "%s" name
  | Debruijn.App ((e1, e2), _) ->
      if p > 1 then Format.fprintf fmt "(";
      pp_expr ctx 1 fmt e1;
      Format.fprintf fmt " ";
      pp_expr ctx 2 fmt e2;
      if p > 1 then Format.fprintf fmt ")"
  | Debruijn.Lam (body, _) ->
      if p > 0 then Format.fprintf fmt "(";
      let v = "x" ^ string_of_int (List.length ctx) in
      Format.fprintf fmt "lambda %s. " v;
      pp_expr (v :: ctx) 0 fmt body;
      if p > 0 then Format.fprintf fmt ")"

let string_of_expr (ctx : var list) (tm : src_info expr) =
  Format.asprintf "%a" (pp_expr ctx 0) tm
