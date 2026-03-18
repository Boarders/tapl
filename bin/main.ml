open Untyped

let parse_string (s : string) : Syntax.raw_expr =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "[internal].tapl" };
  Parser.main Lexer.read lexbuf

let test input =
  Printf.printf "Input: %s\n" input;
  try
    let raw = parse_string input in
    (* Printf.printf "Raw AST: %s\n" (Syntax.show_raw_expr raw); *)
    let (debruijn, free_vars) = Syntax.raw_to_debruijn raw in
    (* Printf.printf "De Bruijn AST: %s\n" (Syntax.show_expr debruijn); *)
    Printf.printf "De Bruijn AST (pretty): %s\n" (Syntax.string_of_expr free_vars debruijn);
    let eval_debruijn = Substitution.whnf_eval debruijn in
    (* Printf.printf "De Bruijn evaluated: %s\n" (Syntax.show_expr eval_debruijn); *)
    Printf.printf "De Bruijn evaluated (pretty): %s\n" (Syntax.string_of_expr free_vars eval_debruijn);
    Printf.printf "\n"
  with
  | Syntax.UnboundVariable (v, info) ->
      Printf.printf "Error: Unbound variable %s at %s\n" v (Syntax.show_src_info info)
  | Failure msg ->
      Printf.printf "Error: %s\n" msg
  | e ->
    Printf.printf "An unexpected error occurred: %s\n" (Printexc.to_string e)

let () =
  test "lambda x. x";
  test "lambda x. lambda y. x y";
  test "(lambda x. x) (lambda y. y)";
  test "lambda x. x (lambda y. y) x";
  test "lambda x. lambda y. lambda z. x y z";
  test "lambda x. lambda y. lambda z. x (y z)";
  test "lambda x. lambda y. (lambda z. z x) y";
