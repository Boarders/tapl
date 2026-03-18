%{
  open Syntax
%}

%token <string> VAR
%token LAMBDA DOT
%token LPAREN RPAREN
%token EOF

%start <Syntax.raw_expr> main

%%

main:
  | e = expr EOF { e }

expr:
  | e = app_expr { e }
  | LAMBDA v = VAR DOT e = expr
    {
      let start_pos = $startpos in
      let end_pos = $endpos in
      Raw.Lam ((v, e), { start_pos; end_pos })
    }

app_expr:
  | e1 = app_expr e2 = atomic_expr
    {
      let start_pos = $startpos in
      let end_pos = $endpos in
      Raw.App ((e1, e2), { start_pos; end_pos })
    }
  | e = atomic_expr { e }

atomic_expr:
  | LPAREN e = expr RPAREN { e }
  | v = VAR
    {
      let start_pos = $startpos in
      let end_pos = $endpos in
      Raw.Var (v, { start_pos; end_pos })
    }
