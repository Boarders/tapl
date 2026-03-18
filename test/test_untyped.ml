open Untyped.Syntax
open Untyped.Substitution

let dummy_info = {
  start_pos = Lexing.dummy_pos;
  end_pos = Lexing.dummy_pos;
}

let rec church_of_int n =
  let rec body n =
    if n = 0 then Debruijn.Var (0, dummy_info)
    else Debruijn.App ((Debruijn.Var (1, dummy_info), body (n - 1)), dummy_info)
  in
  Debruijn.Lam (Debruijn.Lam (body n, dummy_info), dummy_info)

let plus =
  let open Debruijn in
  Lam ( (* m *)
    Lam ( (* n *)
      Lam ( (* f *)
        Lam ( (* x *)
          App (
            (App ((Var (3, dummy_info), Var (1, dummy_info)), dummy_info),
             App ((App ((Var (2, dummy_info), Var (1, dummy_info)), dummy_info),
                   Var (0, dummy_info)), dummy_info)),
            dummy_info
          ),
          dummy_info
        ),
        dummy_info
      ),
      dummy_info
    ),
    dummy_info
  )

let test_plus m n expected =
  let m_church = church_of_int m in
  let n_church = church_of_int n in
  let expected_church : src_info expr = church_of_int expected in
  let term : src_info expr = Debruijn.App ((Debruijn.App ((plus, m_church), dummy_info), n_church), dummy_info) in
  let result : src_info expr = normalize term in
  if result = expected_church then
    Printf.printf "Test %d + %d = %d passed\n" m n expected
  else
    (Printf.printf "Test %d + %d = %d failed\n" m n expected;
     Printf.printf "Expected: %s\n" (show_expr pp_src_info expected_church);
     Printf.printf "Got: %s\n" (show_expr pp_src_info result);
     exit 1)

let () =
  print_endline "Running Peano tests...";
  test_plus 1 1 2;
  test_plus 2 2 4;
  print_endline "Peano tests passed!"


open QCheck
open Untyped.Syntax
open Untyped.Nbe
module S = Untyped.Syntax

let print_unit_expr e =
  string_of_expr [] (Debruijn.map_expr (fun () -> dummy_info) e)

let prop_normalize_equal (e : unit expr) =
  let lhs = Untyped.Nbe.normalize e in
  let rhs = Debruijn.map_expr (fun _ -> ()) (Untyped.Substitution.normalize e) in
  if lhs = rhs then true
  else QCheck.Test.fail_reportf "nbe:  %s\nstep: %s"
    (print_unit_expr lhs)
    (print_unit_expr rhs)

let test_rev_rev =
 Test.make
   ~name:"nbe_equals_step_normalize"         (* Name shown in output *)
     ~count:100
     (QCheck.make
        ~print:(fun e -> string_of_expr [] (Debruijn.map_expr (fun () -> dummy_info) e))
        (Debruijn.expr_gen))
     prop_normalize_equal

 (* 3. Run the test suite *)
 let () =
   QCheck_runner.run_tests_main [test_rev_rev]
