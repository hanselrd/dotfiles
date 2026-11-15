open Dotfiles

let render = Nix.Value.to_string

let test_int () =
  Alcotest.(check string) "integer" "42" (render (Nix.Value.Int 42))

let test_float () =
  Alcotest.(check string) "float" "1.500000" (render (Nix.Value.Float 1.5))

let test_bool () =
  Alcotest.(check string) "boolean" "true" (render (Nix.Value.Bool true))

let test_string () =
  Alcotest.(check string)
    "string is quoted" "\"hello\""
    (render (Nix.Value.String "hello"))

let test_null () = Alcotest.(check string) "null" "null" (render Nix.Value.Null)

let test_list () =
  let value =
    Nix.Value.List [ Nix.Value.Int 1; Nix.Value.String "two"; Nix.Value.Null ]
  in
  Alcotest.(check string) "list" "[ 1 \"two\" null ]" (render value)

let test_attrset () =
  let value =
    Nix.Value.AttrSet
      [
        { Nix.Value.name = "answer"; value = Nix.Value.Int 42 };
        { name = "enabled"; value = Nix.Value.Bool true };
      ]
  in
  Alcotest.(check string)
    "attributes preserve order" "{ answer = 42; enabled = true; }"
    (render value)

let test_run_drains_large_output () =
  Eio_main.run @@ fun env ->
  let process_mgr = Eio.Stdenv.process_mgr env in
  let copts = { Cli.dry_run = false; verbosity = Cli.Normal } in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5.0 @@ fun () ->
  Shell.run ~copts process_mgr [ "head"; "-c"; "1000000"; "/dev/zero" ]

let () =
  Alcotest.run "dotfiles"
    [
      ( "Nix.Value.to_string",
        [
          Alcotest.test_case "integer" `Quick test_int;
          Alcotest.test_case "float" `Quick test_float;
          Alcotest.test_case "boolean" `Quick test_bool;
          Alcotest.test_case "string" `Quick test_string;
          Alcotest.test_case "null" `Quick test_null;
          Alcotest.test_case "list" `Quick test_list;
          Alcotest.test_case "attribute set" `Quick test_attrset;
        ] );
      ( "Shell.run",
        [
          Alcotest.test_case "drains large output without deadlocking" `Quick
            test_run_drains_large_output;
        ] );
    ]
