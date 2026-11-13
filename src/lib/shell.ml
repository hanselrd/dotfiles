open Eio.Std
open Cli

let log_source ~verbosity ~label source =
  match verbosity with
  | Verbose -> (
      let buf = Eio.Buf_read.of_flow source ~max_size:(64 * 1024) in
      try
        while true do
          let line = Eio.Buf_read.line buf in
          Logs.debug (fun m -> m "%s= %s" label line)
        done
      with End_of_file -> ())
  | Normal | Quiet -> ()

let spawn ?cwd ?stdin
    (* ?stdout  *)
    (* ?stderr  *)
    ?env ?executable ~copts:{ Cli.dry_run; Cli.verbosity } ~sw process_mgr args
    =
  (match verbosity with
  | Normal | Verbose ->
      Logs.info (fun m ->
          m "shell.spawn%s= %s"
            (if dry_run then "(dry_run)" else "")
            (String.concat " " args))
  | Quiet -> ());

  if dry_run then None
  else
    let stdout_source, stdout_sink = Eio.Process.pipe process_mgr ~sw in
    let stderr_source, stderr_sink = Eio.Process.pipe process_mgr ~sw in

    let child =
      Eio.Process.spawn ?cwd ?stdin ~stdout:stdout_sink ~stderr:stderr_sink ?env
        ?executable ~sw process_mgr args
    in

    Eio.Flow.close stdout_sink;
    Eio.Flow.close stderr_sink;

    Fiber.all
      [
        (fun () -> log_source ~verbosity ~label:"stdout" stdout_source);
        (fun () -> log_source ~verbosity ~label:"stderr" stderr_source);
      ];

    Some child

let run ?cwd ?stdin
    (* ?stdout  *)
    (* ?stderr  *)
    ?is_success ?env ?executable ~copts:{ Cli.dry_run; Cli.verbosity }
    process_mgr args =
  (match verbosity with
  | Normal | Verbose ->
      Logs.info (fun m ->
          m "shell.run%s= %s"
            (if dry_run then "(dry_run)" else "")
            (String.concat " " args))
  | Quiet -> ());

  if not dry_run then
    Switch.run @@ fun sw ->
    let stdout_source, stdout_sink = Eio.Process.pipe process_mgr ~sw in
    let stderr_source, stderr_sink = Eio.Process.pipe process_mgr ~sw in

    Fiber.all
      [
        (fun () ->
          Fun.protect
            ~finally:(fun () ->
              Eio.Flow.close stdout_sink;
              Eio.Flow.close stderr_sink)
            (fun () ->
              Eio.Process.run ?cwd ?stdin ~stdout:stdout_sink
                ~stderr:stderr_sink ?is_success ?env ?executable process_mgr
                args));
        (fun () -> log_source ~verbosity ~label:"stdout" stdout_source);
        (fun () -> log_source ~verbosity ~label:"stderr" stderr_source);
      ]

let parse_out ?cwd ?stdin
    (* ?stderr  *)
    ?is_success ?env ?executable ~copts:{ Cli.dry_run; Cli.verbosity }
    process_mgr parser args =
  (match verbosity with
  | Normal | Verbose ->
      Logs.info (fun m ->
          m "shell.parse_out%s= %s"
            (if dry_run then "(dry_run)" else "")
            (String.concat " " args))
  | Quiet -> ());

  if dry_run then None
  else
    Switch.run @@ fun sw ->
    let stderr_source, stderr_sink = Eio.Process.pipe process_mgr ~sw in

    let out, _ =
      Fiber.pair
        (fun () ->
          Fun.protect
            ~finally:(fun () -> Eio.Flow.close stderr_sink)
            (fun () ->
              Eio.Process.parse_out ?cwd ?stdin ~stderr:stderr_sink ?is_success
                ?env ?executable process_mgr parser args))
        (fun () -> log_source ~verbosity ~label:"stderr" stderr_source)
    in

    Some out
