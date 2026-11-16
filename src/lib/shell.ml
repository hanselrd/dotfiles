open Eio.Std
open Cli

let drain source =
  let buf = Cstruct.create 4096 in
  try
    while true do
      ignore (Eio.Flow.single_read source buf)
    done
  with End_of_file -> ()

let log_source ~verbosity ~label source =
  match verbosity with
  | Normal | Quiet -> drain source
  | Verbose -> (
      let buf = Eio.Buf_read.of_flow source ~max_size:(64 * 1024) in
      try
        while true do
          let line = Eio.Buf_read.line buf in
          Logs.debug (fun m -> m "%s= %s" label line)
        done
      with
      | End_of_file -> ()
      | Eio.Buf_read.Buffer_limit_exceeded ->
          Logs.debug (fun m -> m "%s= (line too long; draining)" label);
          drain source)

let combine_sinks pipe user =
  let module Sink = struct
    type t = unit

    let single_write () bufs =
      Eio.Flow.write pipe bufs;
      Option.iter (fun sink -> Eio.Flow.write sink bufs) user;
      Cstruct.lenv bufs

    let copy () ~src =
      Fun.protect
        ~finally:(fun () -> Eio.Flow.close pipe)
        (fun () -> Eio.Flow.Pi.simple_copy ~single_write () ~src)
  end in
  Eio.Resource.T ((), Eio.Flow.Pi.sink (module Sink))

let spawn ?cwd ?stdin ?stdout ?stderr ?env ?executable ?(tty = false)
    ~copts:{ Cli.dry_run; Cli.verbosity } ~sw process_mgr args =
  (match verbosity with
  | Normal | Verbose ->
      Logs.info (fun m ->
          m "shell.spawn%s= %s"
            (if dry_run then "(dry_run)" else "")
            (String.concat " " args))
  | Quiet -> ());

  if dry_run then None
  else if tty then
    Some
      (Eio.Process.spawn ?cwd ?stdin ?stdout ?stderr ?env ?executable ~sw
         process_mgr args)
  else
    let stdout_source, stdout_sink = Eio.Process.pipe process_mgr ~sw in
    let stderr_source, stderr_sink = Eio.Process.pipe process_mgr ~sw in

    let child =
      Eio.Process.spawn ?cwd ?stdin
        ~stdout:(combine_sinks stdout_sink stdout)
        ~stderr:(combine_sinks stderr_sink stderr)
        ?env ?executable ~sw process_mgr args
    in

    Fiber.all
      [
        (fun () -> log_source ~verbosity ~label:"stdout" stdout_source);
        (fun () -> log_source ~verbosity ~label:"stderr" stderr_source);
      ];

    Some child

let run ?cwd ?stdin ?stdout ?stderr ?is_success ?env ?executable ?(tty = false)
    ~copts:{ Cli.dry_run; Cli.verbosity } process_mgr args =
  (match verbosity with
  | Normal | Verbose ->
      Logs.info (fun m ->
          m "shell.run%s= %s"
            (if dry_run then "(dry_run)" else "")
            (String.concat " " args))
  | Quiet -> ());

  if not dry_run then
    if tty then
      Eio.Process.run ?cwd ?stdin ?stdout ?stderr ?is_success ?env ?executable
        process_mgr args
    else
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
                Eio.Process.run ?cwd ?stdin
                  ~stdout:(combine_sinks stdout_sink stdout)
                  ~stderr:(combine_sinks stderr_sink stderr)
                  ?is_success ?env ?executable process_mgr args));
          (fun () -> log_source ~verbosity ~label:"stdout" stdout_source);
          (fun () -> log_source ~verbosity ~label:"stderr" stderr_source);
        ]

let parse_out ?cwd ?stdin ?stderr ?is_success ?env ?executable ?(tty = false)
    ~copts:{ Cli.dry_run; Cli.verbosity } process_mgr parser args =
  (match verbosity with
  | Normal | Verbose ->
      Logs.info (fun m ->
          m "shell.parse_out%s= %s"
            (if dry_run then "(dry_run)" else "")
            (String.concat " " args))
  | Quiet -> ());

  if dry_run then None
  else if tty then
    Some
      (Eio.Process.parse_out ?cwd ?stdin ?stderr ?is_success ?env ?executable
         process_mgr parser args)
  else
    Switch.run @@ fun sw ->
    let stderr_source, stderr_sink = Eio.Process.pipe process_mgr ~sw in

    let out, _ =
      Fiber.pair
        (fun () ->
          Fun.protect
            ~finally:(fun () -> Eio.Flow.close stderr_sink)
            (fun () ->
              Eio.Process.parse_out ?cwd ?stdin
                ~stderr:(combine_sinks stderr_sink stderr)
                ?is_success ?env ?executable process_mgr parser args))
        (fun () -> log_source ~verbosity ~label:"stderr" stderr_source)
    in

    Some out
