open Eio.Std
open Cmdliner
open Cmdliner.Term.Syntax

let run ?cmd ?doc name f =
  Random.self_init ();

  Eio_main.run @@ fun env ->
  let stderr = Eio.Stdenv.stderr env in
  Log.setup stderr;

  Switch.run @@ fun sw ->
  let domain_count = Domain.recommended_domain_count () in
  Logs.debug @@ fun m ->
  m "APP= %s with domain(s)= %d" name domain_count;

  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let pool = Eio.Executor_pool.create ~sw ~domain_count domain_mgr in
  let version = Build.version () in
  let info = Cmd.info name ?doc ~version in

  let cmd =
    match cmd with
    | None ->
        Cmd.make info
        @@ let+ copts = Cli.copts_term in
           f pool env (copts, None)
    | Some (`RootCmd cmd) ->
        let cmd = cmd env in
        Cmd.make info
        @@ let+ copts = Cli.copts_term and+ args = cmd in
           f pool env (copts, Some args)
    | Some (`SubCmds cmds) ->
        let default =
          Term.(ret (const (fun _ -> `Help (`Pager, None)) $ Cli.copts_term))
        in
        Cmd.group info ~default
          (List.map
             (fun cmd ->
               let cmd, info = cmd env in
               Cmd.make info
               @@
               let+ copts = Cli.copts_term and+ args = cmd in
               f pool env (copts, Some args))
             cmds)
  in
  if !Sys.interactive then () else exit (Cmd.eval cmd)
