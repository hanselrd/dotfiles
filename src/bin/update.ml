open Eio.Std

let[@warning "-8"] update pool env
    (copts, Some (skip_nix_hashes, skip_git_hashes)) =
  Logs.debug (fun m -> m "copts= %s" (Dotfiles.Cli.show_copts copts));

  let process_mgr = Eio.Stdenv.process_mgr env in

  Switch.run @@ fun sw ->
  let git_grep_output =
    Eio.Executor_pool.submit_exn pool ~weight:1.0 (fun () ->
        Option.get
          (Dotfiles.Shell.parse_out ~copts process_mgr Eio.Buf_read.take_all
             [
               "git";
               "grep";
               "-nPo";
               {|version\s*=\s*.*;\s*#\s*github:.*/.*/.*$|};
             ]))
  in
  Logs.debug (fun m -> m "git_grep_output= %s" git_grep_output);

  let regex =
    Re.Perl.re {|(.*):(.*):version\s*=\s*(.*);\s*#\s*github:(.*)/(.*)/(.*)|}
    |> Re.compile
  in
  let matches = Re.all regex git_grep_output in
  let promises =
    List.map
      (fun m ->
        let file = Re.Group.get m 1 in
        let line_nr = int_of_string (Re.Group.get m 2) in
        let old_rev = Re.Group.get m 3 in
        let owner = Re.Group.get m 4 in
        let repo = Re.Group.get m 5 in
        let branch = Re.Group.get m 6 in
        Logs.debug (fun m ->
            m "file= %s line_nr= %d old_rev= %s owner= %s repo= %s branch= %s"
              file line_nr old_rev owner repo branch);
        Eio.Executor_pool.submit_fork ~sw pool ~weight:1.0 (fun () ->
            Switch.run @@ fun sw ->
            let pipe_source, pipe_sink = Eio.Process.pipe ~sw process_mgr in
            let _p1 =
              Eio.Process.spawn ~sw process_mgr
                [
                  "git";
                  "ls-remote";
                  Printf.sprintf "https://github.com/%s/%s.git" owner repo;
                  branch;
                ]
                ~stdout:pipe_sink
            in
            Eio.Flow.close pipe_sink;
            let new_rev =
              Option.get
                (Dotfiles.Shell.parse_out ~copts process_mgr Eio.Buf_read.line
                   [ "awk"; "{print $1}" ] ~stdin:pipe_source)
            in
            let new_rev = Printf.sprintf "%S" new_rev in
            Logs.debug (fun m -> m "new_rev= %s" new_rev);
            if (not skip_git_hashes) && old_rev <> new_rev then
              Dotfiles.Shell.run ~copts process_mgr
                [
                  "sed";
                  "-i";
                  Printf.sprintf "%ds/%s/%s/" line_nr old_rev new_rev;
                  file;
                ]))
      matches
  in
  List.iter Promise.await_exn promises;
  let cmds =
    List.map
      (fun c -> [ "nh"; "os"; "build"; "."; "-H"; c; "--impure"; "--no-nom" ])
      (Dotfiles.Nix.supported_nixos_configurations process_mgr)
    @ List.map
        (fun c ->
          [ "nh"; "darwin"; "build"; "."; "-H"; c; "--impure"; "--no-nom" ])
        (Dotfiles.Nix.supported_darwin_configurations process_mgr)
    @ List.map
        (fun c ->
          [ "nh"; "home"; "build"; "."; "-c"; c; "--impure"; "--no-nom" ])
        (Dotfiles.Nix.supported_home_configurations process_mgr)
  in
  let rec loop cmds =
    match cmds with
    | [] -> ()
    | x :: xs ->
        let stderr_buf = Buffer.create 1024 in
        let stderr_sink = Eio.Flow.buffer_sink stderr_buf in
        Eio.Process.run process_mgr x ~stderr:stderr_sink
          ~is_success:(fun code -> List.mem code [ 0; 1 ]);
        let output = Buffer.contents stderr_buf in
        Logs.debug (fun m -> m "output= %s" output);
        let regex =
          Re.Perl.re {|specified: (.*)\s*got:    (.*)\s|} |> Re.compile
        in
        let matches = Re.all regex output in
        List.iter
          (fun m ->
            let old_hash = Re.Group.get m 1 in
            let new_hash = Re.Group.get m 2 in
            Logs.debug (fun m ->
                m "old_hash= %s new_hash= %s" old_hash new_hash);
            Dotfiles.Shell.run ~copts process_mgr
              [ "git"; "sed"; old_hash; new_hash ])
          matches;
        if List.length matches <> 0 then (loop [@tailcall]) cmds
        else (loop [@tailcall]) xs
  in
  loop cmds

open Cmdliner
open Cmdliner.Term.Syntax

let cmd _ =
  let skip_nix_hashes =
    let doc = "Skip updating nix hashes" in
    Arg.(value & flag & info [ "N"; "skip-nix-hashes" ] ~doc)
  in

  let skip_git_hashes =
    let doc = "Skip updating git hashes" in
    Arg.(value & flag & info [ "G"; "skip-git-hashes" ] ~doc)
  in

  let+ skip_nix_hashes and+ skip_git_hashes in
  (skip_nix_hashes, skip_git_hashes)

let () =
  Dotfiles.Application.run "update" ~doc:"Update project" ~cmd:(`RootCmd cmd)
    update
