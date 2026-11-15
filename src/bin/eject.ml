open Eio.Std

let[@warning "-8"] main pool env (copts, Some (home, output)) =
  Logs.debug (fun m -> m "copts= %s" (Dotfiles.Cli.show_copts copts));

  let process_mgr = Eio.Stdenv.process_mgr env in
  let home_dir = Option.get (Eio.Process.getenv_opt process_mgr "HOME") in

  let fs = Eio.Stdenv.fs env in

  if not (List.mem home (Dotfiles.Nix.home_configurations process_mgr)) then
    Printf.ksprintf failwith "%s is not a valid home configuration" home;

  let path0 =
    Option.get
      (Dotfiles.Shell.parse_out ~copts process_mgr Eio.Buf_read.line
         [
           "nix";
           "build";
           "--no-link";
           "--print-out-paths";
           Printf.sprintf ".#homeConfigurations.%S.activationPackage" home;
           "--impure";
         ])
  in
  Logs.debug (fun m -> m "path0= %s" path0);

  let path1 =
    Option.get
      (Dotfiles.Shell.parse_out ~copts process_mgr Eio.Buf_read.line
         [ "readlink"; "-f"; home_dir ^ "/.nix-profile" ])
  in
  Logs.debug (fun m -> m "path1= %s" path1);

  let nix_store_prefix = "/nix/store/" in
  let sed_string =
    Printf.sprintf "s@%s.{%d}@%s/@" nix_store_prefix
      (String.length output - String.length nix_store_prefix + 1)
      output
  in

  let deps =
    Option.get
      (Dotfiles.Shell.parse_out ~copts process_mgr
         (fun buf -> Eio.Buf_read.lines buf |> List.of_seq)
         [ "nix-store"; "-qR"; path0; path1 ])
  in

  Switch.run @@ fun sw ->
  let temp_dir = Filename.temp_dir "eject-deps" "" in
  let promises =
    List.map
      (fun dep ->
        let temp_dep =
          Printf.sprintf "%s/%s" temp_dir (Filename.basename dep)
        in
        let temp_dep_tar = temp_dep ^ ".tar" in
        Eio.Executor_pool.submit_fork ~sw pool ~weight:1.0 (fun () ->
            Logs.debug (fun m -> m "dep= %s" dep);
            Dotfiles.Shell.run ~copts process_mgr
              [
                "tar";
                "--hard-dereference";
                "-cPf";
                temp_dep_tar;
                Printf.sprintf "--transform=%sx" sed_string;
                "--mode=u+w";
                dep;
              ];
            Dotfiles.Shell.run ~copts process_mgr
              [ "tar"; "-xf"; temp_dep_tar; "--one-top-level"; "-C"; temp_dir ];
            Eio.Path.unlink Eio.Path.(fs / temp_dep_tar);
            Dotfiles.Shell.run ~copts process_mgr
              [
                "find";
                temp_dep;
                "-type";
                "f";
                "-exec";
                "sed";
                "-i";
                "-E";
                sed_string ^ "g";
                "{}";
                "+";
              ];
            Dotfiles.Shell.run ~copts process_mgr
              [
                "tar";
                "-cPf";
                temp_dep_tar;
                Printf.sprintf "--transform=s@%s@@" temp_dep;
                temp_dep ^ output;
              ];
            Eio.Path.rmtree Eio.Path.(fs / temp_dep)))
      deps
  in
  List.iter Promise.await_exn promises;

  let temp_file = Filename.temp_file "eject-deps" ".tar" in
  Dotfiles.Shell.run ~copts process_mgr
    [ "tar"; "-cf"; temp_file; "-T"; "/dev/null" ];

  List.iter
    (fun dep ->
      let temp_dep = Printf.sprintf "%s/%s" temp_dir (Filename.basename dep) in
      let temp_dep_tar = temp_dep ^ ".tar" in
      Dotfiles.Shell.run ~copts process_mgr
        [ "tar"; "-nAf"; temp_file; temp_dep_tar ];
      Eio.Path.unlink Eio.Path.(fs / temp_dep_tar))
    deps;

  Eio.Path.rmtree Eio.Path.(fs / temp_dir);
  Dotfiles.Shell.run ~copts process_mgr [ "zstdmt"; "--rm"; temp_file ];

  Dotfiles.Shell.run ~copts process_mgr [ "tar"; "-xPf"; temp_file ^ ".zst" ];

  let path0_new =
    Option.get
      (Dotfiles.Shell.parse_out ~copts process_mgr Eio.Buf_read.line
         [ "sh"; "-c"; Printf.sprintf "echo %S | sed -E %S" path0 sed_string ])
  in
  Logs.debug (fun m -> m "path0_new= %s" path0_new);

  let path1_new =
    Option.get
      (Dotfiles.Shell.parse_out ~copts process_mgr Eio.Buf_read.line
         [ "sh"; "-c"; Printf.sprintf "echo %S | sed -E %S" path1 sed_string ])
  in
  Logs.debug (fun m -> m "path1_new= %s" path1_new);

  Dotfiles.Shell.run ~copts process_mgr
    [ "cp"; "-a"; path0_new ^ "/home-files/."; home_dir ^ "/" ];

  Dotfiles.Shell.run ~copts process_mgr
    [ "ln"; "-snfF"; path1_new; home_dir ^ "/.nix-profile" ]

open Cmdliner
open Cmdliner.Term.Syntax

let cmd env =
  let process_mgr = Eio.Stdenv.process_mgr env in

  let home =
    let doc = "Home configuration" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"HOME")
  in

  let output =
    let doc = "Output directory" in
    let default =
      Printf.sprintf "%s/.nix/_/%s"
        (Option.get (Eio.Process.getenv_opt process_mgr "HOME"))
        (Eio.Process.parse_out process_mgr Eio.Buf_read.line
           [ "git"; "rev-parse"; "--short=6"; "HEAD" ])
    in
    Arg.(
      value & opt string default & info [ "o"; "output" ] ~doc ~docv:"OUTPUT")
  in

  let+ home and+ output in
  (home, output)

let () =
  Dotfiles.Application.run "eject"
    ~doc:"Eject home configuration to output directory" ~cmd:(`RootCmd cmd) main
