type git_alias =
  | SmartClone of { url : string }
  | SmartWorktreeAdd of { branch : string; new_branch : string option }
  | SmartWorktreeRemove of { branch : string }
  | Audit
  | Hydrate
  | Maintenance
[@@deriving show]

let worktree_directory branch =
  String.map (fun c -> if Char.equal c '/' then '-' else c) branch

let task copts git_alias env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let cwd = Eio.Stdenv.cwd env in

  match git_alias with
  | SmartClone { url } ->
      let directory = Filename.remove_extension (Filename.basename url) in
      if not copts.Dotfiles.Cli.dry_run then
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / directory);
      Dotfiles.Shell.run ~copts process_mgr
        [
          "git";
          "clone";
          "--bare";
          "--single-branch";
          "--filter=blob:none";
          url;
          Filename.concat directory ".bare";
        ];
      if not copts.Dotfiles.Cli.dry_run then
        Eio.Path.save ~create:(`Or_truncate 0o644)
          Eio.Path.(fs / directory / ".git")
          "gitdir: ./.bare\n";
      Dotfiles.Shell.run ~copts process_mgr
        ~is_success:(fun _ -> true)
        [
          "git"; "-C"; directory; "config"; "--unset-all"; "remote.origin.fetch";
        ]
  | SmartWorktreeAdd { branch; new_branch } -> (
      match new_branch with
      | None ->
          let directory = worktree_directory branch in
          Dotfiles.Shell.run ~copts process_mgr
            [
              "git";
              "fetch";
              "origin";
              Printf.sprintf "%s:refs/remotes/origin/%s" branch branch;
            ];
          Dotfiles.Shell.run ~copts process_mgr
            ~is_success:(fun _ -> true)
            [ "git"; "branch"; branch; "origin/" ^ branch; "--track" ];
          Dotfiles.Shell.run ~copts process_mgr
            [ "git"; "remote"; "set-branches"; "--add"; "origin"; branch ];
          Dotfiles.Shell.run ~copts process_mgr
            [ "git"; "worktree"; "add"; directory; branch ]
      | Some new_branch ->
          let directory = worktree_directory new_branch in
          Dotfiles.Shell.run ~copts process_mgr
            ~is_success:(fun _ -> true)
            [
              "git";
              "fetch";
              "origin";
              Printf.sprintf "%s:refs/remotes/origin/%s" branch new_branch;
            ];
          let origin_branch_exists =
            match
              Dotfiles.Shell.parse_out ~copts process_mgr Eio.Buf_read.take_all
                ~is_success:(fun _ -> true)
                [ "git"; "rev-parse"; "--verify"; "origin/" ^ branch ]
            with
            | Some output -> not (String.is_empty (String.trim output))
            | None -> false
          in
          Dotfiles.Shell.run ~copts process_mgr
            [
              "git";
              "branch";
              new_branch;
              (if origin_branch_exists then "origin/" ^ branch else branch);
            ];
          Dotfiles.Shell.run ~copts process_mgr
            [ "git"; "remote"; "set-branches"; "--add"; "origin"; new_branch ];
          Dotfiles.Shell.run ~copts process_mgr
            [ "git"; "worktree"; "add"; directory; new_branch ];
          Dotfiles.Shell.run ~copts process_mgr
            ~is_success:(fun _ -> true)
            [
              "git";
              "-C";
              directory;
              "--set-upstream-to=origin/" ^ new_branch;
              "branch";
              new_branch;
            ])
  | SmartWorktreeRemove { branch } ->
      let directory = worktree_directory branch in
      Eio.Path.rmtree Eio.Path.(cwd / directory);
      Dotfiles.Shell.run ~copts process_mgr [ "git"; "worktree"; "prune" ];
      Dotfiles.Shell.run ~copts process_mgr
        ~is_success:(fun _ -> true)
        [ "git"; "branch"; "-D"; branch ];
      Dotfiles.Shell.run ~copts process_mgr
        ~is_success:(fun _ -> true)
        [
          "git";
          "config";
          "--unset";
          "remote.origin.fetch";
          Printf.sprintf "refs/heads/%s:refs/remotes/origin/%s" branch branch;
        ];
      Dotfiles.Shell.run ~copts process_mgr
        ~is_success:(fun _ -> true)
        [
          "git";
          "config";
          "--unset";
          "remote.origin.fetch";
          Printf.sprintf "+refs/heads/%s:refs/remotes/origin/%s" branch branch;
        ];
      Dotfiles.Shell.run ~copts process_mgr [ "git"; "repack"; "-d"; "-l" ];
      Dotfiles.Shell.run ~copts process_mgr [ "git"; "prune"; "--expire=now" ]
  | Audit ->
      Dotfiles.Shell.run ~copts process_mgr
        [ "git"; "count-objects"; "--verbose"; "--human-readable" ]
  | Hydrate ->
      Dotfiles.Shell.run ~copts ~tty:true process_mgr
        [ "git"; "fetch"; "origin"; "--no-auto-gc"; "--prune"; "--refetch" ]
  | Maintenance ->
      Dotfiles.Shell.run ~copts process_mgr [ "git"; "hydrate" ];
      Dotfiles.Shell.run ~copts process_mgr [ "git"; "worktree"; "prune" ];
      Dotfiles.Shell.run ~copts process_mgr
        [
          "git";
          "reflog";
          "expire";
          "--expire=now";
          "--expire-unreachable=now";
          "--all";
        ];
      Dotfiles.Shell.run ~copts process_mgr
        [ "git"; "gc"; "--prune=now"; "--aggressive" ];
      Dotfiles.Shell.run ~copts process_mgr [ "git"; "fsck" ]

let[@warning "-8"] main pool env (copts, Some git_alias) =
  Logs.debug (fun m -> m "git_alias= %s" (show_git_alias git_alias));
  Eio.Executor_pool.submit_exn pool ~weight:1.0 (fun () ->
      task copts git_alias env)

open Cmdliner
open Cmdliner.Term.Syntax

let smart_clone_cmd _ =
  let url =
    let doc = "Repository URL to clone" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"URL")
  in
  let doc = "Clone repository with smart defaults" in
  let info = Cmd.info "smart-clone" ~doc in
  ( (let+ url in
     SmartClone { url }),
    info )

let smart_worktree_add_cmd _ =
  let branch =
    let doc = "Branch to add worktree for" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"BRANCH")
  in
  let new_branch =
    let doc = "New branch to create from BRANCH" in
    Arg.(
      value
      & opt (some string) None
      & info [ "b"; "new-branch" ] ~doc ~docv:"BRANCH")
  in
  let doc = "Add worktree, optionally creating a new branch" in
  let info = Cmd.info "smart-worktree-add" ~doc in
  ( (let+ branch and+ new_branch in
     SmartWorktreeAdd { branch; new_branch }),
    info )

let smart_worktree_remove_cmd _ =
  let branch =
    let doc = "Branch to remove worktree for" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"BRANCH")
  in
  let doc = "Remove worktree and its branch" in
  let info = Cmd.info "smart-worktree-remove" ~doc in
  ( (let+ branch in
     SmartWorktreeRemove { branch }),
    info )

let audit_cmd _ =
  let doc = "Audit repository object database" in
  let info = Cmd.info "audit" ~doc in
  (Term.const Audit, info)

let hydrate_cmd _ =
  let doc = "Hydrate partial clone" in
  let info = Cmd.info "hydrate" ~doc in
  (Term.const Hydrate, info)

let maintenance_cmd _ =
  let doc = "Run repository maintenance" in
  let info = Cmd.info "maintenance" ~doc in
  (Term.const Maintenance, info)

let () =
  Dotfiles.Application.run "git-alias" ~doc:"Git aliases"
    ~cmd:
      (`SubCmds
         [
           smart_clone_cmd;
           smart_worktree_add_cmd;
           smart_worktree_remove_cmd;
           audit_cmd;
           hydrate_cmd;
           maintenance_cmd;
         ])
    main
