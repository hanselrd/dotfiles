open Genspio.EDSL

let main =
  let cli_spec =
    Command_line.Arg.(
      Script.dry_run_spec & Script.verbose_spec
      & string ~default:(str "bindmnt") ~doc:"Set type of chroot environment"
          [ "-t"; "--type" ]
      & string
          ~default:(home_path () /// str ".nix-chroot")
          ~doc:"Set install directory" [ "--install-dir" ]
      & Script.usage "nix-chroot" ~anon:[ "DIRECTORY" ]
          ~doc:
            "Create chroot environment in DIRECTORY to perform nix operations")
  in
  Command_line.parse cli_spec (fun ~anon dry_run verbose type' install_dir ->
      let call = Script.call ~dry_run ~verbose in
      let exec = Script.exec ~dry_run ~verbose in
      seq
        [
          Script.parse_anon anon;
          call (strs [ "mkdir"; "-p" ] @ [ install_dir ]);
          (let dir = getenv (str "ANON0") in
           if_then_else
             Str.(dir <$> str "")
             (switch
                [
                  case
                    Str.(type' =$= str "bindmnt")
                    (let user = getenv (str "USER") in
                     let nix_dir = str "/nix" in
                     [
                       call
                         (strs [ "sudo"; "mkdir"; "-pm"; "0755" ]
                         @ [ nix_dir; dir ]);
                       call (strs [ "sudo"; "chown" ] @ [ user; nix_dir; dir ]);
                       call
                         (strs [ "sudo"; "mount"; "--bind" ] @ [ dir; nix_dir ]);
                     ]);
                  case
                    Str.(type' =$= str "nix-user-chroot")
                    (let nix_user_chroot =
                       install_dir /// str "nix-user-chroot"
                     in
                     [
                       call
                         (strs [ "curl"; "-Lo" ]
                         @ [
                             nix_user_chroot;
                             str
                               "https://github.com/nix-community/nix-user-chroot/releases/download/2.1.1/nix-user-chroot-bin-2.1.1-x86_64-unknown-linux-musl";
                           ]);
                       call (strs [ "chmod"; "+x" ] @ [ nix_user_chroot ]);
                       call (strs [ "mkdir"; "-p" ] @ [ dir ]);
                       call ([ nix_user_chroot; dir ] @ strs [ "bash"; "-l" ]);
                     ]);
                  case
                    Str.(type' =$= str "proot")
                    (let proot = install_dir /// str "proot" in
                     [
                       call
                         (strs [ "curl"; "-Lo" ]
                         @ [
                             proot;
                             str "https://proot.gitlab.io/proot/bin/proot";
                           ]);
                       call (strs [ "chmod"; "+x" ] @ [ proot ]);
                       call (strs [ "mkdir"; "-p" ] @ [ dir ]);
                       call [ proot; str "-b"; dir ^$^ str ":/nix"; str "bash" ];
                     ]);
                  default
                    [
                      printf (str "failed to match on type: '%s'\n") [ type' ];
                      exit 1;
                    ];
                ])
             (seq [ exec [ "echo"; "DIRECTORY is required" ]; exit 1 ]));
        ])
