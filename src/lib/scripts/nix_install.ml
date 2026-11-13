open Genspio.EDSL

let main =
  let cli_spec =
    Command_line.Arg.(
      Script.dry_run_spec & Script.verbose_spec
      & Script.usage "nix-install" ~doc:"Install nix and related tooling")
  in
  Command_line.parse cli_spec (fun ~anon dry_run verbose ->
      let call = Script.call ~dry_run ~verbose in
      let tmp_nix_install = tmp_file "nix-install" in
      seq
        [
          call
            (strs [ "curl"; "-Lo" ]
            @ [ tmp_nix_install#path; str "https://nixos.org/nix/install" ]);
          call (strs [ "chmod"; "+x" ] @ [ tmp_nix_install#path ]);
          call [ tmp_nix_install#path; str "--no-daemon" ];
          tmp_nix_install#delete;
        ])
