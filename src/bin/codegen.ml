let () =
  Dotfiles.Application.run "codegen" @@ fun pool env (copts, _) ->
  let scripts =
    [
      (Dotfiles.Scripts.Nix_config.main, "scripts/nix-config.sh");
      (Dotfiles.Scripts.Nix_chroot.main, "scripts/nix-chroot.sh");
      (Dotfiles.Scripts.Nix_install.main, "scripts/nix-install.sh")
      (* (Dotfiles_secrets.Scripts.Work_init.main, "secrets/modules/features/work/init2.sh") *);
    ]
  in
  List.iter
    (fun (src, dest) ->
      Logs.debug (fun m -> m "Writing script: dest= %s" dest);
      Out_channel.with_open_text dest (fun oc ->
          Out_channel.output_string oc "#!/bin/sh\n";
          let output_script ?no_trap () =
            Out_channel.output_string oc
              (Genspio.Compile.to_one_liner ?no_trap src)
          in
          try output_script ~no_trap:true ()
          with Failure _ -> output_script ());
      Unix.chmod dest 0o755)
    scripts
