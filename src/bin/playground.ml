let () =
  Dotfiles.Application.run "playground" @@ fun pool env (copts, _) ->
  let process_mgr = Eio.Stdenv.process_mgr env in
  for _ = 1 to 5 do
    Dotfiles.Shell.run ~copts process_mgr
      [
        "sh";
        "-c";
        "echo 'Output line'; echo 'Error line' >&2; sleep 1; echo 'Done!'";
      ]
  done
