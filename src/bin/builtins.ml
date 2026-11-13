type builtin =
  | RandomString of { length : int }
  | DevicePartition of { file : string }
  | Device of { file : string }
  | DecryptSecret of { identity : string; secret : string }
[@@deriving show]

let rec builtin_task copts builtin env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  match builtin with
  | RandomString { length } ->
      let alphabet =
        List.init 26 (fun i -> Char.chr (Char.code 'a' + i))
        @ List.init 10 (fun i -> Char.chr (Char.code '0' + i))
        |> List.to_seq |> String.of_seq
      in
      Dotfiles.Nix.Value.String
        (String.init length (fun _ ->
             alphabet.[Random.int (String.length alphabet)]))
  | DevicePartition { file } ->
      Dotfiles.Nix.Value.String
        (Option.get
           (Dotfiles.Shell.parse_out ~copts process_mgr Eio.Buf_read.line
              [ "findmnt"; "-no"; "source"; "-T"; file ]))
  | Device { file } ->
      let[@warning "-8"] (Dotfiles.Nix.Value.String device_partition) =
        builtin_task copts (DevicePartition { file }) env
      in
      Dotfiles.Nix.Value.String
        (Option.get
           (Dotfiles.Shell.parse_out ~copts process_mgr Eio.Buf_read.line
              [ "lsblk"; "-dpno"; "pkname"; device_partition ]))
  | DecryptSecret { identity; secret } ->
      let temp_dir =
        match Sys.getenv_opt "XDG_RUNTIME_DIR" with
        | Some dir when not (String.is_empty dir) -> dir
        | _ -> Filename.get_temp_dir_name ()
      in
      let temp = Filename.temp_file ~temp_dir "decrypt-secret" "" in
      Dotfiles.Shell.run ~copts process_mgr
        [ "age"; "-d"; "-i"; identity; "-o"; temp; secret ];
      Dotfiles.Nix.Value.Path temp

let[@warning "-8"] builtin pool env (copts, Some builtin) =
  Logs.debug (fun m -> m "builtin= %s" (show_builtin builtin));
  let value =
    Eio.Executor_pool.submit_exn pool ~weight:1.0 (fun () ->
        builtin_task copts builtin env)
  in
  Logs.debug (fun m -> m "nix_value= %s" (Dotfiles.Nix.Value.show value));
  let stdout = Eio.Stdenv.stdout env in
  Eio.Flow.copy_string (Dotfiles.Nix.Value.to_string value) stdout

open Cmdliner
open Cmdliner.Term.Syntax

let random_string_cmd _ =
  let length =
    let doc = "Length of random string" in
    Arg.(required & pos 0 (some int) None & info [] ~doc ~docv:"LENGTH")
  in
  let doc = "Generate random string of specified length" in
  let info = Cmd.info "random-string" ~doc in
  ( (let+ length in
     RandomString { length }),
    info )

let device_partition_cmd _ =
  let file =
    let doc = "File to get device partition" in
    Arg.(required & pos 0 (some filepath) None & info [] ~doc ~docv:"FILE")
  in
  let doc = "Device partition of file" in
  let info = Cmd.info "device-partition" ~doc in
  ( (let+ file in
     DevicePartition { file }),
    info )

let device_cmd _ =
  let file =
    let doc = "File to get device" in
    Arg.(required & pos 0 (some filepath) None & info [] ~doc ~docv:"FILE")
  in
  let doc = "Device of file" in
  let info = Cmd.info "device" ~doc in
  ( (let+ file in
     Device { file }),
    info )

let decrypt_secret_cmd _ =
  let identity =
    let doc = "Identity to decrypt secret" in
    Arg.(required & pos 0 (some filepath) None & info [] ~doc ~docv:"IDENTITY")
  in
  let secret =
    let doc = "Secret to decrypt" in
    Arg.(required & pos 1 (some filepath) None & info [] ~doc ~docv:"SECRET")
  in
  let doc = "Decrypt secret" in
  let info = Cmd.info "decrypt-secret" ~doc in
  ( (let+ identity and+ secret in
     DecryptSecret { identity; secret }),
    info )

let () =
  Dotfiles.Application.run "builtins" ~doc:"Builtins for nix"
    ~cmd:
      (`SubCmds
         [
           random_string_cmd;
           device_partition_cmd;
           device_cmd;
           decrypt_secret_cmd;
         ])
    builtin
