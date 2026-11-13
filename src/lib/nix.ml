module Value = struct
  type t =
    | Int of int
    | Float of float
    | Bool of bool
    | String of string
    | Path of string
    | Null
    | List of t list
    | AttrSet of nv_pair list

  and nv_pair = { name : string; value : t } [@@deriving show]

  let rec to_string = function
    | Int i -> Printf.sprintf "%d" i
    | Float f -> Printf.sprintf "%f" f
    | Bool b -> Printf.sprintf "%B" b
    | String s -> Printf.sprintf "%S" s
    | Path p -> Printf.sprintf "%s" p
    | Null -> "null"
    | List l -> "[ " ^ String.concat " " (List.map to_string l) ^ " ]"
    | AttrSet a ->
        "{ "
        ^ String.concat " "
            (List.map
               (fun { name; value } ->
                 Printf.sprintf "%s = %s;" name (to_string value))
               a)
        ^ " }"
end

let system process_mgr =
  Eio.Process.parse_out process_mgr Eio.Buf_read.line
    [ "nix"; "eval"; "--expr"; "builtins.currentSystem"; "--impure"; "--raw" ]

let configurations name process_mgr =
  Eio.Switch.run @@ fun sw ->
  let pipe_source, pipe_sink = Eio.Process.pipe ~sw process_mgr in
  ignore
    (Eio.Process.spawn ~sw process_mgr
       [
         "nix";
         "eval";
         Printf.sprintf ".#%sConfigurations" name;
         "--apply";
         "x: builtins.attrNames x";
         "--json";
       ]
       ~stdout:pipe_sink);
  Eio.Flow.close pipe_sink;
  Eio.Process.parse_out process_mgr
    (fun buf -> Eio.Buf_read.lines buf |> List.of_seq)
    [ "jq"; "-r"; ".[]" ] ~stdin:pipe_source

let nixos_configurations process_mgr = configurations "nixos" process_mgr
let darwin_configurations process_mgr = configurations "darwin" process_mgr
let home_configurations process_mgr = configurations "home" process_mgr

let supported_configurations name process_mgr =
  let system = system process_mgr in
  List.filter_map
    (fun c ->
      let c_system =
        if List.mem name [ "nixos"; "darwin" ] then
          Eio.Process.parse_out process_mgr Eio.Buf_read.line
            [
              "nix";
              "eval";
              Printf.sprintf
                ".#%sConfigurations.\"%s\".config.nixpkgs.hostPlatform.system"
                name c;
              "--impure";
              "--raw";
            ]
        else
          Eio.Process.parse_out process_mgr Eio.Buf_read.line
            [
              "nix";
              "eval";
              Printf.sprintf ".#%sConfigurations.\"%s\".config.nixpkgs.system"
                name c;
              "--impure";
              "--raw";
            ]
      in
      if c_system = system then Some c else None)
    (configurations name process_mgr)

let supported_nixos_configurations process_mgr =
  supported_configurations "nixos" process_mgr

let supported_darwin_configurations process_mgr =
  supported_configurations "darwin" process_mgr

let supported_home_configurations process_mgr =
  supported_configurations "home" process_mgr

let fake_hash process_mgr fs clock =
  let temp_dir =
    match Sys.getenv_opt "XDG_RUNTIME_DIR" with
    | Some dir when not (String.is_empty dir) -> dir
    | _ -> Filename.get_temp_dir_name ()
  in
  let temp = Filename.temp_file ~temp_dir "fake-hash" "" in
  let temp_path = Eio.Path.(fs / temp) in
  let now = Eio.Time.now clock in
  Eio.Path.save ~create:(`Or_truncate 0o644) temp_path (Printf.sprintf "%f" now);
  Eio.Process.parse_out process_mgr Eio.Buf_read.line
    [ "nix"; "hash"; "file"; temp ]
