open Genspio.EDSL

let call ?(dry_run = bool false) ?(verbose = bool false) l =
  seq
    [ if_then verbose (call (str "echo" :: l)); if_then (not dry_run) (call l) ]

let exec ?dry_run ?verbose l = call ?dry_run ?verbose (strs l)

let dry_run_spec =
  Command_line.Arg.(
    flag ~doc:"Perform a trial run without making changes" [ "-n"; "--dry-run" ])

let verbose_spec =
  Command_line.Arg.(flag ~doc:"Enable verbose logging" [ "-v"; "--verbose" ])

let usage ?(anon = []) ?(doc = "") name =
  Command_line.Arg.usage
    (Printf.sprintf "Usage: %s [OPTION]...%s%s" name
       (String.concat ""
          (List.map
             (fun anon ->
               if Stdlib.not (String.is_empty anon) then " [" ^ anon ^ "]"
               else anon)
             anon))
       (if Stdlib.not (String.is_empty doc) then "\n" ^ doc else doc))

let parse_anon anon =
  let counter = tmp_file "counter" in
  seq
    [
      counter#set (Integer.to_str (int 0));
      Elist.iter anon ~f:(fun current ->
          seq [ setenv ~var:(str "ANON" ^$^ counter#get) (current ()) ]);
      counter#set Integer.(to_str (of_str counter#get + int 1));
      counter#delete;
    ]
