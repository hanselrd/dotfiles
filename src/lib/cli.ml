type verbosity = Normal | Quiet | Verbose [@@deriving show]
type copts = { dry_run : bool; verbosity : verbosity } [@@deriving show]

open Cmdliner
open Cmdliner.Term.Syntax

let copts_term =
  let docs = Manpage.s_common_options in

  let dry_run =
    let doc = "Perform a trial run without affecting environment" in
    Arg.(value & flag & info [ "n"; "dry-run" ] ~docs ~doc)
  in

  let verbosity =
    let doc = "Suppress informational output" in
    let quiet = (Quiet, Arg.info [ "q"; "quiet" ] ~docs ~doc) in

    let doc = "Give verbose output" in
    let verbose = (Verbose, Arg.info [ "v"; "verbose" ] ~docs ~doc) in

    Arg.(last & vflag_all [ Normal ] [ quiet; verbose ])
  in
  let+ dry_run and+ verbosity in
  { dry_run; verbosity }
