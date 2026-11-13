type domain_logger = {
  buf : Buffer.t;
  fmt : Format.formatter;
  base_reporter : Logs.reporter;
}

let pp_domain_header ppf (level, header) =
  let exec =
    match Array.length Sys.argv with
    | 0 -> Filename.basename Sys.executable_name
    | n -> Filename.basename Sys.argv.(0)
  in
  let domain_id = (Domain.self () :> int) in

  Format.fprintf ppf "%s(%02d): " exec domain_id;
  Logs_fmt.pp_header ppf (level, header);
  Format.fprintf ppf " "

let reporter sink =
  let create_logger () =
    let buf = Buffer.create 512 in
    let fmt = Format.formatter_of_buffer buf in
    Fmt.set_style_renderer fmt (Fmt.style_renderer Format.std_formatter);
    let base_reporter =
      Logs_fmt.reporter ~pp_header:pp_domain_header ~app:fmt ~dst:fmt ()
    in
    { buf; fmt; base_reporter }
  in

  let logger_key = Domain.DLS.new_key create_logger in

  let report src level ~over k msgf =
    let logger = Domain.DLS.get logger_key in

    let k () =
      Format.pp_print_flush logger.fmt ();
      let msg = Buffer.contents logger.buf in
      Buffer.reset logger.buf;
      Eio.Flow.copy_string msg sink;
      k ()
    in

    logger.base_reporter.report src level ~over k msgf
  in
  { Logs.report }

let setup ?(level = Logs.Debug) sink =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (reporter sink);
  Logs.set_level (Some level)
