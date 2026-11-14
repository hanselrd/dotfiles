open Eio.Std

val spawn :
  ?cwd:Eio.Fs.dir_ty Eio.Path.t ->
  ?stdin:_ Eio.Flow.source ->
  ?stdout:_ Eio.Flow.sink ->
  ?stderr:_ Eio.Flow.sink ->
  ?env:Eio.Process.Env.t ->
  ?executable:string ->
  copts:Cli.copts ->
  sw:Switch.t ->
  [> ([> `Generic ] as 'a) Eio.Process.mgr_ty ] Eio.Resource.t ->
  string list ->
  'a Eio.Process.ty Eio.Resource.t option

val run :
  ?cwd:_ Eio.Path.t ->
  ?stdin:_ Eio.Flow.source ->
  ?stdout:_ Eio.Flow.sink ->
  ?stderr:_ Eio.Flow.sink ->
  ?is_success:(int -> bool) ->
  ?env:Eio.Process.Env.t ->
  ?executable:string ->
  copts:Cli.copts ->
  _ Eio.Process.mgr ->
  string list ->
  unit

val parse_out :
  ?cwd:_ Eio.Path.t ->
  ?stdin:_ Eio.Flow.source ->
  ?stderr:_ Eio.Flow.sink ->
  ?is_success:(int -> bool) ->
  ?env:Eio.Process.Env.t ->
  ?executable:string ->
  copts:Cli.copts ->
  _ Eio.Process.mgr ->
  'a Eio.Buf_read.parser ->
  string list ->
  'a option
