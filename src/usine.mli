val compute :
  requests:int ->
  clients:int list ->
  kalive:bool ->
  uri: Uri.t ->
  int ->
  (Result.t list) Lwt.t

val to_csv : out_channel -> Result.t list  -> unit
