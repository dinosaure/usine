module type S = sig
  type t

  exception Error_output of (string * string)

  val make :
    requests:int ->
    clients:int ->
    ?kalive:bool ->
    Uri.t -> t

  val to_string : t -> string
  val compute : ?result:Result.t -> t -> Result.t Lwt.t
end
