type t

val make :
  ?complete_requests:int ->
  ?failed_requests:int ->
  ?requests_per_second:int ->
  ?time_per_request:float ->
  ?transfer_rate:float ->
  ?request_rate:float ->
  ?reply_status:(int * int * int * int * int) ->
  ?requests_status:(int * int * int) ->
  unit -> t

val update :
  t ->
  ?complete_requests:int ->
  ?failed_requests:int ->
  ?requests_per_second:int ->
  ?time_per_request:float ->
  ?transfer_rate:float ->
  ?request_rate:float ->
  ?reply_status:(int * int * int * int * int) ->
  ?requests_status:(int * int * int) ->
  unit -> t

val merge : t -> t -> t

val to_string : t -> string
val to_csv : out_channel -> t -> unit
