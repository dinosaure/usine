type t =
  {
    complete_requests   : int option;
    failed_requests     : int option;
    requests_per_second : int option;
    time_per_request    : float option;
    transfer_rate       : float option;
    request_rate        : float option;
    reply_status        : (int * int * int * int * int) option;
    requests_status     : (int * int * int) option;
  }

let make
  ?complete_requests
  ?failed_requests
  ?requests_per_second
  ?time_per_request
  ?transfer_rate
  ?request_rate
  ?reply_status
  ?requests_status () =
    {
      complete_requests;
      failed_requests;
      requests_per_second;
      time_per_request;
      transfer_rate;
      request_rate;
      reply_status;
      requests_status;
    }

module Option = struct
  include Batteries.Option

  let map2 f o1 o2 =
    match o1, o2 with
    | Some a, Some b -> Some (f a b)
    | Some x, None
    | None, Some x -> Some x
    | None, None -> None
end

let update result
  ?complete_requests
  ?failed_requests
  ?requests_per_second
  ?time_per_request
  ?transfer_rate
  ?request_rate
  ?reply_status
  ?requests_status () =
  let complete_requests =
    Option.map2
    (fun o n -> (o + n) / 2)
    result.complete_requests
    complete_requests in
  let failed_requests =
    Option.map2
    (fun o n -> (o + n) / 2)
    result.failed_requests
    failed_requests in
  let requests_per_second =
    Option.map2
    (fun o n -> (o + n) / 2)
    result.requests_per_second
    requests_per_second in
  let time_per_request =
    Option.map2
    (fun o n -> (o +. n) /. 2.)
    result.time_per_request
    time_per_request in
  let transfer_rate =
    Option.map2
    (fun o n -> (o +. n) /. 2.)
    result.transfer_rate
    transfer_rate in
  let request_rate =
    Option.map2
    (fun o n -> (o +. n) /. 2.)
    result.request_rate
    request_rate in
  let reply_status =
    Option.map2
    (fun (o1, o2, o3, o4, o5) (n1, n2, n3, n4, n5) ->
      ((o1 + n1) / 2,
       (o2 + n2) / 2,
       (o3 + n3) / 2,
       (o4 + n4) / 2,
       (o5 + n5) / 2))
    result.reply_status
    reply_status in
  let requests_status =
    Option.map2
    (fun (o1, o2, o3) (n1, n2, n3) ->
      ((o1 + n1) / 2, (o2 + n2) / 2, (o3 + n3) / 2))
    result.requests_status
    requests_status in
  {
    complete_requests;
    failed_requests;
    requests_per_second;
    time_per_request;
    transfer_rate;
    request_rate;
    reply_status;
    requests_status;
  }

let merge r1
  {
    complete_requests;
    failed_requests;
    requests_per_second;
    time_per_request;
    transfer_rate;
    request_rate;
    reply_status;
    requests_status;
  } =
  update r1
    ?complete_requests
    ?failed_requests
    ?requests_per_second
    ?time_per_request
    ?transfer_rate
    ?request_rate
    ?reply_status
    ?requests_status
    ()

module Buffer = struct
  include Buffer

  let add_option ?(none = "#none") add_data buffer = function
    | None -> add_string buffer none
    | Some a -> add_data buffer a

  let add_int buffer =
    Printf.bprintf buffer "%d"

  let add_float buffer =
    Printf.bprintf buffer "%f"
end

let to_string
  {
    complete_requests;
    failed_requests;
    requests_per_second;
    time_per_request;
    transfer_rate;
    request_rate;
    reply_status;
    requests_status;
  } =
  let buffer_add_reply_status buffer (a, b, c, d, e) =
    Printf.bprintf buffer "1xx = %d, 2xx = %d, 3xx = %d, 4xx = %d, 5xx %d"
      a b c d e in
  let buffer_add_requests_status buffer (a, b, c) =
    Printf.bprintf buffer "succeeded = %d, failed = %d, errored = %d"
      a b c in
  let buffer = Buffer.create 16 in
  Printf.bprintf buffer
    "complete requests   : %a\n\
     failed requests     : %a\n\
     requests per second : %a\n\
     time per request    : %a\n\
     transfer rate       : %a\n\
     request rate        : %a\n\
     reply status        : %a\n\
     requests status     : %a\n"
    (Buffer.add_option Buffer.add_int) complete_requests
    (Buffer.add_option Buffer.add_int) failed_requests
    (Buffer.add_option Buffer.add_int) requests_per_second
    (Buffer.add_option Buffer.add_float) time_per_request
    (Buffer.add_option Buffer.add_float) time_per_request
    (Buffer.add_option Buffer.add_float) transfer_rate
    (Buffer.add_option buffer_add_reply_status) reply_status
    (Buffer.add_option buffer_add_requests_status) requests_status;
  Buffer.contents buffer

let to_csv ch
  {
    complete_requests;
    failed_requests;
    requests_per_second;
    time_per_request;
    transfer_rate;
    request_rate;
    reply_status;
    requests_status;
  } =
  let buffer_add_reply_status buffer (a, b, c, d, e) =
    Printf.bprintf buffer "1xx = %d, 2xx = %d, 3xx = %d, 4xx = %d, 5xx %d"
      a b c d e in
  let buffer_add_requests_status buffer (a, b, c) =
    Printf.bprintf buffer "succeeded = %d, failed = %d, errored = %d"
      a b c in
  let buffer = Buffer.create 16 in
  Printf.bprintf buffer
    "%a;%a;%a;%a;%a;%a;%a;%a;"
    (Buffer.add_option Buffer.add_int) complete_requests
    (Buffer.add_option Buffer.add_int) failed_requests
    (Buffer.add_option Buffer.add_int) requests_per_second
    (Buffer.add_option Buffer.add_float) time_per_request
    (Buffer.add_option Buffer.add_float) time_per_request
    (Buffer.add_option Buffer.add_float) transfer_rate
    (Buffer.add_option buffer_add_reply_status) reply_status
    (Buffer.add_option buffer_add_requests_status) requests_status;
  Printf.fprintf ch "%s" (Buffer.contents buffer)

