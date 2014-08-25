open Lwt
open Re

type t = Lwt_process.command

exception Error_output of (string * string)

let () =
  let p = Printf.sprintf in
  Printexc.register_printer
    (function
     | Error_output (command, output) ->
       Some (p "Error in Httperf output:\n \
                $ %s\n \
                %s\n \
                ========================\n" command output)
     | _ -> None)

let make ~requests ~clients ?(kalive = false) uri =
  let p = Printf.sprintf in
  let c = match kalive with
    | true ->
      p "httperf --server='%s' --port=%d \
                 --rate=%d --num-conns=%d --num-calls=%d \
                 --timeout 5 --hog --uri='%s'"
      (Uri.host_with_default uri)
      (Uri.port uri |> Batteries.Option.default 80)
      clients clients requests
      (Uri.path uri)
    | false ->
      p "httperf --server='%s' --port=%d \
                 --rate=%d --num-conns=%d --num-calls=1 \
                 --timeout 5 --hog --uri='%s'"
      (Uri.host_with_default uri)
      (Uri.port uri |> Batteries.Option.default 80)
      clients clients
      (Uri.path uri)
  in Lwt_process.shell c

module Buffer = struct
  include Buffer

  let add_array ?(sep = "") add_data buffer data =
    let rec aux = function
      | [] -> ()
      | [ x ] -> add_data buffer x
      | x :: r -> Printf.bprintf buffer "%a%s" add_data x sep; aux r
    in aux (Array.to_list data)
end

let to_string (_, a) =
  let buffer = Buffer.create 16 in
  Printf.bprintf buffer "%a"
    (Buffer.add_array ~sep:" " Buffer.add_string) a;
  Buffer.contents buffer

let compute ?(result = Result.make ()) cmd =
  let re_request_rate =
    Re_pcre.regexp "Request rate: ([0-9]+\\.[0-9]+)" in
  let re_reply_status =
    Re_pcre.regexp "Reply status: \
    1xx=([0-9]+) \
    2xx=([0-9]+) \
    3xx=([0-9]+) \
    4xx=([0-9]+) \
    5xx=([0-9]+)" in
  let ex_reply_status rex str =
    let atoi a i = Array.get a i |> int_of_string in
    Re_pcre.extract ~rex str
      |> fun a -> (atoi a 1, atoi a 2, atoi a 3, atoi a 4, atoi a 5) in
  let re_transfer_rate =
    Re_pcre.regexp "Net I/O: ([0-9]+\\.[0-9]+) KB/s" in
  let extract rex str transform =
    Re_pcre.extract ~rex str |> fun a -> Array.get a 1 |> transform in
  Printf.printf "=> %s\n%!" (to_string cmd);
  Lwt_process.pread ~stderr:`Dev_null cmd
  >>= fun output ->
    try
      let request_rate = extract re_request_rate output float_of_string in
      let reply_status = ex_reply_status re_reply_status output in
      let transfer_rate = extract re_transfer_rate output float_of_string in
      let result =
        Result.update result
          ~request_rate
          ~reply_status
          ~transfer_rate
          ()
      in Lwt.return result
    with Not_found ->
      Printf.fprintf stderr "%s"
        (Printexc.to_string (Error_output (to_string cmd, output)));
      Lwt.return result
