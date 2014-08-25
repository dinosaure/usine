open Lwt
open Re

type t = Lwt_process.command

exception Error_output of (string * string)

let () =
  let p = Printf.sprintf in
  Printexc.register_printer
    (function
     | Error_output (command, output) ->
       Some (p "Error in Weighttp output:\n \
                $ %s\n \
                %s\n \
                =========================\n" command output)
     | _ -> None)

let make ~requests ~clients ?(kalive = false) uri =
  let p = Printf.sprintf in
  let c = p "weighttp -n %d -c %d %s '%s'"
    requests clients (if kalive then "-k" else "")
    (Uri.to_string uri)
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
  let re_requests_per_second =
    Re_pcre.regexp "([0-9]+) req/s" in
  let re_transfer_rate =
    Re_pcre.regexp "([0-9]+) kbyte/s" in
  let re_requests_status =
    Re_pcre.regexp "requests: \
    [0-9]+ total, \
    [0-9]+ started, \
    [0-9]+ done, \
    ([0-9]+) succeeded, \
    ([0-9]+) failed, \
    ([0-9]+) errored" in
  let ex_requests_status rex str =
    let atoi a i = Array.get a i |> int_of_string in
    Re_pcre.extract ~rex str
      |> fun a -> (atoi a 1, atoi a 2, atoi a 3) in
  let re_reply_status =
    Re_pcre.regexp "status codes: \
    ([0-9]+) 2xx, \
    ([0-9]+) 3xx, \
    ([0-9]+) 4xx, \
    ([0-9]+) 5xx" in
  let ex_reply_status rex str =
    let atoi a i = Array.get a i |> int_of_string in
    Re_pcre.extract ~rex str
      |> fun a -> (0, atoi a 1, atoi a 2, atoi a 3, atoi a 4) in
  let extract rex str transform =
    Re_pcre.extract ~rex str |> fun a -> Array.get a 1 |> transform in
  Printf.printf "=> %s\n%!" (to_string cmd);
  Lwt_process.pread ~stderr:`Dev_null cmd
  >>= fun output ->
    try
      let requests_per_second =
        extract re_requests_per_second output int_of_string in
      let transfer_rate =
        extract re_transfer_rate output int_of_string |> float_of_int in
      let requests_status =
        ex_requests_status re_requests_status output in
      let reply_status =
        ex_reply_status re_reply_status output in
      let result =
        Result.update result
          ~requests_per_second
          ~transfer_rate
          ~requests_status
          ~reply_status
          ()
      in Lwt.return result
    with Not_found ->
      Printf.fprintf stderr "%s"
        (Printexc.to_string (Error_output (to_string cmd, output)));
      Lwt.return result
