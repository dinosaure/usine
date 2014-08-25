open Lwt
open Re

type t = Lwt_process.command

exception Error_output of (string * string)

let () =
  let p = Printf.sprintf in
  Printexc.register_printer
    (function
     | Error_output (command, output) ->
       Some (p "Error in Apache Benchmark output:\n \
                $ %s\n \
                %s\n \
                =================================\n" command output)
     | _ -> None)

let make ~requests ~clients ?(kalive = false) uri =
  let p = Printf.sprintf in
  let c = p "ab -n %d -c %d -S -d -t 1 '%s'"
    requests clients (Uri.to_string uri)
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
  let re_complete_requests =
    Re_pcre.regexp "Complete requests:[ \t]+([0-9]+)\n" in
  let re_failed_requests =
    Re_pcre.regexp "Failed requests:[ \t]+([0-9]+)\n" in
  let re_requests_per_second =
    Re_pcre.regexp "Requests per second:[ \t]+([0-9]+\\.[0-9]+)" in
  let re_time_per_request =
    Re_pcre.regexp "Time per request:[ \t]+([0-9]+\\.[0-9]+)" in
  let re_transfer_rate =
    Re_pcre.regexp "Transfer rate:[ \t]+([0-9]+\\.[0-9]+)" in
  let extract rex str transform =
    Re_pcre.extract ~rex str |> fun a -> Array.get a 1 |> transform in
  Printf.printf "=> %s\n%!" (to_string cmd);
  Lwt_process.pread ~stderr:`Dev_null cmd
  >>= fun output ->
    try
      let complete_requests =
        extract re_complete_requests output int_of_string in
      let failed_requests = extract re_failed_requests output int_of_string in
      let requests_per_second =
        extract re_requests_per_second output float_of_string |> int_of_float in
      let time_per_request =
        extract re_time_per_request output float_of_string in
      let transfer_rate =
        extract re_transfer_rate output float_of_string in
      let result =
        Result.update result
          ~complete_requests
          ~failed_requests
          ~requests_per_second
          ~time_per_request
          ~transfer_rate
          ()
      in Lwt.return result
    with Not_found ->
      Printf.fprintf stderr "%s"
        (Printexc.to_string (Error_output (to_string cmd, output)));
      Lwt.return result
