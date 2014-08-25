open Lwt
open Cmdliner

exception Error_expr of string

let () =
  let p = Printf.sprintf in
  Printexc.register_printer
    (function
      | Error_expr str ->
        Some (p "parsing error in expr: %s" str)
      | _ -> None)

let make_list expr =
  let lexbuf = Lexing.from_string expr in
  try Parser.final Lexer.token lexbuf
  with
    | Parser.Error
    | Lexer.Error ->
      let loc = Location.make
        (Lexing.lexeme_start_p lexbuf)
        (Lexing.lexeme_end_p lexbuf)
      in let str = Printf.sprintf "%s"
        (Location.to_string_of_line loc expr)
      in raise (Error_expr str)

let make_and_compute requests clients kalive uri =
  let ab = Ab.make ~requests ~clients ~kalive uri in
  let httperf = Httperf.make ~requests ~clients ~kalive uri in
  let weighttp = Weighttp.make ~requests ~clients ~kalive uri in
  Ab.compute ab
  >>= fun result ->
  Httperf.compute ~result httperf
  >>= fun result ->
  Weighttp.compute ~result weighttp
  >>= fun result ->
    Printf.printf "%s\n%!" (Result.to_string result);
    Lwt.return ()

let main
  (host : string)
  (port : int)
  (path : string)
  (ssl : bool)
  (nbr : int)
  (kalive : bool)
  (repeat : int)
  (expr : string) =
  try
    let scheme = if ssl then "https" else "http" in
    let uri = Uri.make ~scheme ~host ~port ~path () in
    let clients= make_list expr in
    let cmd () =
      Usine.compute ~requests:nbr ~clients ~kalive ~uri repeat
      >|= Usine.to_csv stdout in
    Lwt_main.run (Lwt.join [cmd ()]);
    `Ok ()
  with exn -> `Error (false, Printexc.to_string exn)

let addr =
  let doc = "Host of server" in
  Arg.(required & opt (some string) None & info ["h"; "host"] ~docv:"HOST" ~doc)

let port =
  let doc = "Port of server" in
  Arg.(required & opt (some int) None & info ["p"; "port"] ~docv:"PORT" ~doc)

let path =
  let doc = "Path of server" in
  Arg.(value & opt string "/" & info ["u"; "uri"] ~docv:"PATH" ~doc)

let ssl =
  let doc = "Enable SSL" in
  Arg.(value & flag & info ["ssl"] ~doc)

let nbr =
  let doc = "Number of requests" in
  Arg.(value & opt int 10000 & info ["n"; "number"] ~docv:"NUMBER" ~doc)

let kalive =
  let doc = "Keep-alive" in
  Arg.(value & opt bool true & info ["k"; "keep-alive"] ~doc)

let repeat =
  let doc = "Repeat operation" in
  Arg.(value & opt int 1 & info ["r"; "repeat"] ~docv:"NUMBER" ~doc)

let expr =
  let doc = "[ expr | enum & predicat ]" in
  Arg.(required
       & pos ~rev:true 0 (some string) None
       & info [] ~docv:"EXPR" ~doc)

let cmd =
  let doc = "Benchmark of HTTP server" in
  let man = [
    `P "BUGS";
    `S "Email them to <romain.calascibetta@gmail.com>";
  ] in
  Term.(ret (pure main
      $ addr
      $ port
      $ path
      $ ssl
      $ nbr
      $ kalive
      $ repeat
      $ expr)),
  Term.info "usine" ~version:"0.1" ~doc ~man

let () =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0
