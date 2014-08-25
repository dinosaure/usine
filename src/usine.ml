open Lwt

let compute
  ~requests
  ~clients
  ~kalive
  ~uri
  repeat =
    Lwt_list.map_p
      (fun clients ->
        let a = Ab.make ~requests ~clients ~kalive uri in
        let b = Httperf.make ~requests ~clients ~kalive uri in
        let c = Weighttp.make ~requests ~clients ~kalive uri in
        Lwt_list.fold_left_s
          (fun result _ ->
            Ab.compute ~result a
            >>= fun result -> Httperf.compute ~result b
            >>= fun result -> Weighttp.compute ~result c)
          (Result.make ())
          (Batteries.List.make repeat ()))
      clients

let to_csv ch lst =
  List.iter
    (fun result -> Printf.fprintf ch "%a\n" Result.to_csv result) lst
