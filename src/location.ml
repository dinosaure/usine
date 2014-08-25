type position = { lnum : int; cnum : int; seek : int; }
type t = (position * position)

let make start_pos end_pos =
  let get_pos pos =
    { lnum = pos.Lexing.pos_lnum;
      cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol;
      seek = pos.Lexing.pos_cnum; }
  in (get_pos start_pos, get_pos end_pos)

let compose start stop = (start, stop)

let dummy =
  ({ lnum = 0; cnum = 0; seek = 0 },
   { lnum = 0; cnum = 0; seek = 0 })

let start = fst
let stop = snd

let to_string_of_line (a, b) str =
  let buffer = Buffer.create 16 in
  let rec aux i c =
    if i = a.seek then
      begin Buffer.add_string buffer "\027[0;31m";
      Buffer.add_char buffer c end
    else if i = b.seek then
      begin Buffer.add_char buffer c;
      Buffer.add_string buffer "\027[0;37m"; end
    else Buffer.add_char buffer c
  in
  String.iteri aux str;
  if String.length str = b.seek then Buffer.add_string buffer "\027[0;37m";
  Buffer.contents buffer

let to_string (a, b) =
  let print_aux ty () (a, b) =
    if a = b then Printf.sprintf "%s%d" ty a
    else Printf.sprintf "%s%d - %d" ty a b
  in
  Printf.sprintf "%a %a"
    (print_aux "l.") (a.lnum, b.lnum)
    (print_aux "c.") (a.cnum, b.cnum)
