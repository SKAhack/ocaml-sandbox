open String
open List
open Printf

let split s c =
  let rec loop str =
    try
      let pos = String.index str c in
      let sub = String.sub str 0 pos in
      let left = String.sub str (pos + 1) (String.length str - pos - 1) in
      sub :: (loop left)
    with Not_found ->
      str :: []
  in
  loop s
;;

let () =
  (* split test *)
  let languages = "OCaml,Perl,C++,C" in
  let f s = printf "%s\n" s in
  List.iter f (split languages ',')
