open Str

type point = {
    i : int;
	x : int;
    y : int
};;

let load_file f =
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    (s);;

let rec remove = fun x l ->
      match l with
      | [] -> []
      | e::es -> 
            if e = x then remove x es
            else e::(remove x es)

let strip_string s =
   Str.global_replace (Str.regexp "[\r\n\t\" ]") "" s

let extract_val = fun s ->
        let str = Str.split (Str.regexp "[\\{\\},ixy:]") s in
        remove "[" (remove "]" (remove "" str))

let to_points = fun s ->
     let cast_int = fun str ->
        int_of_float (float_of_string str) in
     let rec to_points_rec = fun sl pl ->
          match sl with
          [] -> List.rev pl
          | i::x::y::ps -> to_points_rec ps ({i= cast_int i;x= cast_int x;y= cast_int y}::pl) in
     to_points_rec s []

let input_points = fun file_name ->
    let s = strip_string (load_file file_name) in
    Str.search_forward (Str.regexp "\\[.*\\]") s 0;
    let str_points = Str.matched_string s  in
    let list_val = extract_val str_points in
    to_points list_val
