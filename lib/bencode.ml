module Dict = Map.Make ( String )

type t =
  | BList of t list
  | BDict of t Dict.t
  | BString of string
  | BInteger of int64

(* Accessors *)

let bval_opt ~key = function
  | BDict dict -> Dict.find_opt key dict
  | _          -> None

let get_int_opt ~key dict =
  match bval_opt ~key dict with
  | Some (BInteger i) -> Some i
  | _                 -> None

let get_str_opt ~key dict =
  match bval_opt ~key dict with
  | Some (BString s)  -> Some s
  | _                 -> None

let get_lst_opt ~key dict =
  match bval_opt ~key dict with
  | Some (BList l)    -> Some l
  | _                 -> None

let get_dct_opt ~key dict =
  match bval_opt ~key dict with
  | Some (BDict d)    -> Some d
  | _                 -> None

let rec bval_deep_opt ~keys bval =
  match keys, bval with
  | [], _             -> Some bval
  | x::xs, BDict dict ->
    begin match bval_opt ~key:x (BDict dict) with
      | Some subval -> bval_deep_opt ~keys:xs subval
      | None        -> None
    end
  | _ -> None

(* Parsing and Serialization *)

let rec to_string = function (* TODO use buffer here *)
  | BString s  -> Printf.sprintf "%d:%s" (String.length s) s
  | BInteger i -> Printf.sprintf "i%se" (Int64.to_string i)
  | BList l    -> List.fold_left (fun acc s -> acc ^ to_string s) "l" l ^ "e"
  | BDict d    -> Dict.fold (fun k v acc -> acc ^ to_string (BString k) ^ to_string v) d "d" ^ "e"

open Parsers
open Parsers.Let_syntax

let rec of_string_opt str =
  let inp = Input.make str in
  Result.to_option @@ run_p inp bdict_p

and bvalue_p inp =
  (binteger_p <|> bstring_p <|> blist_p <|> bdict_p) inp

and string_p =
  let* length = uint_p <* char_p ':' in
  bytes_p length

and binteger_p =
  let+ value = char_p 'i' *> int_p <* char_p 'e' in
  BInteger value

and bstring_p inp = inp |>
  let+ value = string_p in
  BString value

and blist_p inp = inp |>
  let+ list_values = char_p 'l' *> many bvalue_p <* char_p 'e' in
  BList list_values

and bdict_p inp = inp |>
  let dict_pair = string_p <*> bvalue_p in
  let+ dict_values = char_p 'd' *> many dict_pair <* char_p 'e' in
  BDict (Dict.of_list dict_values)
