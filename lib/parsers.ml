module Input = struct
  type t = {
    pos : int;
    len : int;
    src : string;
  }

  let make src = { pos=0; len=String.length src; src }
  let incr inp = { inp with pos = inp.pos + 1 }

  let to_string inp =
    if inp.pos >= inp.len then ""
                          else String.sub inp.src inp.pos (inp.len - inp.pos)
end

module Error = struct
  type t = {
    kind  : kind;    [@warning "-69"]
    input : Input.t; [@warning "-69"]
  }

  and kind =
    | EOF
    | Custom of string
    | Expected of string
    | ExpectedButFound of string * string

  let make kind input = { kind; input }
end

type 'a parse_result = ('a * Input.t, Error.t) Result.t
type 'a t = Input.t -> 'a parse_result (** the parser type *)

let run_p input (p : 'a t) =
  match p input with
  | Ok (result, _) -> Ok result
  | Error e -> Error e

let return value = fun inp -> Ok (value, inp)
let fail kind = fun inp -> Error (Error.make kind inp)

let map p f = fun inp ->
  match p inp with
  | Error e -> Error e
  | Ok (result, rest) -> Ok (f result, rest)

let bind p f = fun inp ->
  match p inp with
  | Error e -> Error e
  | Ok (result, rest) -> (f result) rest

let ( <|> ) a b = fun inp ->
  match a inp with
  | Error _ -> b inp
  | Ok result -> Ok result

let rec many p = fun inp ->
  match p inp with
  | Error _ -> Ok ([], inp)
  | Ok (result, rest) ->
    match many p rest with
    | Error _ -> Ok ([result], rest)
    | Ok (results, final) -> Ok (result :: results, final)

let notp p : unit t = fun inp ->
  match p inp with
  | Error _ -> Ok ((), inp)
  | Ok _ -> Error (Error.make (Custom "Negation failure") inp)   

let many1 p =
  bind p @@ fun first ->
    bind (many p) @@ fun rest ->
      return (first :: rest)

let ( *> ) a b = bind a (fun _ -> b)
let ( <* ) a b = bind a (fun a' -> map b (fun _ -> a'))
let ( <*> ) a b = bind a (fun a' -> map b (fun b' -> (a', b')))

module Let_syntax = struct
  let ( let+ ) = map
  let ( let* ) = bind
  let ( and+ ) = ( <*> )
end

let any_p : char t = fun inp ->
  if inp.pos < inp.len then
    let c = String.get inp.src inp.pos in
    let next = Input.incr inp in
    Ok (c, next)
  else
    Error (Error.make EOF inp)

let bytes_p (n : int64) = fun (inp : Input.t) ->
  let open Int64 in
  let int_n = to_int n in
  let length = of_int inp.len in
  let offset = add n (of_int inp.pos) in
  if offset <= length then
    let next = { inp with pos=inp.pos + int_n } in
    let result = String.sub inp.src inp.pos int_n in
    Ok (result, next)
  else
    Error (Error.make Error.EOF inp)

open Let_syntax

let char_p expected =
  bind any_p @@ function
    | c when Char.equal c expected -> return c
    | c -> fail (ExpectedButFound (Char.escaped expected, Char.escaped c))

let digit_p : char t =
  bind any_p @@ function
    | '0'..'9' as c -> return c
    | c -> fail (Custom (Printf.sprintf "Expected digit character but found '%c'" c))

let alpha_p : char t =
  bind any_p @@ function
    | 'a'..'z' | 'A'..'Z' as c -> return c
    | c -> fail (Custom (Printf.sprintf "Expected alpha character but found '%c'" c))

let string_p str : string t =
  map (String.fold_left (fun acc c -> acc *> char_p c) (return ' ') str)
      (fun _ -> str)

let uint_p =
  let c_int c = Int64.of_int @@ int_of_char c - int_of_char '0' in
  let+ first, rest = digit_p <*> many digit_p in 
  first :: rest 
    |> List.fold_left (fun acc d -> Int64.(add (mul acc 10L) (c_int d))) 0L

let int_p =
  let* neg = char_p '-' *> return true <|> return false in
  let+ num = uint_p in
  if neg then Int64.neg num else num

