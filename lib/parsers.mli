module Input : sig
  type t

  val make : string -> t
  val to_string : t -> string
end

module Error : sig
  type t

  type kind =
    | EOF
    | Custom of string
    | Expected of string
    | ExpectedButFound of string * string
end

type 'a parse_result
type 'a t = Input.t -> 'a parse_result

val run_p : Input.t -> 'a t -> ('a, Error.t) result

val return : 'a -> 'a t
val fail   : Error.kind -> 'a t
val map    : 'a t -> ('a -> 'b) -> 'b t
val bind   : 'a t -> ('a -> 'b t) -> 'b t
val many   : 'a t -> 'a list t
val many1  : 'a t -> 'a list t
val notp   : 'a t -> unit t

val ( *> )  : 'a t -> 'b t -> 'b t
val ( <* )  : 'a t -> 'b t -> 'a t
val ( <*> ) : 'a t -> 'b t -> ('a * 'b) t
val ( <|> ) : 'a t -> 'a t -> 'a t

module Let_syntax : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

val any_p    : char t
val bytes_p  : int64 -> string t
val char_p   : char -> char t
val digit_p  : char t
val alpha_p  : char t
val string_p : string -> string t
val int_p    : int64 t
val uint_p   : int64 t
