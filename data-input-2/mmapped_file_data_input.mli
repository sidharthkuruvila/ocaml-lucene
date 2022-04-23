
type t

val of_bytes: Mmapped_file_bytes.t -> t

val to_bytes: t -> Mmapped_file_bytes.t

val read_byte : t -> char
val read_bytes : t -> int -> string
val get_byte : t -> int -> char
val skip_bytes : t -> int -> unit

val get_position : t -> int
val set_position : t -> int -> unit
val length : t -> int
val copy: t -> t

val read_int : t -> int
val read_uint : t -> int
val read_long : t -> int
val read_vlong : t -> int
val read_vint : t -> int
val read_string: t -> String.t
val read_list_of_strings: t -> string list
val read_assoc_list_of_strings: t -> (string * string) list