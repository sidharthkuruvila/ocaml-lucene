(** Split the list at index

  This function treats the list as zero indexed and everything at and after the index will
  be in the second part.

  This function will return a value even if the list is shorter than the split index
  in that case it will return the list and and an empty list.
*)
val split_at_index: index:int -> 'a list -> ('a list * 'a list)

(** Drop the first n elements of a list

If the list contains less than n elements return an empty list
*)
val drop_n: n:int -> 'a list -> 'a list

(** Find the length of the common prefix of two lists

Two elements are considered equal if compare returns 0
*)
val common_prefix_length: 'a list -> 'a list -> compare:('a -> 'a -> int) -> int

(** Check if a list is empty *)
val is_empty: 'a list -> bool
