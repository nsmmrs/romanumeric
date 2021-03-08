(** Roman numeral decoding and encoding. *)

val decode : string -> int
(** Converts from Roman numeral to decimal integer.

    {4 Examples}

    {[ print_endline @@ decode "IV" ]} *)
