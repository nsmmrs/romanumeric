(** Roman numeral decoding and encoding. *)

val decode : string -> int
(** Converts from Roman numeral to decimal integer.

    {[
      # Roman_numeral.decode "IV"
      - : int = 4
    ]}
*)

val encode : int -> string
(** Converts from Roman numeral to decimal integer.

    {[
      # Roman_numeral.encode 4
      - : string "IV"
    ]}
*)
