(** Roman numeral decoding and encoding. *)

val decode : string -> int
(** Converts from Roman numeral to decimal integer.

    {[
      # Roman_numeral.decode "IV"
      - : int = 4
    ]}
*)
