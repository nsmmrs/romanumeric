(** Roman numeral decoding and encoding. *)
module Glyph : sig
  type t = I | V | X | L | C | D | M
end

val decode : string -> int
(** Converts from Roman numeral to decimal integer.
    {[
      # open Roman_numeral
      # decode "XVIII"
      - : int = 18
    ]}
    Also handles non-standard historical "edge cases":
    {[
      # open Roman_numeral
      # decode "XIIX"
      - : int = 18
      # decode "IIXX"
      - : int = 18
      # decode "XXIIX"
      - : int = 28
    ]}
*)

val encode : ?msd:int -> ?msl:int -> int -> string
(** Converts from Roman numeral to decimal integer.
    {[
      # open Roman_numeral
      # encode 1956;;
      - : string = "MCMLVI"
      # encode 4;;
      - : string = "IV"
    ]}
*)
