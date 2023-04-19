type code = {symbol: char; value: int}

type table = code list

type table_memo = {asc: table; desc: table}

type code_repetition = {code: code; length: int}

type numeral = code_repetition list

type accumulator = {numeral: numeral; remainder: int}

type system =
  { table: table_memo
  ; repeatable: code -> bool
  ; subtractors: code -> table
  ; msl: int
  ; msd: int }

val encode : system:system -> int -> string

val make_encoder : table -> int -> int -> int -> string

val decode : table:table -> string -> int

val make_decoder : table -> string -> int

module Roman : sig
  val table : table

  val decode : string -> int

  val conventional : int -> string

  val compressed : int -> int -> string
end
