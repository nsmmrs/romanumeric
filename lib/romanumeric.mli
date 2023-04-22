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

type encoding_result = (string, string) result

type decoding_result = (int, string) result

val encode : system:system -> int -> encoding_result

val make_encoder : table -> int -> int -> int -> encoding_result

val decode : table:table -> string -> decoding_result

val make_decoder : table -> string -> decoding_result

module Roman : sig
  val table : table

  val to_int : string -> decoding_result

  val of_int : ?c:int -> int -> encoding_result
end
