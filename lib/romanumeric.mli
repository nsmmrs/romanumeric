type code = {symbol: char; value: int; repeatable: bool}

type table = code list

type repetition = {code: code; length: int}

type numeral = repetition list

type accumulator = {numeral: numeral; remainder: int}

type memo = {code: code; subtractors: table}

type system = {table: table; memos: memo list; msl: int; msd: int}

val compare : code -> code -> int

val sort_asc : table -> table

val sort_desc : table -> table

val repeat : code -> int -> repetition

val subtraction :
  code:code -> subtractors:table -> target:int -> msl:int -> accumulator option

val encode : system:system -> int -> string

val encode_subtractive : system -> accumulator -> accumulator option

val encode_additive : system -> accumulator -> accumulator

val repeatable : int -> ('a * int) list -> bool

val subtractors : code -> table -> int -> table

val make_code : char * int -> symbols_values:('a * int) list -> code

val make_table : (char * int) list -> table

val make_memos : table -> msd:int -> memo list

val make_encoder : table -> int -> int -> int -> string

val plus_or_minus : code * int -> given:code * 'a -> int

val sum : ?acc:int -> (code * int) list -> int

val decode : table:table -> string -> int

val make_decoder : table -> string -> int

val chars_of_repetition : repetition -> char list

module Roman : sig
  val table : table

  val decode : string -> int

  val conventional : int -> string

  val compressed : int -> int -> string
end
