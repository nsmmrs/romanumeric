type code = { symbol : char; value : int; repeatable : bool; }
type repetition = { code: code; length: int }
type table = code list
type memo = { code : code; subtractors : table; }
type system = { table : table; memos : memo list; msl : int; msd : int; }
val compare : code -> code -> int
val sort_asc : table -> table
val sort_desc : table -> table
val repeat : code -> int -> repetition
val subtraction :
  code:code ->
  subtractors:table -> target:int -> msl:int -> (repetition list * int) option
val encode : system:system -> ?acc:repetition list -> int -> string
val encode_subtractive : system:system -> int -> (repetition list * int) option
val encode_additive : int -> table -> repetition list * int
val repeatable : int -> ('a * int) list -> bool
val subtractors : code -> table -> int -> table
val make_code : char * int -> symbols_values:('a * int) list -> code
val make_table : (char * int) list -> table
val make_memos : table -> msd:int -> memo list
val make_encoder :
  (char * int) list -> int -> int -> ?acc:repetition list -> int -> string
val plus_or_minus : code * int -> given:code * 'a -> int
val sum : ?acc:int -> (code * int) list -> int
val decode : table:table -> string -> int
val make_decoder : (char * int) list -> string -> int
