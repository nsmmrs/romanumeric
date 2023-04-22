type code = {symbol: char; value: int}

type code_repetition = {code: code; length: int}

type table = code list

type table_memo = {asc: table; desc: table}

type system =
  { table: table_memo
  ; repeatable: code -> bool
  ; subtractors: code -> table
  ; msl: int
  ; msd: int }

type encoding_result = (string, string) result

type decoding_result = (int, string) result

module Code : sig
  type t = code

  type repetition = code_repetition

  val compare : t -> t -> int

  val make : char * int -> t

  val repeat : int -> t -> repetition

  val repeatable : table -> t -> bool
end = struct
  type t = code

  type repetition = code_repetition

  let make (symbol, value) = {symbol; value}

  let compare a b = Int.compare a.value b.value

  let repeat length code = {code; length}

  let repeatable table code =
    let double_of code other = code.value * 2 = other.value in
    not (List.exists table ~f:(double_of code))
end

module CodeMap = Map.Make (Code)

type code_table_map = table CodeMap.t

module Table : sig
  type t = table

  type memo = table_memo

  val make : (char * int) list -> t

  val exists : code -> table -> bool

  val memoize : t -> memo

  val code_of_char : table -> char -> code option
end = struct
  type t = table

  type memo = table_memo

  let sort : t -> t = List.sort ~cmp:Code.compare

  let make symbols_values = symbols_values |> List.map ~f:Code.make |> sort

  let exists (code : code) (table : table) =
    List.exists table ~f:(fun other -> other.value = code.value)

  let memoize table = {asc= table; desc= List.rev table}

  let code_of_char table char =
    List.find_opt table ~f:(fun code -> Char.equal code.symbol char)
end

module Repetition : sig
  type t = code_repetition

  val value : t -> int

  val of_chars : table -> char list -> t option

  val to_chars : t -> char list

  val to_addend : next:t -> t -> int
end = struct
  type t = code_repetition

  let of_chars table chars =
    match chars with
    | [] ->
        None
    | char :: _ ->
        Option.map
          (Code.repeat (List.length chars))
          (Table.code_of_char table char)

  let to_chars {code; length} =
    List.init (Int.abs length) ~f:(fun _ -> code.symbol)

  let value {code; length} = code.value * length

  let to_addend ~next current =
    let v = value current in
    if current.code.value < next.code.value then -v else v
end

module System : sig
  type t = system

  val make : table -> int -> int -> t
end = struct
  type t = system

  let repeatable : table_memo -> code -> bool =
   fun table ->
    let repeatable_codes =
      List.filter table.asc ~f:(Code.repeatable table.asc)
    in
    fun code -> Table.exists code repeatable_codes

  let subtractors_for code table msd =
    let valid subs other =
      if
        List.length subs = msd
        || other.value >= code.value
        || other.value * 2 = code.value
      then subs
      else other :: subs
    in
    List.fold_left ~f:valid table.desc ~init:[]

  let memoize_subtractors table msd : code_table_map =
    List.fold_left table.asc ~init:CodeMap.empty ~f:(fun map code ->
        CodeMap.add code (subtractors_for code table msd) map )

  let subtractors : table_memo -> int -> code -> table =
   fun table msd ->
    let memoized = memoize_subtractors table msd in
    fun code -> CodeMap.get_or code memoized ~default:[]

  let make table msd msl =
    let table = Table.memoize table in
    { table
    ; repeatable= repeatable table
    ; subtractors= subtractors table msd
    ; msd
    ; msl }
end

let repeat = Code.repeat

type numeral = code_repetition list

type accumulator = {numeral: numeral; remainder: int}

module Numeral : sig
  type t = numeral

  val to_int : t -> int

  val of_int : system -> int -> t option

  val to_string : t -> string

  val of_string : table -> string -> t option
end = struct
  type t = numeral

  let to_string numeral =
    numeral
    |> List.rev_map ~f:Repetition.to_chars
    |> List.concat |> String.of_list

  let to_int numeral =
    let rec sum ?(acc = 0) (remainder : t) =
      match remainder with
      | [] ->
          acc
      | current :: (next :: _ as remaining) ->
          let addend = Repetition.to_addend current ~next in
          sum remaining ~acc:(acc + addend)
      | current :: empty ->
          sum empty ~acc:(acc + Repetition.value current)
    in
    sum numeral

  let append_group table numeral group =
    Option.bind numeral (fun n ->
        Option.bind (Repetition.of_chars table group) (fun r -> Some (r :: n)) )

  let of_char_groups table groups =
    List.fold_left groups ~init:(Some []) ~f:(append_group table)
    |> Option.map List.rev

  let of_string table string =
    string |> String.to_list
    |> List.group_succ ~eq:Char.equal
    |> of_char_groups table

  let subtraction : system -> code -> accumulator -> accumulator option =
   fun system code acc ->
    let dist = code.value - acc.remainder in
    let suitable last c =
      match last with
      | Some result ->
          Some result
      | None ->
          let v = c.value in
          if code.value - (v * system.msl) <= acc.remainder then
            let reps = if dist mod v = 0 then dist / v else (dist / v) + 1 in
            let remainder = (v * reps) - dist in
            if reps = 1 || system.repeatable c then
              Some {numeral= [repeat reps c]; remainder}
            else None
          else None
    in
    List.fold_left ~f:suitable (system.subtractors code) ~init:None

  let encode_additive : system -> accumulator -> accumulator =
   fun system acc ->
    let closest_lower =
      system.table.desc |> List.find ~f:(fun c -> acc.remainder / c.value > 0)
    in
    { numeral= [repeat 1 closest_lower]
    ; remainder= acc.remainder - closest_lower.value }

  let encode_subtractive : system -> accumulator -> accumulator option =
   fun system acc ->
    system.table.asc
    |> List.find_opt ~f:(fun code -> code.value >= acc.remainder)
    |> Option.flat_map (fun code ->
           if code.value - acc.remainder = 0 then
             Some {numeral= [repeat 1 code]; remainder= 0}
           else
             subtraction system code acc
             |> Option.flat_map (fun r ->
                    Some {r with numeral= repeat 1 code :: r.numeral} ) )

  let rec _encode system acc =
    if acc.remainder = 0 then Some acc.numeral
    else
      let result =
        Option.get_or
          (encode_subtractive system acc)
          ~default:(encode_additive system acc)
      in
      let numeral = List.concat [result.numeral; acc.numeral] in
      _encode system {result with numeral}

  let of_int system n = _encode system {numeral= []; remainder= n}
end

let decode ~table string =
  string |> Numeral.of_string table
  |> function
  | Some n -> Ok (Numeral.to_int n) | None -> Error "Invalid numeral"

let encode ~system arabic =
  if arabic < 0 then Error "Negative numbers are not supported"
  else if system.msd < 1 then Error "Invalid system"
  else
    arabic |> Numeral.of_int system
    |> function
    | Some n -> Ok (Numeral.to_string n) | None -> Error "Insufficient system"

let make_decoder table = decode ~table

let make_encoder table msd msl = encode ~system:(System.make table msd msl)

(* $MDX part-begin=Roman *)
module Roman = struct
  let table =
    Table.make
      [ ('I', 1)
      ; ('V', 5)
      ; ('X', 10)
      ; ('L', 50)
      ; ('C', 100)
      ; ('D', 500)
      ; ('M', 1_000) ]

  let to_int = make_decoder table

  let of_int ?c:(compression_level = 0) =
    make_encoder table (compression_level + 1) 1
end
(* $MDX part-end *)
