type code = {symbol: char; value: int}

type code_repetition = {code: code; length: int}

type table = code list

type table_memo = {asc: table; desc: table}

type numeral = code_repetition list

type accumulator = {numeral: numeral; remainder: int}

type system =
  { table: table_memo
  ; repeatable: code -> bool
  ; subtractors: code -> table
  ; msl: int
  ; msd: int }

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
    table
    |> List.find_opt ~f:(fun other -> code.value * 2 = other.value)
    |> Option.is_none
end

module CodeMap = Map.Make (Code)

type code_table_map = table CodeMap.t

let repeat = Code.repeat

module Table : sig
  type t = table

  type memo = table_memo

  val make : (char * int) list -> t

  val sort : t -> t

  val exists : code -> table -> bool

  val memoize : t -> memo
end = struct
  type t = table

  type memo = table_memo

  let sort = List.sort ~cmp:Code.compare

  let make symbols_values = symbols_values |> List.map ~f:Code.make |> sort

  let exists (code : code) (table : table) =
    List.exists table ~f:(fun other -> other.value = code.value)

  let memoize table = {asc= table; desc= List.rev table}
end

module System : sig
  type t = system

  val make : table -> int -> int -> t

  val repeatable : table_memo -> code -> bool

  val subtractors : table_memo -> int -> code -> table
end = struct
  type t = system

  let repeatable table code = Table.exists code table.asc

  let subtractors_for code table msd =
    let valid subs other =
      if List.length subs = msd then subs
      else
        match other.value >= code.value with
        | true ->
            subs
        | false -> (
          match other.value * 2 = code.value with
          | true ->
              subs
          | false ->
              other :: subs )
    in
    List.fold_left ~f:valid table.desc ~init:[]

  let memoize_subtractors table msd : code_table_map =
    List.fold_left table.asc ~init:CodeMap.empty ~f:(fun map code ->
        CodeMap.add code (subtractors_for code table msd) map )

  let subtractors table msd =
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

let chars_of_repetition {code; length} =
  List.init (Int.abs length) ~f:(fun _ -> code.symbol)

let string_of_numeral (numeral : numeral) : string =
  numeral |> List.rev
  |> List.map ~f:chars_of_repetition
  |> List.concat |> String.of_list

let subtraction : system -> code -> accumulator -> accumulator option = 
fun system code acc ->
  let dist = code.value - acc.remainder in
  let suitable last c =
    match last with
    | Some result ->
        Some result
    | None -> (
        let v = c.value in
        match code.value - (v * system.msl) <= acc.remainder with
        | false ->
            None
        | true -> (
            let reps =
              match dist mod v = 0 with
              | true ->
                  dist / v
              | false ->
                  (dist / v) + 1
            in
            let rem = (v * reps) - dist in
            match reps = 1 with
            | true ->
                Some {numeral= [repeat 1 c]; remainder= rem}
            | false -> (
              match system.repeatable c with
              | false ->
                  None
              | true ->
                  Some {numeral= [repeat reps c]; remainder= rem} ) ) )
  in
  List.fold_left ~f:suitable (system.subtractors code) ~init:None

let encode_additive : system -> accumulator -> accumulator = fun system acc ->
  let closest_lower =
    system.table.desc |> List.find ~f:(fun c -> acc.remainder / c.value > 0)
  in
  { numeral= [repeat 1 closest_lower]
  ; remainder= acc.remainder - closest_lower.value }

let encode_subtractive : system -> accumulator -> accumulator option =
 fun system acc ->
  system.table.asc
  |> List.find_opt ~f:(fun code -> code.value >= acc.remainder)
  |> function
  | None ->
      None
  | Some code -> (
      if code.value - acc.remainder = 0 then
        Some {numeral= [repeat 1 code]; remainder= 0}
      else
        match subtraction system code acc with
        | None ->
            None
        | Some r ->
            Some {numeral= repeat 1 code :: r.numeral; remainder= r.remainder} )

let rec _encode : system -> accumulator -> string =
 fun system acc ->
  if acc.remainder = 0 then string_of_numeral acc.numeral
  else
    let r =
      match encode_subtractive system acc with
      | Some result ->
          result
      | None ->
          encode_additive system acc
    in
    _encode system
      {numeral= List.concat [r.numeral; acc.numeral]; remainder= r.remainder}

let plus_or_minus : code * int -> given:code * 'a -> int = fun (code, reps) ~given:(next_code, _) ->
  let v = code.value * reps in
  if code.value < next_code.value then -v else v

let rec sum : ?acc:int -> (code * int) list -> int =
  fun ?(acc = 0) parts ->
  match parts with
  | [] ->
      acc
  | ((code, reps) as first) :: remaining -> (
    match remaining with
    | next :: _ ->
        let addend = plus_or_minus first ~given:next in
        sum remaining ~acc:(acc + addend)
    | empty ->
        sum empty ~acc:(acc + (code.value * reps)) )

let decode : table:table -> string -> int =
 fun ~table string ->
  let table = Table.memoize table in
  let codes_by_symbol =
    List.map table.asc ~f:(fun ({symbol; _} as c) -> (symbol, c))
    |> Hashtbl.of_list
  in
  let code_of_symbol symbol = Hashtbl.get codes_by_symbol symbol in
  let acc parts group =
    match parts with
    | None ->
        None
    | Some parts -> (
      match group with
      | [] ->
          Some parts
      | symbol :: _ -> (
        match code_of_symbol symbol with
        | Some c ->
            Some ((c, List.length group) :: parts)
        | None ->
            None ) )
  in
  string |> String.to_list
  |> List.group_succ ~eq:Char.equal
  |> List.fold_left ~f:acc ~init:(Some [])
  |> function Some parts -> sum (List.rev parts) | None -> 0

let make_decoder : table -> string -> int = fun table -> decode ~table

let encode : system:system -> int -> string =
 fun ~system arabic -> _encode system {numeral= []; remainder= arabic}

let make_encoder : table -> int -> int -> int -> string =
 fun table msd msl -> encode ~system:(System.make table msd msl)

module Roman = struct
  let table : table =
    Table.make
      [ ('I', 1)
      ; ('V', 5)
      ; ('X', 10)
      ; ('L', 50)
      ; ('C', 100)
      ; ('D', 500)
      ; ('M', 1_000) ]

  let decode = make_decoder table

  let conventional = make_encoder table 1 1

  let compressed lvl =
    match lvl with
    | 1 | 2 | 3 | 4 ->
        make_encoder table (lvl + 1) 1
    | _ ->
        failwith "unsupported option"
end
