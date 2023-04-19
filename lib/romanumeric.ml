type code = {symbol: char; value: int; repeatable: bool}

let repeatable value symbols_values =
  symbols_values
  |> List.find_opt ~f:(fun (_, other) -> value * 2 = other)
  |> Option.is_none
let make_code (symbol, value) ~symbols_values =
  {symbol; value; repeatable= repeatable value symbols_values}

type table = code list

let make_table symbols_values =
  let symbols_values =
    List.sort symbols_values ~cmp:(fun (_, a) (_, b) -> Int.compare a b)
  in
  symbols_values |> List.map ~f:(make_code ~symbols_values)

let compare a b = Int.compare a.value b.value
let sort_asc : table -> table = List.sort ~cmp:compare
let sort_desc : table -> table = List.sort ~cmp:(Fun.flip compare)

type repetition = {code: code; length: int}

let repeat code length = {code; length}

type numeral = repetition list

type memo = {code: code; subtractors: table}

let subtractors code table msd =
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
  List.fold_left ~f:valid (sort_desc table) ~init:[]

let make_memos table ~msd =
  List.map table ~f:(fun code ->
      {code; subtractors= subtractors code table msd} )

type system = {table: table; memos: memo list; msl: int; msd: int}

let make_system table msd msl =
  let memos = make_memos table ~msd in
  {table; memos; msd; msl}

type accumulator = {numeral: numeral; remainder: int}

let chars_of_repetition {code; length} =
  List.init (Int.abs length) ~f:(fun _ -> code.symbol)

let string_of_numeral (numeral : numeral) : string =
  numeral |> List.rev
  |> List.map ~f:chars_of_repetition
  |> List.concat |> String.of_list

let subtraction ~(code : code) ~(subtractors : table) ~(target : int)
    ~(msl : int) : accumulator option =
  let dist = code.value - target in
  let suitable last c =
    match last with
    | Some result ->
        Some result
    | None -> (
        let v = c.value in
        match code.value - (v * msl) <= target with
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
                Some {numeral= [repeat c 1]; remainder= rem}
            | false -> (
              match c.repeatable with
              | false ->
                  None
              | true ->
                  Some {numeral= [repeat c reps]; remainder= rem} ) ) )
  in
  List.fold_left ~f:suitable subtractors ~init:None

let rec _encode (system : system) (acc : accumulator) : string =
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

and encode_subtractive (system : system) (acc : accumulator) :
    accumulator option =
  system.memos
  |> List.find_opt ~f:(fun (m : memo) -> m.code.value >= acc.remainder)
  |> function
  | None ->
      None
  | Some {code; subtractors} -> (
      if code.value - acc.remainder = 0 then
        Some {numeral= [repeat code 1]; remainder= 0}
      else
        match
          subtraction ~code ~subtractors ~target:acc.remainder ~msl:system.msl
        with
        | None ->
            None
        | Some r ->
            Some {numeral= repeat code 1 :: r.numeral; remainder= r.remainder} )

and encode_additive system acc =
  let closest_lower =
    system.table |> sort_desc
    |> List.find ~f:(fun c -> acc.remainder / c.value > 0)
  in
  { numeral= [repeat closest_lower 1]
  ; remainder= acc.remainder - closest_lower.value }

let plus_or_minus (code, reps) ~given:(next_code, _) =
  let v = code.value * reps in
  print_endline (string_of_int v) ;
  print_endline (string_of_int code.value) ;
  print_endline (string_of_int next_code.value) ;
  if code.value < next_code.value then -v else v

let rec sum ?(acc = 0) parts =
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

let decode ~table string =
  let codes_by_symbol =
    List.map table ~f:(fun ({symbol; _} as c) -> (symbol, c)) |> Hashtbl.of_list
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

let make_decoder table = decode ~table

let encode ~system (arabic : int) = _encode system {numeral= []; remainder= arabic}

let make_encoder table msd msl =
   encode ~system:(make_system table msd msl)

module Roman = struct
  let table : table =
    make_table
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
