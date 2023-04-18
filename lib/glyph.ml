type code = {symbol: char; value: int; repeatable: bool}

type repetition = { code: code; length: int }

type table = code list

type memo = {code: code; subtractors: table}

type system = {table: table; memos: memo list; msl: int; msd: int}

let compare a b = Int.compare a.value b.value

let sort_asc = List.sort ~cmp:compare

let sort_desc = List.sort ~cmp:(Fun.flip compare)

let repeat code length = { code; length }
  
let chars_of_repetition (repetition:repetition) : char list = List.init (Int.abs repetition.length) ~f:(fun _ -> repetition.code.symbol)

let subtraction ~(code:code) ~(subtractors:table) ~(target:int) ~(msl:int) : (repetition list * int) option =
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
                Some ([repeat c 1], rem)
            | false -> (
              match c.repeatable with
              | false ->
                  None
              | true ->
                  Some ([repeat c reps], rem) ) ) )
  in
  List.fold_left ~f:suitable subtractors ~init:None

let rec encode ~system ?(acc = []) num =
  if num = 0 then acc |> List.rev |> List.map ~f:chars_of_repetition |> List.concat |> String.of_list
  else
    let chunk, rem =
      match encode_subtractive ~system num with
      | Some result ->
          result
      | None ->
          encode_additive num system.table
    in
    encode ~system ~acc:(List.concat [chunk; acc]) rem

and encode_subtractive ~(system:system) (target:int) : (repetition list * int) option =
  system.memos
  |> List.find_opt ~f:(fun (m:memo) -> m.code.value >= target)
  |> function
  | None ->
      None
  | Some {code; subtractors} -> (
      if code.value - target = 0 then Some ([repeat code 1], 0)
      else
        match subtraction ~code ~subtractors ~target ~msl:system.msl with
        | None ->
            None
        | Some (repetitions, rem) ->
            Some (repeat code 1 :: repetitions, rem) )

and encode_additive num table =
  let closest_lower =
    table |> sort_desc |> List.find ~f:(fun c -> num / c.value > 0)
  in
  ([repeat closest_lower 1], num - closest_lower.value)

let repeatable value symbols_values =
  symbols_values
  |> List.find_opt ~f:(fun (_, other) -> value * 2 = other)
  |> Option.is_none

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

let make_code (symbol, value) ~symbols_values =
  {symbol; value; repeatable= repeatable value symbols_values}

let make_table symbols_values =
  let symbols_values =
    List.sort symbols_values ~cmp:(fun (_, a) (_, b) -> Int.compare a b)
  in
  symbols_values |> List.map ~f:(make_code ~symbols_values)

let make_memos table ~msd =
  List.map table ~f:(fun code ->
      {code; subtractors= subtractors code table msd} )

let make_encoder symbols_values msd msl =
  let table = symbols_values |> make_table in
  let memos = make_memos table ~msd in
  let system = {table; memos; msd; msl} in
  encode ~system

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

let make_decoder symbols_values =
  let table = symbols_values |> make_table in
  decode ~table
