type glyph = {char: char; value: int; repeatable: bool}

type memo = {glyph: glyph; subtractors: glyph list}

type config = {glyphs: glyph list; memos: memo list; msl: int; msd: int}

let compare a b = Int.compare a.value b.value

let sort_asc = List.sort ~cmp:compare

let sort_desc = List.sort ~cmp:(Fun.flip compare)

let repeat x n = List.init (Int.abs n) ~f:(fun _ -> x)

let subtraction ~glyph ~subtractors ~target ~msl =
  let dist = glyph.value - target in
  let suitable last g =
    match last with
    | Some result ->
        Some result
    | None -> (
        let v = g.value in
        match glyph.value - (v * msl) <= target with
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
                Some ([g.char], rem)
            | false -> (
              match g.repeatable with
              | false ->
                  None
              | true ->
                  Some (repeat g.char reps, rem) ) ) )
  in
  List.fold_left ~f:suitable subtractors ~init:None

let rec encode ~config ?(acc = []) num =
  if num = 0 then List.concat acc |> List.rev |> String.of_list
  else
    let chunk, rem =
      match encode_subtractive ~config num with
      | Some result ->
          result
      | None ->
          encode_additive num config.glyphs
    in
    encode ~config ~acc:(chunk :: acc) rem

and encode_subtractive ~config target =
  config.memos
  |> List.find_opt ~f:(fun m -> m.glyph.value >= target)
  |> function
  | None ->
      None
  | Some {glyph; subtractors} -> (
      if glyph.value - target = 0 then Some ([glyph.char], 0)
      else
        match subtraction ~glyph ~subtractors ~target ~msl:config.msl with
        | None ->
            None
        | Some (chars, rem) ->
            Some (glyph.char :: chars, rem) )

and encode_additive num glyphs =
  let closest_lower =
    glyphs |> sort_desc |> List.find ~f:(fun g -> num / g.value > 0)
  in
  ([closest_lower.char], num - closest_lower.value)

let repeatable value chars_values =
  chars_values
  |> List.find_opt ~f:(fun (_, other) -> value * 2 = other)
  |> Option.is_none

let subtractors glyph glyphs msd =
  let valid subs other =
    if List.length subs = msd then subs
    else
      match other.value >= glyph.value with
      | true ->
          subs
      | false -> (
        match other.value * 2 = glyph.value with
        | true ->
            subs
        | false ->
            other :: subs )
  in
  List.fold_left ~f:valid (sort_desc glyphs) ~init:[]

let make_glyph (char, value) ~chars_values =
  {char; value; repeatable= repeatable value chars_values}

let make_glyphs chars_values =
  let chars_values =
    List.sort chars_values ~cmp:(fun (_, a) (_, b) -> Int.compare a b)
  in
  chars_values |> List.map ~f:(make_glyph ~chars_values)

let make_memos glyphs ~msd =
  List.map glyphs ~f:(fun glyph ->
      {glyph; subtractors= subtractors glyph glyphs msd} )

let make_encoder chars_values msd msl =
  let glyphs = chars_values |> make_glyphs in
  let memos = make_memos glyphs ~msd in
  let config = {glyphs; memos; msd; msl} in
  encode ~config

let plus_or_minus (glyph, reps) ~given:(next_glyph, _) =
  let v = glyph.value * reps in
  print_endline (string_of_int v) ;
  print_endline (string_of_int glyph.value) ;
  print_endline (string_of_int next_glyph.value) ;
  if glyph.value < next_glyph.value then -v else v

let rec sum ?(acc = 0) parts =
  match parts with
  | [] ->
      acc
  | ((glyph, reps) as first) :: remaining -> (
    match remaining with
    | next :: _ ->
        let addend = plus_or_minus first ~given:next in
        sum remaining ~acc:(acc + addend)
    | empty ->
        sum empty ~acc:(acc + (glyph.value * reps)) )

let decode ~glyphs string =
  let glyphs_by_char =
    List.map glyphs ~f:(fun ({char; _} as g) -> (char, g)) |> Hashtbl.of_list
  in
  let glyph_of_char char = Hashtbl.get glyphs_by_char char in
  let acc parts group =
    match parts with
    | None ->
        None
    | Some parts -> (
      match group with
      | [] ->
          Some parts
      | char :: _ -> (
        match glyph_of_char char with
        | Some g ->
            Some ((g, List.length group) :: parts)
        | None ->
            None ) )
  in
  string |> String.to_list
  |> List.group_succ ~eq:Char.equal
  |> List.fold_left ~f:acc ~init:(Some [])
  |> function Some parts -> sum (List.rev parts) | None -> 0

let make_decoder chars_values =
  let glyphs = chars_values |> make_glyphs in
  decode ~glyphs
