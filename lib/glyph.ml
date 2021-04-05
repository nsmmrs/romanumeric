type t = {char: char; value: int}

type group = {glyph: t; length: int}

let compare a b = Int.compare a.value b.value

let sort_asc = List.sort ~cmp:compare

let sort_desc = List.sort ~cmp:(Fun.flip compare)

let repeatable ~glyphs glyph =
  glyphs
  |> List.find_opt ~f:(fun other -> glyph.value * 2 = other.value)
  |> Option.is_none

let singular glyph = {glyph; length= 1}

let make_group ~glyphs glyph length =
  if length = 1 || repeatable ~glyphs glyph then Some {glyph; length} else None

let valid_subtractor_for glyph other =
  other.value < glyph.value && other.value * 2 <> glyph.value

let nearest_gte ~glyphs target =
  glyphs |> sort_asc |> List.find_opt ~f:(fun g -> g.value >= target)

let nearest_lower ~glyphs target =
  glyphs |> sort_desc |> List.find_opt ~f:(fun g -> target / g.value > 0)

let subtractors ~glyphs ~msd:n glyph =
  glyphs |> sort_desc |> Utils.filter_take n ~f:(valid_subtractor_for glyph)

let subtraction ~glyphs ~msl ~msd glyph target =
  let dist = glyph.value - target in
  let suitable sub =
    let sv = sub.value in
    if glyph.value - (sv * msl) <= target then
      let len =
        match dist mod sv = 0 with
        | true -> dist / sv
        | false -> (dist / sv) + 1
      in
      let rem = (sv * len) - dist in
      make_group ~glyphs sub len |> Option.map (fun group -> (group, rem))
    else None
  in
  List.find_map ~f:suitable (subtractors ~glyphs ~msd glyph)

let chars_of_group {glyph= {char; _}; length} =
  List.init length ~f:(fun _ -> char)

let encode_subtractive ~glyphs ~msd ~msl target =
  nearest_gte ~glyphs target
  |> function
  | Some g ->
      let base = singular g in
      if g.value = target then ([base], 0)
      else
        subtraction ~glyphs ~msd ~msl g target
        |> Option.map (fun (sub, rem) -> ([base; sub], rem))
  | None -> None

let encode_additive ~glyphs target =
  nearest_lower ~glyphs target
  |> Option.map (fun g -> (singular g, target - g.value))

let rec encode ~glyphs ~msd ~msl ?(groups = []) target =
  if target = 0 then
    List.(groups |> rev |> map ~f:chars_of_group |> concat) |> String.of_list
  else
    let groups, remainder =
      match nearest_gte ~glyphs target with
      | Some g -> (
          let base = singular g in
          if g.value = target then (base :: groups, 0)
          else
            match subtraction ~glyphs ~msd ~msl g target with
            | Some (sub, rem) -> (base :: sub :: groups, rem)
            | None -> encode_additive ~glyphs target )
      | None -> failwith "lol"
    in
    encode ~glyphs ~msd ~msl ~groups remainder

let make_glyph (char, value) = {char; value}

let make_glyphs chars_values = chars_values |> List.map ~f:make_glyph

let make_encoder chars_values msd msl =
  encode ~glyphs:(make_glyphs chars_values) ~msd ~msl

let plus_or_minus (glyph, reps) ~given:(next_glyph, _) =
  let v = glyph.value * reps in
  if glyph.value < next_glyph.value then -v else v

let rec sum ?(acc = 0) parts =
  match parts with
  | [] -> acc
  | ((glyph, reps) as first) :: remaining -> (
    match remaining with
    | next :: _ ->
        let addend = plus_or_minus first ~given:next in
        sum remaining ~acc:(acc + addend)
    | empty -> sum empty ~acc:(acc + (glyph.value * reps)) )

let decode ~glyphs string =
  let glyphs_by_char =
    List.map glyphs ~f:(fun ({char; _} as g) -> (char, g)) |> Hashtbl.of_list
  in
  let glyph_of_char char = Hashtbl.get glyphs_by_char char in
  let acc parts group =
    match parts with
    | None -> None
    | Some parts -> (
      match group with
      | [] -> Some parts
      | char :: _ -> (
        match glyph_of_char char with
        | Some g -> Some ((g, List.length group) :: parts)
        | None -> None ) )
  in
  string
  |> String.to_list
  |> List.group_succ ~eq:Char.equal
  |> List.fold_left ~f:acc ~init:(Some [])
  |> function
  | Some parts -> sum (List.rev parts)
  | None -> 0

let make_decoder chars_values =
  let glyphs = chars_values |> make_glyphs in
  decode ~glyphs
