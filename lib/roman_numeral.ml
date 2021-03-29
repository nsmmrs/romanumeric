open Containers

module Glyph = struct
  type t = I | V | X | L | C | D | M

  type sub_memo = {t: t; value: int}

  type glyph_memo =
    {t: t; value: int; repeatable: bool; subtractors: sub_memo list}

  type config_memo = {msd: int; msl: int; glyphs: glyph_memo list}

  let next = function
    | I -> Some V
    | V -> Some X
    | X -> Some L
    | L -> Some C
    | C -> Some D
    | D -> Some M
    | M -> None

  let value = function
    | I -> 1
    | V -> 5
    | X -> 10
    | L -> 50
    | C -> 100
    | D -> 500
    | M -> 1_000

  let from_char = function
    | 'I' -> Some I
    | 'V' -> Some V
    | 'X' -> Some X
    | 'L' -> Some L
    | 'C' -> Some C
    | 'D' -> Some D
    | 'M' -> Some M
    | ___ -> None

  let to_char = function
    | I -> 'I'
    | V -> 'V'
    | X -> 'X'
    | L -> 'L'
    | C -> 'C'
    | D -> 'D'
    | M -> 'M'

  let all =
    let rec build_list_from ?(acc = []) glyph =
      let acc = glyph :: acc in
      match next glyph with
      | None -> acc
      | Some glyph -> build_list_from glyph ~acc
    in
    build_list_from I |> List.rev

  let list_from ~string:s =
    String.(trim s |> to_list) |> List.map from_char |> List.all_some

  let equal = Equal.physical

  let descending = List.rev all

  let closest_lower num = descending |> List.find (fun g -> num / value g > 0)

  let repeat g n = List.init (Int.abs n) (fun _ -> g)

  let repeatable g =
    all
    |> List.find_opt (fun other -> value g * 2 = value other)
    |> function
    | None -> true
    | Some _ -> false

  let valid_subtractors ~msd high_val =
    let valid acc g =
      if List.length acc = msd then acc
      else
        let v = value g in
        match v >= high_val with
        | true -> acc
        | false -> (
          match v * 2 = high_val with
          | true -> acc
          | false -> g :: acc )
    in
    List.fold_left valid [] descending

  let subtractor ~config ~(high : glyph_memo) ~target =
    let dist = high.value - target in
    let suitable (last : (t list * int) option) (g : sub_memo) =
      match last with
      | Some s -> Some s
      | None -> (
          let v = g.value in
          match high.value - (v * config.msl) <= target with
          | false -> None
          | true -> (
              let reps =
                match dist mod v = 0 with
                | true -> dist / v
                | false -> (dist / v) + 1
              in
              let rem = (v * reps) - dist in
              match reps = 1 with
              | true -> Some ([g.t], rem)
              | false -> (
                match high.repeatable with
                | false -> None
                | true -> Some (repeat g.t reps, rem) ) ) )
    in
    List.fold_left suitable None high.subtractors

  let sub_memoize g = {t= g; value= value g}

  let memoize ~msd g =
    let value = value g in
    let subtractors = List.map sub_memoize (valid_subtractors ~msd value) in
    {t= g; value; repeatable= repeatable g; subtractors}

  let rec encode_part ~config ?(acc = []) num =
    if num = 0 then List.concat acc |> List.rev
    else
      let chunk, rem =
        match encode_subtractive ~config num with
        | Some result -> result
        | None -> encode_additive num
      in
      encode_part ~config ~acc:(chunk :: acc) rem

  and encode_subtractive ~config target =
    config.glyphs
    |> List.find_opt (fun (g : glyph_memo) -> g.value >= target)
    |> function
    | None -> None
    | Some high -> (
        if high.value - target = 0 then Some ([high.t], 0)
        else
          match subtractor ~high ~target ~config with
          | None -> None
          | Some (sub, rem) -> Some (high.t :: sub, rem) )

  and encode_additive num =
    let g = closest_lower num in
    ([g], num - value g)

  let start_encode ~msd ~msl arabic =
    let glyphs = List.map (memoize ~msd) all in
    let config = {msd; msl; glyphs} in
    encode_part ~config arabic
end

let encode ?(msd = 1) ?(msl = 1) arabic =
  Glyph.start_encode ~msd ~msl arabic
  |> List.map Glyph.to_char
  |> String.of_list

module Part = struct
  let value (glyph, repetition) = Glyph.value glyph * repetition

  let plus_or_minus ((glyph, _) as part) ~given:(next_glyph, _) =
    let v = value part in
    if Glyph.(value glyph < value next_glyph) then -v else v

  let list glyphs =
    let open List in
    group_succ glyphs ~eq:Glyph.equal
    |> map (function
         | glyph :: _ as group -> Some (glyph, length group)
         | _ -> None )
    |> keep_some

  let rec sum ?(acc = 0) parts =
    match parts with
    | [] -> acc
    | first :: remaining -> (
      match remaining with
      | next :: _ ->
          let addend = plus_or_minus first ~given:next in
          sum remaining ~acc:(acc + addend)
      | empty -> sum empty ~acc:(acc + value first) )
end

let decode string =
  match Glyph.list_from ~string with
  | None -> 0
  | Some glyphs -> Part.(list glyphs |> sum)
