open Containers

module Glyph = struct
  type t = I | V | X | L | C | D | M

  type closest =
    | Exactly of t
    | Above of (t * int)
    | Between of (t * int) * (t * int)

  let next = function
    | I -> Some V
    | V -> Some X
    | X -> Some L
    | L -> Some C
    | C -> Some D
    | D -> Some M
    | M -> None

  let all =
    let rec build_list glyphs glyph =
      let glyphs = glyph :: glyphs in
      match next glyph with
      | None -> glyphs
      | Some glyph -> build_list glyphs glyph
    in
    build_list [] I |> List.rev

  let value = function
    | I -> 1
    | V -> 5
    | X -> 10
    | L -> 50
    | C -> 100
    | D -> 500
    | M -> 1_000

  let rec is_power_of m = function
    | n when n = 1 -> true
    | n when n mod m <> 0 -> false
    | n -> is_power_of m (n / m)

  let power_of n g = value g |> is_power_of n

  let powers_of_ten = List.filter (power_of 10)

  let of_char = function
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

  let list s = String.(trim s |> to_list) |> List.map of_char |> List.all_some

  let string l = List.map to_char l |> String.of_list

  let equal = Equal.physical

  let descending = List.rev all

  let highest_denominator n =
    descending |> List.find (fun g -> n mod value g = 0)

  let denomination n = value (highest_denominator n)

  let closest_higher num = all |> List.find_opt (fun g -> value g / num = 1)

  let exact_match num = all |> List.find_opt (fun g -> value g = num)

  let closest_lower num = descending |> List.find (fun g -> num / value g > 0)

  let repeat g n = List.init (Int.abs n) (fun _ -> g)

  let repeatable g =
    all
    |> List.find_opt (fun other -> value g * 2 = value other)
    |> function
    | None -> true
    | Some _ -> false

  let flat_fill num =
    let g = highest_denominator num in
    repeat g (num / value g)

  let remaining_ones n =
    if n < 10 then None
    else
      match (n mod 10, n mod 5) with
      | 5, 0 | 0, 0 -> None
      | ones -> Some ones

  let subtractor high_val num msl =
    let dist = high_val - num in
    let suitable last next =
      match last with
      | Some s -> Some s
      | None -> (
          let v = value next in
          if v * 2 = high_val then None
          else if v >= high_val then None
          else
            match high_val - (v * msl) <= num with
            | false -> None
            | true -> (
              match dist mod v = 0 with
              | true -> (
                  let reps = dist / v in
                  match reps = 1 with
                  | true -> Some ([next], 0)
                  | false -> (
                    match repeatable next with
                    | false -> None
                    | true -> Some (repeat next reps, 0) ) )
              | false -> (
                  let reps = (dist / v) + 1 in
                  let rem = (v * reps) - dist in
                  match reps = 1 with
                  | true -> Some ([next], rem)
                  | false -> (
                    match repeatable next with
                    | false -> None
                    | true -> Some (repeat next reps, rem) ) ) ) )
    in
    List.fold_left suitable None all

  let rec encode_part ?e:(encoded : t list list = []) ?(msl = 1) num =
    if num = 0 then List.concat encoded |> List.rev
    else
      let chunk, rem =
        match encode_subtractive ~msl num with
        | Some result -> result
        | None -> encode_additive num
      in
      encode_part ~e:(chunk :: encoded) ~msl rem

  and encode_subtractive ~msl num =
    let () = print_endline @@ string_of_int num in
    all
    |> List.find_opt (fun g -> value g >= num)
    |> function
    | None -> None
    | Some g -> (
        let high_val = value g in
        let dist = high_val - num in
        if dist = 0 then Some ([g], 0)
        else
          match subtractor high_val num msl with
          | None -> None
          | Some (sub, rem) -> Some (g :: sub, rem) )

  and encode_additive num =
    let g = closest_lower num in
    ([g], num - value g)
end

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
  match Glyph.list string with
  | None -> 0
  | Some glyphs -> Part.(list glyphs |> sum)

let breakdown num denominations =
  let rec break ?(acc = []) n = function
    | [] ->
        let result = n :: acc in
        let () =
          List.iter (fun i -> print_int i ; print_string " ") result ;
          print_endline ""
        in
        result
    | d :: denominations ->
        let r = n mod d in
        let acc = r :: acc in
        break (n - r) denominations ~acc
  in
  break num denominations |> List.filter (fun n -> n > 0)

let%test _ = List.equal ( = ) (breakdown 3999 [10; 100; 1000]) [3000; 900; 90; 9]

let%test _ = List.equal ( = ) (breakdown 3999 [5; 50; 1000]) [3000; 950; 45; 4]

let%test _ = List.equal ( = ) (breakdown 18 [10; 100; 1000]) [10; 8]

let walk glyphs num =
  let denominations = List.map Glyph.value glyphs in
  breakdown num denominations

let collapse_small_numbers parts =
  let parts = List.rev parts in
  let collapsed =
    match parts with
    | 4 :: v :: rem -> (
      match Glyph.denomination v with
      | 5 -> (
          List.find_opt (fun n -> n = v) [495; 995]
          |> function
          | Some _ -> parts
          | None -> (v + 4) :: rem )
      | _ -> parts )
    | _ -> parts
  in
  List.rev collapsed

let encode ?(glyphs = Glyph.(all |> powers_of_ten)) ?(msl = 1) arabic =
  walk glyphs arabic
  |> collapse_small_numbers
  |> List.map (Glyph.encode_part ~msl)
  |> List.concat
  |> List.map Glyph.to_char
  |> String.of_list
