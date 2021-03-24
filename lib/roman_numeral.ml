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
    let rec accumulate glyphs glyph =
      let glyphs = glyph :: glyphs in
      match next glyph with
      | None -> glyphs
      | Some glyph -> accumulate glyphs glyph
    in
    accumulate [] I |> List.rev

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

  let equal = Equal.physical

  let descending = List.rev all

  let highest_denominator n =
    descending |> List.find (fun g -> n mod value g = 0)

  let denomination n = value (highest_denominator n)

  let _denomination ?(f = fun _ -> true) n =
    let g =
      descending |> List.filter f |> List.find (fun g -> n mod value g = 0)
    in
    value g

  let decimal_denomination n = _denomination ~f:(power_of 10) n

  let same_denomination ints =
    ints
    |> List.map decimal_denomination
    |> List.uniq ~eq:( = )
    |> function
    | [1] -> false
    | [_] -> true
    | ___ -> false

  let closest_higher num = all |> List.find_opt (fun g -> value g / num = 1)

  let exact_match num = all |> List.find_opt (fun g -> value g = num)

  let closest_lower num = descending |> List.find (fun g -> num / value g > 0)

  let repeat g n = List.init (Int.abs n) (fun _ -> g)

  let flat_fill num =
    let g = highest_denominator num in
    repeat g (num / value g)

  let remaining_ones n =
    if n < 10 then None
    else
      match (n mod 10, n mod 5) with
      | 5, 0 | 0, 0 -> None
      | ones -> Some ones

  let encode_subtractive ~encoded ~msl num =
    match closest_higher num with
    | None -> None
    | Some g ->
        let () =
          print_endline ("closest higher is " ^ String.of_list [to_char g])
        in
        let rem = value g mod num in
        let sub = flat_fill rem in
        if List.length sub > msl then None
        else Some List.(append (g :: sub) encoded |> rev)

  (*

    let () =
      print_endline List.(map to_char encoded |> rev |> String.of_list)
    in
    let g = closest_lower num in
    let normal = encode_part ~e:(g :: encoded) ~msl (num - value g) in
    match remaining_ones num with
    | None -> normal
    | Some (lt10, lt5) -> (
      match (lt10 = lt5, lt5 = 0) with
      | false, false ->
          let () = print_endline (string_of_int lt10) in
          let () = print_endline (string_of_int lt5) in
          let a = encode_part ~e:encoded ~msl (num - lt10) in
          let b = encode_part ~e:encoded ~msl (num - lt5) in
          let ab, rem =
            if List.(length a < length b) then (a, lt10) else (b, lt5)
          in
          if List.(length ab < length normal) then
            encode_part ~e:(List.rev ab) ~msl rem
          else normal
      | _ ->
          let () = print_endline (string_of_int lt10) in
          let first_chunk = encode_part ~e:encoded ~msl (num - lt10) in
          encode_part ~e:(List.rev first_chunk) ~msl lt10 )
*)

  let rec encode_part ?e:(encoded = []) ?(msl = 1) rem =
    if rem = 0 then List.rev encoded
    else
      match exact_match rem with
      | Some g -> encode_part ~e:(g :: encoded) ~msl 0
      | None -> (
          let add = encode_additive ~e:encoded ~msl rem in
          match encode_subtractive ~encoded ~msl rem with
          | None -> add
          | Some sub -> if List.(length add < length sub) then add else sub )

  (* and encode_additive ~e:encoded ~msl num =
     let g = closest_lower num in
     let rem_ones = num mod 10 in
     match (rem_ones < num, rem_ones = 0) with
     | true, false ->
         let first_chunk = encode_part ~e:encoded ~msl (num - rem_ones) in
         encode_part ~e:(List.rev first_chunk) ~msl rem_ones
     | _ -> encode_part ~e:(g :: encoded) ~msl (num - value g) *)
  and encode_additive ~e:encoded ~msl num =
    let () =
      print_endline List.(map to_char encoded |> rev |> String.of_list)
    in
    let g = closest_lower num in
    let normal = encode_part ~e:(g :: encoded) ~msl (num - value g) in
    match remaining_ones num with
    | None -> normal
    | Some (lt10, lt5) -> (
      match (lt10 = lt5, lt5 = 0) with
      | false, false ->
          let () = print_endline (string_of_int lt10) in
          let () = print_endline (string_of_int lt5) in
          let a = encode_part ~e:encoded ~msl (num - lt10) in
          let b = encode_part ~e:encoded ~msl (num - lt5) in
          let ab, rem =
            if List.(length a < length b) then (a, lt10) else (b, lt5)
          in
          if List.(length ab < length normal) then
            encode_part ~e:(List.rev ab) ~msl rem
          else normal
      | _ ->
          let () = print_endline (string_of_int lt10) in
          let first_chunk = encode_part ~e:encoded ~msl (num - lt10) in
          encode_part ~e:(List.rev first_chunk) ~msl lt10 )
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
    | [] -> n :: acc
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

let _collapse_same_denominations parts =
  let rec collapse acc = function
    | [] -> List.rev acc
    | first :: (next :: rest as remaining) ->
        if Glyph.same_denomination [first; next; first + next] then
          collapse ((first + next) :: acc) rest
        else collapse (first :: acc) remaining
    | last :: empty -> collapse (last :: acc) empty
  in
  collapse [] parts

let compress_small_numbers parts =
  let parts = List.rev parts in
  let compressed =
    match parts with
    | 4 :: v :: rest -> (
      match (Glyph.denomination v, v - 5) with
      | 5, 0 -> 9 :: rest
      | 5, _ -> 9 :: (v - 5) :: rest
      | _ -> parts )
    | _ -> parts
  in
  List.rev compressed

let encode ?(glyphs = Glyph.(all |> powers_of_ten)) ?(msl = 1) arabic =
  walk glyphs arabic
  |> compress_small_numbers
  |> _collapse_same_denominations
  |> List.map (Glyph.encode_part ~msl)
  |> List.concat
  |> List.map Glyph.to_char
  |> String.of_list
