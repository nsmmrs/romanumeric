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

  let equal = Equal.physical

  let descending = List.rev all

  let highest_denominator n = descending |> List.find (fun g -> n mod value g = 0)

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

  let closest num =
    let candidates = all |> List.map (fun g -> (g, num - value g)) in
    List.find_opt (fun (_, dist) -> dist = 0) candidates
    |> function
    | Some (g, _) -> Exactly g
    | None -> (
        candidates
        |> List.partition (fun (_, dist) -> dist > 0)
        |> Pair.map_same (fun l ->
               List.sort
                 (fun (_, d) (_, d2) -> Int.(compare (abs d2) (abs d)))
                 l
               |> List.last_opt )
        |> function
        | Some g, None -> Above g
        | Some g1, Some g2 -> Between (g1, g2)
        | None, Some _ | None, None -> failwith "shaking my smh rn" )

  let list s = String.(trim s |> to_list) |> List.map of_char |> List.all_some

  let repeat g n = List.init (Int.abs n) (fun _ -> g)

  let flat_fill num =
    let g = highest_denominator num in
    repeat g (num / value g)

  let descending_fill ?(init = []) num =
    match Int.abs num with
    let rec f acc = function
      | 0 -> List.(rev acc |> concat)
      | n ->
          let g = descending |> List.find (fun g -> n mod value g <> n) in
          let length, remainder = (n / value g, n mod value g) in
          let acc = repeat g length :: acc in
          f acc remainder
    in
    f init num

  let encode_additive (g, dist) = descending_fill ~init:[[g]] dist

  let encode_subtractive (g, dist) max_sub_len =
    let sub = flat_fill dist in
    if List.length sub > max_sub_len then None else Some (List.append sub [g])

  let rec encode_part max_sub_len num =
    let additive = encode_additive num in
    match encode_subtractive ~max_sub_len num with
    | Some subtractive -> if List.(length additive < length subtractive) then additive else subtractive
    | None -> additive
  and encode_additive num =
  
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

let encode ?(glyphs = Glyph.(all |> powers_of_ten)) ?(max_sub_len = 1) arabic =
  walk glyphs arabic
  |> compress_small_numbers
  |> _collapse_same_denominations
  |> List.map (Glyph.encode_part max_sub_len)
  |> List.concat
  |> List.map Glyph.to_char
  |> String.of_list
