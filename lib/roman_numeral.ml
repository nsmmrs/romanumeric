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

  let rec power_of m = function
    | n when n = 1 -> true
    | n when n mod m <> 0 -> false
    | n -> power_of m (n / m)

  let powers_of_ten =
    all |> List.filter (fun g -> value g |> power_of 10) |> List.rev

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

  let dumb_encode ?(glyphs = all) num =
    let g =
      glyphs
      |> List.filter (fun g -> num mod value g = 0)
      |> List.fold_left (fun a g -> if value g > value a then g else a) I
    in
    let length = Int.abs (num / value g) in
    List.init length (fun _ -> g)

  let encode_part glyphs max_sub_len int =
    match closest int with
    | Exactly g -> [g]
    | Above (g, dist) -> g :: dumb_encode dist
    | Between ((low, ldist), (high, hdist)) ->
        let add = low :: dumb_encode ldist in
        let sub = List.append (dumb_encode hdist ~glyphs) [high] in
        let al, sl = List.(length add, length sub) in
        if al < sl then add else if sl > max_sub_len + 1 then add else sub
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

let rec walk glyphs ?(parts = []) num =
  let greater_than m n = n > m in
  match glyphs with
  | [] -> parts |> List.filter (greater_than 0) |> List.rev
  | g :: remaining ->
      let part = Glyph.(num / value g * value g) in
      let parts = part :: parts in
      walk remaining ~parts (num - part)

let encode ?(glyphs = Glyph.powers_of_ten) ?(max_sub_len = 1) arabic =
  walk glyphs arabic
  |> List.map (Glyph.encode_part glyphs max_sub_len)
  |> List.concat
  |> List.map Glyph.to_char
  |> String.of_list
