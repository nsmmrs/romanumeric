open Containers

module Glyph = struct
  type t = I | V | X | L | C | D | M

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

  let equal (g1 : t) (g2 : t) = Equal.physical g1 g2

  let%test _ = equal I I

  let%test _ = not @@ equal V I

  module List = struct
    let rec from_chars = function
      | char :: chars -> (
        match from_char char with
        | Some glyph -> glyph :: from_chars chars
        | None -> [] )
      | [] -> []

    let from_string string =
      let chars = String.(trim string |> to_list) in
      let glyphs = from_chars chars in
      let open List in
      if length chars = length glyphs then glyphs else []

    let equal = List.equal equal
  end
end

let%test _ = Glyph.List.equal Glyph.[M; V; C] (Glyph.List.from_string " MVC ")

let%test _ = List.is_empty (Glyph.List.from_string "MAC")

let decode _ = 0
