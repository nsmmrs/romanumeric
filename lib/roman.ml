let chars_values =
  [ ('I', 1)
  ; ('V', 5)
  ; ('X', 10)
  ; ('L', 50)
  ; ('C', 100)
  ; ('D', 500)
  ; ('M', 1_000) ]

let decode = Glyph.make_decoder chars_values

module Encode = struct
  let conventional = Glyph.make_encoder chars_values 1 1

  let compressed lvl =
    match lvl with
    | 1 | 2 | 3 | 4 -> Glyph.make_encoder chars_values (lvl + 1) 1
    | _ -> failwith "unsupported option"
end
