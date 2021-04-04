(** Decoding test suite for the Roman_numeral module. *)

let decode = Glyph.make_decoder Roman.chars_values

let can_decode (numeral, integer) =
  let description = "can decode \"" ^ numeral ^ "\"" in
  let assertion = numeral ^ " = " ^ string_of_int integer in
  ( description
  , `Quick
  , fun () ->
      let actual = decode numeral in
      check int assertion integer actual )

let suite =
  List.map can_decode
    [ ("", 0)
    ; ("IIII", 4)
    ; ("IV", 4)
    ; ("IIIII", 5)
    ; ("IIIIII", 6)
    ; ("IIX", 8)
    ; ("VIII", 8)
    ; ("IX", 9)
    ; ("VIIII", 9)
    ; ("IIIXX", 17)
    ; ("IIXX", 18)
    ; ("XIIX", 18)
    ; ("XIIX", 18)
    ; ("XVIII", 18)
    ; ("XXIIII", 24)
    ; ("XXIIX", 28)
    ; ("XXXIX", 39)
    ; ("XL", 40)
    ; ("XXXX", 40)
    ; ("XLIIII", 44)
    ; ("XXXXX", 50)
    ; ("XXXXXX", 60)
    ; ("LXXIIII", 74)
    ; ("LXXXX", 90)
    ; ("XC", 90)
    ; ("IIIC", 97)
    ; ("IIC", 98)
    ; ("IC", 99)
    ; ("CLX", 160)
    ; ("CCVII", 207)
    ; ("CCXLVI", 246)
    ; ("CCCC", 400)
    ; ("CD", 400)
    ; ("CCCCLXXXX", 490)
    ; ("CDXCIX", 499)
    ; ("ID", 499)
    ; ("LDVLIV", 499)
    ; ("VDIV", 499)
    ; ("XDIX", 499)
    ; ("DCCLXXXIX", 789)
    ; ("CM", 900)
    ; ("DCCCC", 900)
    ; ("MIX", 1009)
    ; ("MLXVI", 1066)
    ; ("MDCCLXXVI", 1776)
    ; ("MCM", 1900)
    ; ("MDCDIII", 1903)
    ; ("MCMX", 1910)
    ; ("MDCCCCX", 1910)
    ; ("MCMXVIII", 1918)
    ; ("MCMLIV", 1954)
    ; ("MMXIV", 2014)
    ; ("MMXXI", 2021)
    ; ("MMCDXXI", 2421)
    ; ("MMMCMXCIX", 3999) ]

let () = Alcotest.run "Roman_numeral" [("decode", suite)]
