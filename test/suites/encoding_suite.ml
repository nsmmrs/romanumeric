(** Encoding test suite for the Roman_numeral module. *)

let can_encode (arabic, roman) ~f:encode =
  let int = string_of_int arabic in
  let assertion = int ^ " -> " ^ roman in
  ( assertion
  , `Quick
  , fun () ->
      let actual = encode arabic in
      check string assertion roman actual )

let conventional_suite =
  List.map
    (can_encode ~f:Roman_numeral.encode)
    [ (1, "I")
    ; (2, "II")
    ; (3, "III")
    ; (4, "IV")
    ; (5, "V")
    ; (6, "VI")
    ; (7, "VII")
    ; (8, "VIII")
    ; (9, "IX")
    ; (10, "X")
    ; (11, "XI")
    ; (15, "XV")
    ; (18, "XVIII")
    ; (27, "XXVII")
    ; (29, "XXIX")
    ; (33, "XXXIII")
    ; (40, "XL")
    ; (41, "XLI")
    ; (44, "XLIV")
    ; (48, "XLVIII")
    ; (52, "LII")
    ; (56, "LVI")
    ; (59, "LIX")
    ; (60, "LX")
    ; (63, "LXIII")
    ; (69, "LXIX")
    ; (73, "LXXIII")
    ; (85, "LXXXV")
    ; (86, "LXXXVI")
    ; (94, "XCIV")
    ; (121, "CXXI")
    ; (139, "CXXXIX")
    ; (145, "CXLV")
    ; (195, "CXCV")
    ; (197, "CXCVII")
    ; (271, "CCLXXI")
    ; (290, "CCXC")
    ; (299, "CCXCIX")
    ; (315, "CCCXV")
    ; (321, "CCCXXI")
    ; (560, "DLX")
    ; (566, "DLXVI")
    ; (587, "DLXXXVII")
    ; (611, "DCXI")
    ; (633, "DCXXXIII")
    ; (676, "DCLXXVI")
    ; (710, "DCCX")
    ; (809, "DCCCIX")
    ; (963, "CMLXIII")
    ; (965, "CMLXV")
    ; (1014, "MXIV")
    ; (1069, "MLXIX")
    ; (2563, "MMDLXIII")
    ; (2633, "MMDCXXXIII")
    ; (2797, "MMDCCXCVII")
    ; (2946, "MMCMXLVI")
    ; (2999, "MMCMXCIX")
    ; (3339, "MMMCCCXXXIX")
    ; (3867, "MMMDCCCLXVII")
    ; (3877, "MMMDCCCLXXVII")
    ; (3999, "MMMCMXCIX") ]

let compression_level_1 =
  let open Roman_numeral in
  List.map
    (can_encode ~f:(encode ~glyphs:Glyph.[D; L; V; I]))
    [ (1, "I")
    ; (2, "II")
    ; (3, "III")
    ; (4, "IV")
    ; (5, "V")
    ; (6, "VI")
    ; (7, "VII")
    ; (8, "VIII")
    ; (9, "IX")
    ; (10, "X")
    ; (11, "XI")
    ; (15, "XV")
    ; (18, "XVIII")
    ; (27, "XXVII")
    ; (29, "XXIX")
    ; (33, "XXXIII")
    ; (40, "XL")
    ; (41, "XLI")
    ; (44, "XLIV")
    ; (48, "VLIII")
    ; (52, "LII")
    ; (56, "LVI")
    ; (59, "LIX")
    ; (60, "LX")
    ; (63, "LXIII")
    ; (69, "LXIX")
    ; (73, "LXXIII")
    ; (85, "LXXXV")
    ; (86, "LXXXVI")
    ; (94, "XCIV")
    ; (121, "CXXI")
    ; (139, "CXXXIX")
    ; (145, "CVL")
    ; (195, "CVC")
    ; (197, "CVCII")
    ; (271, "CCLXXI")
    ; (290, "CCXC")
    ; (299, "CCVCIV")
    ; (315, "CCCXV")
    ; (321, "CCCXXI")
    ; (560, "DLX")
    ; (566, "DLXVI")
    ; (587, "DLXXXVII")
    ; (611, "DCXI")
    ; (633, "DCXXXIII")
    ; (676, "DCLXXVI")
    ; (710, "DCCX")
    ; (809, "DCCCIX")
    ; (963, "LMXIII")
    ; (965, "LMXV")
    ; (1014, "MXIV")
    ; (1069, "MLXIX")
    ; (2563, "MMDLXIII")
    ; (2633, "MMDCXXXIII")
    ; (2797, "MMDCCVCII")
    ; (2946, "MMCMVLI")
    ; (2999, "MMLMVLIV")
    ; (3339, "MMMCCCXXXIX")
    ; (3867, "MMMDCCCLXVII")
    ; (3877, "MMMDCCCLXXVII")
    ; (3999, "MMMLMVLIV") ]

let compression_level_2 =
  let open Roman_numeral in
  List.map
    (can_encode ~f:(encode ~glyphs:Glyph.[X; I]))
    [ (1, "I")
    ; (2, "II")
    ; (3, "III")
    ; (4, "IV")
    ; (5, "V")
    ; (6, "VI")
    ; (7, "VII")
    ; (8, "VIII")
    ; (9, "IX")
    ; (10, "X")
    ; (11, "XI")
    ; (15, "XV")
    ; (18, "XVIII")
    ; (27, "XXVII")
    ; (29, "XXIX")
    ; (33, "XXXIII")
    ; (40, "XL")
    ; (41, "XLI")
    ; (44, "XLIV")
    ; (48, "VLIII")
    ; (52, "LII")
    ; (56, "LVI")
    ; (59, "LIX")
    ; (60, "LX")
    ; (63, "LXIII")
    ; (69, "LXIX")
    ; (73, "LXXIII")
    ; (85, "LXXXV")
    ; (86, "LXXXVI")
    ; (94, "XCIV")
    ; (121, "CXXI")
    ; (139, "CXXXIX")
    ; (145, "CVL")
    ; (195, "CVC")
    ; (197, "CVCII")
    ; (271, "CCLXXI")
    ; (290, "CCXC")
    ; (299, "CCIC")
    ; (315, "CCCXV")
    ; (321, "CCCXXI")
    ; (560, "DLX")
    ; (566, "DLXVI")
    ; (587, "DLXXXVII")
    ; (611, "DCXI")
    ; (633, "DCXXXIII")
    ; (676, "DCLXXVI")
    ; (710, "DCCX")
    ; (809, "DCCCIX")
    ; (963, "LMXIII")
    ; (965, "LMXV")
    ; (1014, "MXIV")
    ; (1069, "MLXIX")
    ; (2563, "MMDLXIII")
    ; (2633, "MMDCXXXIII")
    ; (2797, "MMDCCVCII")
    ; (2946, "MMCMVLI")
    ; (2999, "MMXMIX")
    ; (3339, "MMMCCCXXXIX")
    ; (3867, "MMMDCCCLXVII")
    ; (3877, "MMMDCCCLXXVII")
    ; (3999, "MMMXMIX") ]

let compression_level_3 =
  let open Roman_numeral in
  List.map
    (can_encode ~f:(encode ~glyphs:Glyph.[V; I]))
    [ (1, "I")
    ; (2, "II")
    ; (3, "III")
    ; (4, "IV")
    ; (5, "V")
    ; (6, "VI")
    ; (7, "VII")
    ; (8, "VIII")
    ; (9, "IX")
    ; (10, "X")
    ; (11, "XI")
    ; (15, "XV")
    ; (18, "XVIII")
    ; (27, "XXVII")
    ; (29, "XXIX")
    ; (33, "XXXIII")
    ; (40, "XL")
    ; (41, "XLI")
    ; (44, "XLIV")
    ; (48, "VLIII")
    ; (52, "LII")
    ; (56, "LVI")
    ; (59, "LIX")
    ; (60, "LX")
    ; (63, "LXIII")
    ; (69, "LXIX")
    ; (73, "LXXIII")
    ; (85, "LXXXV")
    ; (86, "LXXXVI")
    ; (94, "XCIV")
    ; (121, "CXXI")
    ; (139, "CXXXIX")
    ; (145, "CVL")
    ; (195, "CVC")
    ; (197, "CVCII")
    ; (271, "CCLXXI")
    ; (290, "CCXC")
    ; (299, "CCIC")
    ; (315, "CCCXV")
    ; (321, "CCCXXI")
    ; (560, "DLX")
    ; (566, "DLXVI")
    ; (587, "DLXXXVII")
    ; (611, "DCXI")
    ; (633, "DCXXXIII")
    ; (676, "DCLXXVI")
    ; (710, "DCCX")
    ; (809, "DCCCIX")
    ; (963, "LMXIII")
    ; (965, "LMXV")
    ; (1014, "MXIV")
    ; (1069, "MLXIX")
    ; (2563, "MMDLXIII")
    ; (2633, "MMDCXXXIII")
    ; (2797, "MMDCCVCII")
    ; (2946, "MMCMVLI")
    ; (2999, "MMVMIV")
    ; (3339, "MMMCCCXXXIX")
    ; (3867, "MMMDCCCLXVII")
    ; (3877, "MMMDCCCLXXVII")
    ; (3999, "MMMVMIV") ]

let compression_level_4 =
  let open Roman_numeral in
  List.map
    (can_encode ~f:(encode ~glyphs:Glyph.[I]))
    [ (1, "I")
    ; (2, "II")
    ; (3, "III")
    ; (4, "IV")
    ; (5, "V")
    ; (6, "VI")
    ; (7, "VII")
    ; (8, "VIII")
    ; (9, "IX")
    ; (10, "X")
    ; (11, "XI")
    ; (15, "XV")
    ; (18, "XVIII")
    ; (27, "XXVII")
    ; (29, "XXIX")
    ; (33, "XXXIII")
    ; (40, "XL")
    ; (41, "XLI")
    ; (44, "XLIV")
    ; (48, "VLIII")
    ; (52, "LII")
    ; (56, "LVI")
    ; (59, "LIX")
    ; (60, "LX")
    ; (63, "LXIII")
    ; (69, "LXIX")
    ; (73, "LXXIII")
    ; (85, "LXXXV")
    ; (86, "LXXXVI")
    ; (94, "XCIV")
    ; (121, "CXXI")
    ; (139, "CXXXIX")
    ; (145, "CVL")
    ; (195, "CVC")
    ; (197, "CVCII")
    ; (271, "CCLXXI")
    ; (290, "CCXC")
    ; (299, "CCIC")
    ; (315, "CCCXV")
    ; (321, "CCCXXI")
    ; (560, "DLX")
    ; (566, "DLXVI")
    ; (587, "DLXXXVII")
    ; (611, "DCXI")
    ; (633, "DCXXXIII")
    ; (676, "DCLXXVI")
    ; (710, "DCCX")
    ; (809, "DCCCIX")
    ; (963, "LMXIII")
    ; (965, "LMXV")
    ; (1014, "MXIV")
    ; (1069, "MLXIX")
    ; (2563, "MMDLXIII")
    ; (2633, "MMDCXXXIII")
    ; (2797, "MMDCCVCII")
    ; (2946, "MMCMVLI")
    ; (2999, "MMIM")
    ; (3339, "MMMCCCXXXIX")
    ; (3867, "MMMDCCCLXVII")
    ; (3877, "MMMDCCCLXXVII")
    ; (3999, "MMMIM") ]

let big_numbers =
  List.map can_encode
    [ (4000, "MMMM")
    ; (5000, "MMMMM")
    ; (6000, "MMMMMM")
    ; (7000, "MMMMMMM")
    ; (8000, "MMMMMMMM")
    ; (9000, "MMMMMMMMM")
    ; (9999, "MMMMMMMMMCMXCIX") ]

let () =
  Alcotest.run "Encoding"
    [ ("conventional", conventional_suite)
    ; ("compressed (lvl 1)", compression_level_1)
    ; ("compressed (lvl 2)", compression_level_2)
    ; ("compressed (lvl 3)", compression_level_3)
    ; ("compressed (lvl 4)", compression_level_4) ]