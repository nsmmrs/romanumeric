(** Encoding test suite for the Roman_numeral module. *)

open Roman_numeral

let can_encode (arabic, roman) ~f:encode =
  let int = string_of_int arabic in
  let assertion = int ^ " -> " ^ roman in
  ( assertion
  , `Quick
  , fun () ->
      let actual = encode arabic in
      check string assertion roman actual )

let conv, c1, c2, c3, c4 =
  let parse_line (cs, c1s, c2s, c3s, c4s) line =
    String.(trim line |> split_on_char '\t')
    |> function
    | [n; c; c1; c2; c3; c4] ->
        let n = int_of_string n in
        ( (n, c) :: cs
        , (n, c1) :: c1s
        , (n, c2) :: c2s
        , (n, c3) :: c3s
        , (n, c4) :: c4s )
    | _ -> failwith "failed to parse test fixture"
  in
  let rev_lists (a, b, c, d, e) = List.(rev a, rev b, rev c, rev d, rev e) in
  let open CCList in
  CCIO.(with_in "suites/encoding_suite.tsv" read_lines_l)
  |> fold_left parse_line ([], [], [], [], [])
  |> rev_lists

let conventional_suite = List.map (can_encode ~f:encode) conv

let compression_level_1 = List.map (can_encode ~f:(encode ~msd:2)) c1

let compression_level_2 = List.map (can_encode ~f:(encode ~msd:3)) c2

let compression_level_3 = List.map (can_encode ~f:(encode ~msd:4)) c3

let compression_level_4 = List.map (can_encode ~f:(encode ~msd:5)) c4

let () =
  Alcotest.run "Encoding"
    [ ("conventional", conventional_suite)
    ; ("compressed (lvl 1)", compression_level_1)
    ; ("compressed (lvl 2)", compression_level_2)
    ; ("compressed (lvl 3)", compression_level_3)
    ; ("compressed (lvl 4)", compression_level_4) ]
