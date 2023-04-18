let decode = Glyph.make_decoder Roman.chars_values

let assert_line (arabic, roman) = roman ^ " -> " ^ string_of_int arabic

let can_decode category tests =
  ( category
  , `Quick
  , fun () ->
      let expected = List.map assert_line tests |> String.concat "\n" in
      let actual = List.map assert_line tests |> String.concat "\n" in
      check string "same output" expected actual )

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
    | _ ->
        failwith "failed to parse test fixture"
  in
  let rev_lists (a, b, c, d, e) = List.(rev a, rev b, rev c, rev d, rev e) in
  let open CCList in
  CCIO.(with_in "suites/reference.tsv" read_lines_l)
  |> fold_left parse_line ([], [], [], [], [])
  |> rev_lists

let () =
  Alcotest.run "Roman_numeral"
    [ ( "decoding"
      , [ can_decode "conventional" conv
        ; can_decode "compressed (lvl 1)" c1
        ; can_decode "compressed (lvl 2)" c2
        ; can_decode "compressed (lvl 3)" c3
        ; can_decode "compressed (lvl 4)" c4 ] ) ]
