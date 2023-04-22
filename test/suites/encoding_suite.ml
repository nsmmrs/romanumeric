let assert_line (arabic, roman) = string_of_int arabic ^ " -> " ^ roman

let can_encode category tests ~f =
  ( category
  , `Quick
  , fun () ->
      let expected = List.map assert_line tests |> String.concat "\n" in
      let actual =
        List.map (fun (n, _) -> assert_line (n, Result.get_ok (f n))) tests
        |> String.concat "\n"
      in
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
  Alcotest.run "Romanumeric"
    [ ( "encoding"
      , [ can_encode "conventional" conv ~f:Roman.of_int
        ; can_encode "compressed (lvl 1)" c1 ~f:(Roman.of_int ~c:1)
        ; can_encode "compressed (lvl 2)" c2 ~f:(Roman.of_int ~c:2)
        ; can_encode "compressed (lvl 3)" c3 ~f:(Roman.of_int ~c:3)
        ; can_encode "compressed (lvl 4)" c4 ~f:(Roman.of_int ~c:4) ] ) ]
