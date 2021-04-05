let greet name = "Hello " ^ name ^ "!"

let%test "can greet Ash" = String.equal (greet "Ash") "Hello Ash!"

let filter_take ~f:valid n list =
  let rec take_from ?(taken = []) ?(n_taken = 0) list =
    if n_taken = n then taken
    else
      match list with
      | [] -> taken
      | e :: remaining ->
          let taken, n_taken =
            if valid e then (e :: taken, n_taken + 1) else (taken, n_taken)
          in
          take_from remaining ~taken ~n_taken
  in
  take_from list |> List.rev
