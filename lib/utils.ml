let greet name = "Hello " ^ name ^ "!"

let%test "can greet Ash" = greet "Ash" = "Hello Ash!"
