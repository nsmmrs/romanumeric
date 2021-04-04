let greet name = "Hello " ^ name ^ "!"

let%test "can greet Ash" = String.equal (greet "Ash") "Hello Ash!"
