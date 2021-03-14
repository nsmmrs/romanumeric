let greet name = "Hello " ^ name ^ "!"

let%test "can greet Ash" = greet "Ash" = "Hello Ash!"

let%test "can greet Pikachu" = greet "Pikachu" = "Pika Pika!"
