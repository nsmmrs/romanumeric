# romanumeric

`romanumeric` is an enterprise-grade Roman numeral toy project inspired by the `ROMAN()` function [from Excel](https://support.microsoft.com/en-us/office/roman-function-d6b0b99e-de46-4704-a518-b45a0f8b56f5).

It not only supports typical encoding and decoding, but also allows for creating and using custom numeral systems. The included `Roman` module is itself just a simple preset:

<!-- $MDX file=lib/romanumeric.ml,part=Roman -->
```ocaml
module Roman = struct
  let table =
    Table.make
      [ ('I', 1)
      ; ('V', 5)
      ; ('X', 10)
      ; ('L', 50)
      ; ('C', 100)
      ; ('D', 500)
      ; ('M', 1_000) ]

  let to_int = make_decoder table

  let of_int ?c:(compression_level = 0) =
    match compression_level with
    | 0 | 1 | 2 | 3 | 4 ->
        make_encoder table (compression_level + 1) 1
    | _ ->
        failwith "unsupported option"
end
```

## Usage

<!-- $MDX env=usage -->
```ocaml
module Roman = Romanumeric.Roman;;
```

### Decoding

<!-- $MDX env=usage -->
```ocaml
# Roman.to_int "MCMXII"
- : int = 1912
```

`romanumeric` follows a single rule of interpretation: if the code of a repetition has a lower value than the code of the next, it is treated as negative (e.g. "XIIV" is interpreted as "10 + (-2) + 5").

Because of this, there is no problem decoding "non-standard" numerals like the following examples from history:

<!-- $MDX env=usage -->
```ocaml
assert (Roman.to_int "IIIXX" = 17);;
assert (Roman.to_int "IIXX" = 18);;
```

<!-- $MDX env=usage -->
```ocaml
assert (Roman.to_int "IIIC" = 97);;
assert (Roman.to_int "IIC" = 98);;
assert (Roman.to_int "IC" = 99);;
```

<!-- $MDX env=usage -->
```ocaml
assert (Roman.to_int "IIX" = 8);;
assert (Roman.to_int "XIIX" = 18);;
assert (Roman.to_int "XXIIX" = 28);;
```

Compare this to the output from Google Sheets:

| Input              | Expected   | Actual  |
| ---                | ---        | ---     |
| `=ARABIC("IIIXX")` | 17         | #VALUE! | 
| `=ARABIC("IIXX")`  | 18         | #VALUE! | 
| `=ARABIC("IIIC")`  | 97         | #VALUE! | 
| `=ARABIC("IIC")`   | 98         | #VALUE! | 
| `=ARABIC("IC")`    | 99         |      99 | 
| `=ARABIC("IIX")`   | 8          | #VALUE! |  
| `=ARABIC("XIIX")`  | 18         | #VALUE! | 
| `=ARABIC("XXIIX")` | 28         | #VALUE! | 

A historical example of a truly non-standard numeral would be the use of "IIXX" to indicate 22 (as "two and twenty"). Under the standard rules of interpretation, "IIXX" evaluates to 18:

<!-- $MDX env=usage -->
```ocaml
# Roman.to_int "IIXX"
- : int = 18
```

### Encoding

<!-- $MDX env=usage -->
```ocaml
# Roman.of_int 1234
- : string = "MCCXXXIV"
```

#### Compression

Like `ROMAN()`, compressed encoding is supported:

<!-- $MDX env=usage -->
```ocaml
# Roman.of_int ~c:0 499
- : string = "CDXCIX"
```

<!-- $MDX env=usage -->
```ocaml
# Roman.of_int ~c:1 499
- : string = "LDVLIV"
```

<!-- $MDX env=usage -->
```ocaml
# Roman.of_int ~c:4 499
- : string = "ID"
```

#### Limits

Unlike `ROMAN()`, input is not restricted to the arbitrary 1-3999 range:

<!-- $MDX env=usage -->
```ocaml
# Roman.of_int 0
- : string = ""
```

<!-- $MDX env=usage -->
```ocaml
# Roman.of_int 5348
- : string = "MMMMMCCCXLVIII"
```

Negative numbers are still not supported, however:

<!-- $MDX env=usage -->
```ocaml
# Roman.of_int (-5)
Exception: Not_found.
```
