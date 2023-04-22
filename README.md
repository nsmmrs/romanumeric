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
    make_encoder table (compression_level + 1) 1
end
```

## Usage

<!-- $MDX env=usage -->
```ocaml
open Romanumeric
```

### Decoding

<!-- $MDX env=usage -->
```ocaml
# Roman.to_int "MCMXII"
- : decoding_result = Ok 1912
```

`romanumeric` follows a single rule of interpretation: if the code of a repetition has a lower value than the code of the next repetition, it is treated as negative (e.g. "XIIV" is interpreted as "10 + (-2) + 5").

Because of this, there is no problem decoding "non-standard" numerals like the following examples from history:

<!-- $MDX env=usage -->
```ocaml
let decode n = Result.get_ok (Roman.to_int n);;

assert (decode "IIIXX" = 17);;
assert (decode "IIXX" = 18);;
assert (decode "IIIC" = 97);;
assert (decode "IIC" = 98);;
assert (decode "IC" = 99);;
assert (decode "IIX" = 8);;
assert (decode "XIIX" = 18);;
assert (decode "XXIIX" = 28);;
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
- : decoding_result = Ok 18
```

### Encoding

<!-- $MDX env=usage -->
```ocaml
# Roman.of_int 1234
- : encoding_result = Ok "MCCXXXIV"
```

#### Compression

Like `ROMAN()`, compressed encoding is supported:

<!-- $MDX env=usage -->
```ocaml
# Roman.of_int ~c:0 499
- : encoding_result = Ok "CDXCIX"
```

<!-- $MDX env=usage -->
```ocaml
# Roman.of_int ~c:1 499
- : encoding_result = Ok "LDVLIV"
```

<!-- $MDX env=usage -->
```ocaml
# Roman.of_int ~c:4 499
- : encoding_result = Ok "ID"
```

#### Limitations

Unlike `ROMAN()`, input is not restricted to the arbitrary 1-3999 range:

<!-- $MDX env=usage -->
```ocaml
# Roman.of_int 0
- : encoding_result = Ok ""
```

<!-- $MDX env=usage -->
```ocaml
# Roman.of_int 5348
- : encoding_result = Ok "MMMMMCCCXLVIII"
```

Negative numbers are still not supported, however:

<!-- $MDX env=usage -->
```ocaml
# Roman.of_int (-5)
- : encoding_result = Error "Negative numbers are not supported"
```
