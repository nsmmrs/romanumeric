# romanumeric

`romanumeric` is an enterprise-grade Roman numeral toy project inspired by the `ROMAN()` function [from Excel](https://support.microsoft.com/en-us/office/roman-function-d6b0b99e-de46-4704-a518-b45a0f8b56f5).

It not only supports typical encoding and decoding, but also allows for creating and using custom numeral systems. The included `Roman` preset is itself just a simple preset over the generic machinery:

```js
import { makeTable, makeDecoder, makeEncoder } from "romanumeric";

export const Roman = {
  table: makeTable([
    ["I", 1],
    ["V", 5],
    ["X", 10],
    ["L", 50],
    ["C", 100],
    ["D", 500],
    ["M", 1000],
  ]),
};

Roman.toInt = makeDecoder(Roman.table);
Roman.ofInt = (n, c = 0) => makeEncoder(Roman.table, c + 1, 1)(n);
```

Results mirror OCaml's `(value, error) result`: `{ ok: true, value }` on success, `{ ok: false, error }` on failure.

## Usage

```js
import { Roman } from "romanumeric";
```

### Decoding

```js
Roman.toInt("MCMXII");
// { ok: true, value: 1912 }
```

`romanumeric` follows a single rule of interpretation: if the code of a repetition has a lower value than the code of the next repetition, it is treated as negative (e.g. "XIIV" is interpreted as "10 + (-2) + 5").

Because of this, there is no problem decoding "non-standard" numerals like the following examples from history:

```js
const decode = (n) => {
  const result = Roman.toInt(n);
  if (!result.ok) throw new Error(result.error);
  return result.value;
};

console.assert(decode("IIIXX") === 17);
console.assert(decode("IIXX") === 18);
console.assert(decode("IIIC") === 97);
console.assert(decode("IIC") === 98);
console.assert(decode("IC") === 99);
console.assert(decode("IIX") === 8);
console.assert(decode("XIIX") === 18);
console.assert(decode("XXIIX") === 28);
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

```js
Roman.toInt("IIXX");
// { ok: true, value: 18 }
```

### Encoding

```js
Roman.ofInt(1234);
// { ok: true, value: "MCCXXXIV" }
```

#### Compression

Like `ROMAN()`, compressed encoding is supported:

```js
Roman.ofInt(499, 0);
// { ok: true, value: "CDXCIX" }
```

```js
Roman.ofInt(499, 1);
// { ok: true, value: "LDVLIV" }
```

```js
Roman.ofInt(499, 4);
// { ok: true, value: "ID" }
```

#### Limitations

Unlike `ROMAN()`, input is not restricted to the arbitrary 1-3999 range:

```js
Roman.ofInt(0);
// { ok: true, value: "" }
```

```js
Roman.ofInt(5348);
// { ok: true, value: "MMMMMCCCXLVIII" }
```

Negative numbers are still not supported, however:

```js
Roman.ofInt(-5);
// { ok: false, error: "Negative numbers are not supported" }
```

## Custom numeral systems

Everything is built on generic tables and systems, so you can define your own:

```js
import { makeTable, makeDecoder, makeEncoder } from "romanumeric";

const table = makeTable([
  ["A", 1],
  ["B", 5],
  ["C", 10],
]);

const toInt = makeDecoder(table);
const ofInt = makeEncoder(table, 1, 1);

toInt("CCBA"); // { ok: true, value: 26 }
ofInt(26); // { ok: true, value: "CCBA" }
```

## Development

```sh
npm test
```

Tests are ported from the OCaml Alcotest suites and run against the shared `test/suites/reference.tsv` fixture (3,999 numerals × 5 compression levels, decoded and encoded).
