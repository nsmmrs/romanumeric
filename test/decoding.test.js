import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { Roman } from "../lib/romanumeric.js";
import { loadReference, unwrap } from "./reference.js";

const { conv, c1, c2, c3, c4 } = loadReference();

function canDecode(category, tests) {
  it(category, () => {
    const expected = tests.map(([arabic, roman]) => `${roman} -> ${arabic}`);
    const actual = tests.map(([, roman]) => `${roman} -> ${unwrap(Roman.toInt(roman))}`);
    assert.deepEqual(actual, expected);
  });
}

describe("decoding", () => {
  canDecode("conventional", conv);
  canDecode("compressed (lvl 1)", c1);
  canDecode("compressed (lvl 2)", c2);
  canDecode("compressed (lvl 3)", c3);
  canDecode("compressed (lvl 4)", c4);
});

describe("decoding edge cases", () => {
  it("decodes non-standard historical numerals", () => {
    const cases = [
      ["IIIXX", 17],
      ["IIXX", 18],
      ["IIIC", 97],
      ["IIC", 98],
      ["IC", 99],
      ["IIX", 8],
      ["XIIX", 18],
      ["XXIIX", 28],
    ];
    for (const [roman, arabic] of cases) {
      assert.equal(unwrap(Roman.toInt(roman)), arabic, roman);
    }
  });

  it("rejects unknown symbols", () => {
    assert.deepEqual(Roman.toInt("ABC"), {
      ok: false,
      error: "Invalid numeral",
    });
  });
});
