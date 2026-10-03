import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { Roman } from "../lib/romanumeric.js";
import { loadReference, unwrap } from "./reference.js";

const { conv, c1, c2, c3, c4 } = loadReference();

function canEncode(category, tests, f) {
  it(category, () => {
    const expected = tests.map(([arabic, roman]) => `${arabic} -> ${roman}`);
    const actual = tests.map(([arabic]) => `${arabic} -> ${unwrap(f(arabic))}`);
    assert.deepEqual(actual, expected);
  });
}

describe("encoding", () => {
  canEncode("conventional", conv, (n) => Roman.ofInt(n));
  canEncode("compressed (lvl 1)", c1, (n) => Roman.ofInt(n, 1));
  canEncode("compressed (lvl 2)", c2, (n) => Roman.ofInt(n, 2));
  canEncode("compressed (lvl 3)", c3, (n) => Roman.ofInt(n, 3));
  canEncode("compressed (lvl 4)", c4, (n) => Roman.ofInt(n, 4));
});

describe("encoding edge cases", () => {
  it("encodes numbers above the conventional 3999 limit", () => {
    assert.equal(unwrap(Roman.ofInt(5348)), "MMMMMCCCXLVIII");
  });

  it("rejects negative numbers", () => {
    assert.deepEqual(Roman.ofInt(-5), {
      ok: false,
      error: "Negative numbers are not supported",
    });
  });

  it("round-trips 1..3999 through the conventional encoding", () => {
    for (let n = 1; n <= 3999; n++) {
      const roman = unwrap(Roman.ofInt(n));
      assert.equal(unwrap(Roman.toInt(roman)), n, `round-trip failed for ${n}`);
    }
  });
});
