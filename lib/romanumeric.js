// romanumeric.js
//
// Enterprise-grade Roman numeral conversion for JavaScript.
//
// A faithful port of the OCaml `romanumeric` library. It supports typical
// encoding and decoding of Roman numerals, plus custom numeral systems and
// compressed ("subtractive-heavy") encodings à la Excel's ROMAN().
//
// Results mirror OCaml's `(value, error) result`:
//   { ok: true, value } on success, { ok: false, error } on failure.

// ---------------------------------------------------------------------------
// Codes and tables
// ---------------------------------------------------------------------------

// A code pairs a single-character symbol with an integer value.
export function makeCode(symbol, value) {
  return { symbol, value };
}

function compareCodes(a, b) {
  return a.value - b.value;
}

// A code is repeatable when no other code in the table is exactly double its
// value (e.g. V is not repeatable because X = 2 * V exists).
function isRepeatable(table, code) {
  return !table.some((other) => code.value * 2 === other.value);
}

function repeatCode(length, code) {
  return { code, length };
}

// A table is a list of codes sorted ascending by value.
export function makeTable(symbolValues) {
  return symbolValues
    .map(([symbol, value]) => makeCode(symbol, value))
    .sort(compareCodes);
}

function tableHasValue(code, table) {
  return table.some((other) => other.value === code.value);
}

function memoizeTable(table) {
  return { asc: table, desc: [...table].reverse() };
}

function codeOfChar(table, char) {
  return table.find((code) => code.symbol === char);
}

// ---------------------------------------------------------------------------
// Repetitions
// ---------------------------------------------------------------------------

function repetitionOfChars(table, chars) {
  if (chars.length === 0) return null;
  const code = codeOfChar(table, chars[0]);
  return code ? repeatCode(chars.length, code) : null;
}

function repetitionToChars({ code, length }) {
  return Array(Math.abs(length)).fill(code.symbol);
}

function repetitionValue({ code, length }) {
  return code.value * length;
}

// A repetition counts negative when the next repetition's code is worth more
// (e.g. in "XIIV", the "II" contributes -2).
function repetitionToAddend(current, next) {
  const v = repetitionValue(current);
  return current.code.value < next.code.value ? -v : v;
}

// ---------------------------------------------------------------------------
// Systems
// ---------------------------------------------------------------------------

function makeRepeatablePredicate(tableMemo) {
  const repeatableCodes = tableMemo.asc.filter((code) =>
    isRepeatable(tableMemo.asc, code)
  );
  return (code) => tableHasValue(code, repeatableCodes);
}

// The `msd` largest codes below `code` in value, excluding codes worth exactly
// half of it (they never need subtractive help).
function subtractorsFor(code, tableDesc, msd) {
  const subs = [];
  for (const other of tableDesc) {
    if (subs.length === msd) break;
    if (other.value >= code.value) continue;
    if (other.value * 2 === code.value) continue;
    subs.unshift(other);
  }
  return subs;
}

function makeSubtractors(tableMemo, msd) {
  const memoized = new Map();
  for (const code of tableMemo.asc) {
    memoized.set(code.value, subtractorsFor(code, tableMemo.desc, msd));
  }
  return (code) => memoized.get(code.value) ?? [];
}

// A system bundles a table with its repeatability and subtractor rules plus
// the maximum subtractive depth (msd) and maximum subtractor length (msl).
export function makeSystem(table, msd, msl) {
  const memo = memoizeTable(table);
  return {
    table: memo,
    repeatable: makeRepeatablePredicate(memo),
    subtractors: makeSubtractors(memo, msd),
    msl,
    msd,
  };
}

// ---------------------------------------------------------------------------
// Numerals
// ---------------------------------------------------------------------------

export function numeralToString(numeral) {
  return [...numeral].reverse().flatMap(repetitionToChars).join("");
}

export function numeralToInt(numeral) {
  let acc = 0;
  for (let i = 0; i < numeral.length; i++) {
    const current = numeral[i];
    const next = numeral[i + 1];
    acc += next ? repetitionToAddend(current, next) : repetitionValue(current);
  }
  return acc;
}

function groupSuccessive(chars) {
  const groups = [];
  for (const char of chars) {
    const last = groups[groups.length - 1];
    if (last && last[0] === char) last.push(char);
    else groups.push([char]);
  }
  return groups;
}

export function numeralOfString(table, string) {
  const numeral = [];
  for (const group of groupSuccessive([...string])) {
    const repetition = repetitionOfChars(table, group);
    if (!repetition) return null;
    numeral.push(repetition);
  }
  return numeral;
}

function subtraction(system, code, acc) {
  const dist = code.value - acc.remainder;
  for (const c of system.subtractors(code)) {
    const v = c.value;
    if (code.value - v * system.msl <= acc.remainder) {
      const reps = dist % v === 0 ? dist / v : Math.floor(dist / v) + 1;
      const remainder = v * reps - dist;
      if (reps === 1 || system.repeatable(c)) {
        return { numeral: [repeatCode(reps, c)], remainder };
      }
    }
  }
  return null;
}

function encodeAdditive(system, acc) {
  const closestLower = system.table.desc.find(
    (c) => Math.floor(acc.remainder / c.value) > 0
  );
  return {
    numeral: [repeatCode(1, closestLower)],
    remainder: acc.remainder - closestLower.value,
  };
}

function encodeSubtractive(system, acc) {
  const code = system.table.asc.find((c) => c.value >= acc.remainder);
  if (!code) return null;
  if (code.value - acc.remainder === 0) {
    return { numeral: [repeatCode(1, code)], remainder: 0 };
  }
  const sub = subtraction(system, code, acc);
  if (!sub) return null;
  return {
    numeral: [repeatCode(1, code), ...sub.numeral],
    remainder: sub.remainder,
  };
}

function numeralOfInt(system, n) {
  let acc = { numeral: [], remainder: n };
  while (acc.remainder !== 0) {
    const result = encodeSubtractive(system, acc) ?? encodeAdditive(system, acc);
    acc = {
      numeral: [...result.numeral, ...acc.numeral],
      remainder: result.remainder,
    };
  }
  return acc.numeral;
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

function ok(value) {
  return { ok: true, value };
}

function err(error) {
  return { ok: false, error };
}

export function decode(table, string) {
  const numeral = numeralOfString(table, string);
  return numeral === null ? err("Invalid numeral") : ok(numeralToInt(numeral));
}

export function encode(system, arabic) {
  if (arabic < 0) return err("Negative numbers are not supported");
  if (system.msd < 1) return err("Invalid system");
  const numeral = numeralOfInt(system, arabic);
  return numeral === null ? err("Insufficient system") : ok(numeralToString(numeral));
}

export function makeDecoder(table) {
  return (string) => decode(table, string);
}

export function makeEncoder(table, msd, msl) {
  const system = makeSystem(table, msd, msl);
  return (arabic) => encode(system, arabic);
}

// The Roman preset: the included `Roman` module is just a simple preset over
// the generic machinery above.
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
