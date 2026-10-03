import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const dir = dirname(fileURLToPath(import.meta.url));

// Parses test/suites/reference.tsv into five lists of [arabic, roman] pairs:
// conventional, then compression levels 1-4.
export function loadReference() {
  const tsv = readFileSync(join(dir, "suites", "reference.tsv"), "utf8");
  const conv = [];
  const c1 = [];
  const c2 = [];
  const c3 = [];
  const c4 = [];
  for (const line of tsv.split("\n")) {
    const trimmed = line.trim();
    if (trimmed === "") continue;
    const [n, c, l1, l2, l3, l4] = trimmed.split("\t");
    if (l4 === undefined) throw new Error("failed to parse test fixture");
    const arabic = parseInt(n, 10);
    conv.push([arabic, c]);
    c1.push([arabic, l1]);
    c2.push([arabic, l2]);
    c3.push([arabic, l3]);
    c4.push([arabic, l4]);
  }
  return { conv, c1, c2, c3, c4 };
}

export function unwrap(result) {
  if (!result.ok) throw new Error(`expected Ok, got Error: ${result.error}`);
  return result.value;
}
