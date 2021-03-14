// @flow

const { execSync } = require("child_process");
const fs = require("fs");
const path = require("path");

const filesToCopy = ["LICENSE", "README.md", "CHANGES.md"];

function exec(cmd) {
  console.log(`exec: ${cmd}`);
  return execSync(cmd).toString();
}

function mkdirpSync(p) {
  if (fs.existsSync(p)) {
    return;
  }
  mkdirpSync(path.dirname(p));
  fs.mkdirSync(p);
}

function removeSync(p) {
  exec(`rm -rf "${p}"`);
}

const src = path.resolve(path.join(__dirname, ".."));
const dst = path.resolve(path.join(__dirname, "..", "_release"));

removeSync(dst);
mkdirpSync(dst);

for (const file of filesToCopy) {
  const p = path.join(dst, file);
  mkdirpSync(path.dirname(p));
  fs.copyFileSync(path.join(src, file), p);
}

fs.copyFileSync(
  path.join(src, "scripts", "release-postinstall.js"),
  path.join(dst, "postinstall.js")
);

const filesToTouch = [
  "romanum"
];

for (const file of filesToTouch) {
  const p = path.join(dst, file);
  mkdirpSync(path.dirname(p));
  fs.writeFileSync(p, "");
}

const pkgJson = {
  name: "@noahsummers/romanum",
  version: "%%VERSION%%",
  description: "A library and CLI for the decoding and encoding of Roman numerals",
  author: "Noah Summers",
  license: "MIT",
  homepage: "https://github.com/noahsummers/romanum",
  bugs: {
    url: "https://github.com/noahsummers/romanum/issues"
  },
  repository: {
    type: "git",
    url: "https://github.com/noahsummers/romanum.git"
  },
  scripts: {
    postinstall: "node postinstall.js"
  },
  bin: {
    romanum: "romanum"
  },
  files: [
    "platform-windows-x64/",
    "platform-linux-x64/",
    "platform-darwin-x64/",
    "postinstall.js",
    "romanum"
  ]
};

fs.writeFileSync(path.join(dst, "package.json"), JSON.stringify(pkgJson, null, 2));
