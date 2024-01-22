import * as path from "path";
import * as fs from "fs/promises";

const pandocDir = "pandoc";
const outDir = "dist";

async function* walk(dir) {
  for await (const d of await fs.opendir(dir)) {
    const entry = path.join(dir, d.name);
    if (d.isDirectory()) {
      yield* walk(entry);
    } else if (d.isFile()) {
      yield entry;
    }
  }
}

let data = Buffer.from([]);
const metadata = {
  files: [],
};
let head = 0;
let tail = 0;

for await (const f of walk(path.join(pandocDir, "data"))) {
  const fileData = await fs.readFile(f);
  data = Buffer.concat([data, fileData]);
  head = tail;
  tail += fileData.byteLength;
  metadata.files.push({
    filename: path.relative(pandocDir, f),
    start: head,
    end: tail,
  });
}
metadata.remote_package_size = tail;

fs.writeFile(
  path.join(outDir, "pandoc-data.metadata"), JSON.stringify(metadata)
);
fs.writeFile(path.join(outDir, "pandoc-data.data"), data);
