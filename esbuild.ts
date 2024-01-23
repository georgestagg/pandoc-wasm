import esbuild from "esbuild";

declare var process : {
  argv: string[];
}

let prod = false;
let serve = false;
let watch = false;

if (process.argv.some((x) => x === "--watch")) {
  watch = true;
}

if (process.argv.some((x) => x === "--serve")) {
  watch = true;
  serve = true;
}

if (process.argv.some((x) => x === "--prod")) {
  prod = true;
}

function build(
  inputs: string[],
  outdir: string,
  platform: esbuild.Platform,
  minify: boolean
) {
  return esbuild.context({
    bundle: true,
    entryPoints: inputs,
    loader: { ".html": "copy" },
    minify,
    outdir: outdir,
    platform: platform,
    logLevel: "info",
  });
}

const distFiles = ["src/pandoc.ts"];
const appFiles = ["src/app/app.ts", "src/app/worker.ts", "src/app/index.html"];
const distCtx = await build(distFiles, "./dist", "neutral", prod);
const appCtx = await build(appFiles, "./www", "browser", prod);

await distCtx.rebuild();
await appCtx.rebuild();

if (watch) {
  await distCtx.watch();
  await appCtx.watch();
  [...distFiles, ...appFiles].forEach((f) => console.log(`> Watching: ${f}`));
}

if (serve) {
  await appCtx.serve({ servedir: "www" });
} else if (!watch) {
  distCtx.dispose();
  appCtx.dispose();
}
