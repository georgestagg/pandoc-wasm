# Pandoc on WebAssembly

The universal document converter, compiled for WebAssembly and running in the browser.

Demo application: https://georgestagg.github.io/pandoc-wasm

## What's included

This repository builds an npm package that wraps Pandoc, compiled for WebAssembly using the [Asterius](https://github.com/tweag/asterius) Haskell-to-Wasm compiler. A demo application is also included allowing for conversion between various document types.

**Warning:** Running Pandoc under WebAssembly using this library is fairly fragile at the moment (see the Extra Notes section below for details). Small documents seem to convert well, but there is definitely room for stability improvement when converting larger documents or including images.

## How to use

First, install the `pandoc-wasm` package using npm:

```
npm install --save pandoc-wasm
```

Import the module, run `init()` to download the Wasm binary, and convert documents using `run()`:

```
import { Pandoc } from "pandoc-wasm";

const pandoc = new Pandoc()
pandoc.init().then(async (pandoc) => {
  const result = await pandoc.run({
    text: "Some input text",
    options: { from: "markdown", to: "html" },
  });
  console.log(result);
});

```

See the `example` and `src/app` directories for more detailed examples.

## Pandoc options

The Haskell code that powers the `run()` function is a modified version of Pandoc's own built in `pandoc-server` code, and takes a similar `options` object to control how documents are converted. See [Pandoc's server documentation](https://pandoc.org/pandoc-server.html) for details on the options settings that can be passed to Pandoc.

### Including additional files

Supplemental files, such as images, can be included in the argument to the Pandoc `.run()` function. The `files` property should be a mapping from paths to file content, encoded either in a `Uint8Array` or a base64 encoded string:

```
pandoc.run({
  text: "![An image](images/test.png)",
  options: {
    'from': "markdown",
    'to': "html",
    'embed-resources': true
  },
  files: {
    'images/test.png': "iVBORw0KGgoAAAANSUhEUgAAADAAAAAlAQAAAAAsYlcCAAAACklEQVR4AWMYBQABAwABRUEDtQAAAABJRU5ErkJggg=="
  }
});
```

### Using a JS Web Worker

Pandoc can be run from inside a Web Worker. I recommend the [Comlink](https://github.com/GoogleChromeLabs/comlink) library as a way to handle communication between the main and worker threads. For an example see the demo application in the `src/app` directory, which uses this method.

## Extra Notes

* Asterius is deprecated in favour of the newer [ghc-wasm-meta](https://gitlab.haskell.org/ghc/ghc-wasm-meta) version of the GHC compiler. Once there is a simple way to use Template Haskell with `ghc-wasm-meta`, I'll switch to compiling with that toolchain.

* Pandoc relies on some Haskell libraries that use external C sources (e.g. zlib), which does not work when compiling with Asterius. For those libraries the functionality is instead replicated with JavaScript libraries called using Asterius's JS FFI.

* Asterius's [`--yolo`](https://asterius.netlify.app/ahc-link#--yolo) mode has been used to avoid GC issues. Smaller documents seem to work OK, but it is easy to trigger "out of memory" errors. This should be less of a problem once we've switched to using [ghc-wasm-meta](https://gitlab.haskell.org/ghc/ghc-wasm-meta).

* Node/Deno should be possible, but does not work right now due to the way `fetch()` is used to download the Pandoc binary and support files.

* No Lua filters right now. I _think_ it should be possible to compile a C Lua interpreter using [Emscripten](https://emscripten.org) (or something similar), then hook it up to the Pandoc wasm binary through a JS FFI.

## Quick Development

A Docker development container has been built containing the required prerequisites for building Pandoc for WebAssembly using Asterius. The following commands are a possible simplified route to get up and running:

```
git clone https://github.com/georgestagg/pandoc-wasm
cd pandoc-wasm
npm install
make submodules
make docker-container
make
```

See the included `Dockerfile` for more details about the pre-built development container.

## Related Projects

* [Asterius's Pandoc demo](https://asterius.netlify.app/demo/pandoc/pandoc.html)
* [y-taka-23/wasm-pandoc](https://github.com/y-taka-23/wasm-pandoc)

We use the same general process as used in the above projects to build Pandoc, but this project has the following advantages:
 * Provides a newer version of Pandoc.
 * Supports more readers and writers, including binary formats such as docx.
 * Supports more options and extensions, including parsing YAML headers.
 * Supports adding and embedding supplemental files, such as images, to document output.
