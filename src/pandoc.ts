// @ts-expect-error Asterius generated: no types
import req from "./_generated/pandoc-wasm.req.mjs";
// @ts-expect-error Asterius generated: no types
import * as rts from "./_generated/rts.mjs";
import * as utils from "./utils";
import * as pako from "pako";
import * as crc32 from "crc-32/crc32";
import * as crc32c from "crc-32/crc32c";
import { default as adler32 } from "adler-32";
import yaml from "js-yaml";

const baseUrl = "./";
const cdnUrl = `https://cdn.jsdelivr.net/npm/pandoc-wasm@${process.env.VERSION}/dist/`;

async function fallbackCdnFetch(input: string, init?: RequestInit) {
  const baseHeaders = await fetch(`${baseUrl}${input}`, { method: "HEAD" });
  if (baseHeaders.ok) return await fetch(`${baseUrl}${input}`, init);

  const cdnResponse = await fetch(`${cdnUrl}${input}`, init);
  if (cdnResponse.ok) return cdnResponse;

  throw new Error("Can't download pandoc assets from either base or CDN URL.");
}

export type PandocParams = {
  text: string;
  options: { [key: string]: unknown } & {
    from: string;
    to: string;
  };
  files?: { [key: string]: ArrayBufferLike | string };
  citeproc?: boolean;
};

export class Pandoc {
  static pako: typeof pako = pako;
  static digest: {
    crc32: typeof crc32;
    crc32c: typeof crc32c;
    adler32: typeof adler32;
  } = { crc32, crc32c, adler32 };
  static yaml: typeof yaml = yaml;
  #runQueue: Array<{
    params: PandocParams;
    resolve: (_: string) => void;
    reject: (_: string) => void;
  }> = [];
  wasm: Promise<WebAssembly.Module>;
  dataFiles: { [key: string]: ArrayBufferLike } = {};

  constructor() {
    this.wasm = fallbackCdnFetch(`pandoc-wasm.wasm.gz`)
      .then((response) => response.arrayBuffer())
      .then((gz) => Pandoc.pako.ungzip(gz))
      .then((buf) => WebAssembly.compile(buf));

    this.#downloadData();
    this.#installErrorHandler();
  }

  async init() {
    await this.wasm;
    await this.#downloadData();
    return this;
  }

  async #downloadData() {
    if (Object.keys(this.dataFiles).length > 0) {
      return this.dataFiles;
    }

    const gz = await fallbackCdnFetch(`pandoc-data.data.gz`);
    const data = Pandoc.pako.ungzip(await gz.arrayBuffer());
    const metaFile = await fallbackCdnFetch(`pandoc-data.metadata`);
    const metadata = await metaFile.json();
    if (metadata.remote_package_size != data.byteLength) {
      throw new Error(
        "Unexpected content when downloading Pandoc support data."
      );
    }
    metadata.files.forEach(
      (f: { filename: string; start: number; end: number }) => {
        this.dataFiles[`data/${f.filename}`] = data.subarray(f.start, f.end);
      }
    );
    return this.dataFiles;
  }

  /*
   * Asterius's GC is a little fragile, so we try to avoid it. Here we detect if
   * we're out of memory and reject with an error message.
   */
  #installErrorHandler() {
    globalThis.onerror = (
      event: string | Event,
      source: string | undefined,
      lineno: number | undefined,
      colno: number | undefined,
      error: Error | undefined
    ) => {
      if (!error) return;
      // Assume any uncaught RangeError is due to Asterius GC failure
      // TODO: Work out why GC fails, or switch to ghc-wasm-meta
      if (error.name === "RangeError" || error.name === "RuntimeError") {
        const message =
          error.name === "RangeError"
            ? "RangeError: Out of memory in Wasm heap."
            : `${error.name}: ${error.message}`;
        this.#runQueue.forEach((q) => q.reject(message));
      } else {
        throw error;
      }
    };
  }

  run(_params: PandocParams): Promise<string> {
    const params = {
      text: _params.text,
      options: _params.options,
      citeproc: _params.citeproc,
      files: Object.fromEntries(
        Object.entries({
          ..._params.files,
          ...this.dataFiles,
        }).map(([k, v]) => [
          k,
          typeof v === "string" ? v : utils.arrayBufferToBase64(v),
        ])
      ),
    };
    let q: {
      params: PandocParams;
      resolve: (_: string) => void;
      reject: (_: string) => void;
    };
    return new Promise<string>((resolve, reject) => {
      q = { params, resolve, reject };
      this.#runQueue.push(q);

      // TODO: Once GC is working, we won't need to recompile and instantiate
      // Pandoc again for each run.
      this.wasm
        .then((module) =>
          rts.newAsteriusInstance(Object.assign(req, { module }))
        )
        .then((instance) => instance.exports.runPandoc(params))
        .then((ret) => {
          if ("error" in ret) {
            reject(ret.error);
          } else {
            resolve(ret.output);
          }
        })
        .catch((err) => reject(err));
    }).finally(() => (this.#runQueue = this.#runQueue.filter((x) => x !== q)));
  }

  async getVersion(): Promise<string> {
    const module = await this.wasm;
    const instance = await rts.newAsteriusInstance(
      Object.assign(req, { module })
    );
    return await instance.exports.getVersion();
  }
}

(globalThis as typeof globalThis & { Pandoc: typeof Pandoc }).Pandoc = Pandoc;
export default Pandoc;
