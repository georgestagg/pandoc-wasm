// @ts-expect-error Asterius generated: no types
import req from "./_generated/pandoc-wasm.req.mjs";
// @ts-expect-error Asterius generated: no types
import * as rts from "./_generated/rts.mjs";
import * as utils from "./utils";
import * as pako from "pako";
import crc32 from "crc/calculators/crc32";
import yaml from "js-yaml";

const srcUrl = (globalThis as any).document
  ? (document.currentScript as HTMLScriptElement).src
  : self.location.href;
const baseUrl = srcUrl.substring(0, srcUrl.lastIndexOf("/"));

export type PandocParams = {
  text: string;
  options: { [key: string]: any } & {
    from: string;
    to: string;
  };
  files?: { [key: string]: ArrayBufferLike | string };
  citeproc?: boolean;
};

export class Pandoc {
  static pako: typeof pako = pako;
  static crc32: typeof crc32 = crc32;
  static yaml: typeof yaml = yaml;
  #runQueue: Array<{
    params: PandocParams;
    resolve: (_: any) => void;
    reject: (_: any) => void;
  }> = [];
  instance: Promise<any>;
  wasm: Promise<WebAssembly.Module>;
  dataFiles: { [key: string]: ArrayBufferLike } = {};

  constructor() {
    this.wasm = fetch(`${baseUrl}/pandoc-wasm.wasm.gz`)
      .then((response) => response.arrayBuffer())
      .then((gz) => Pandoc.pako.ungzip(gz))
      .then((buf) => WebAssembly.compile(buf));

    this.instance = this.wasm.then((module) =>
      rts.newAsteriusInstance(Object.assign(req, { module }))
    );

    this.#downloadData();
    this.#installErrorHandler();
  }

  async init() {
    await this.instance;
    await this.#downloadData();
    return this;
  }

  async #downloadData() {
    if (Object.keys(this.dataFiles).length > 0) {
      return this.dataFiles;
    }

    const gz = await fetch(`${baseUrl}/pandoc-data.data.gz`);
    const data = Pandoc.pako.ungzip(await gz.arrayBuffer());
    const metaFile = await fetch(`${baseUrl}/pandoc-data.metadata`);
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
   * we're out of memory and try to recover by creating a new pandoc Wasm
   * instance from scratch.
   */
  #installErrorHandler() {
    (globalThis as any).onerror = (
      event: Event,
      source: string,
      lineno: number,
      colno: number,
      error: Error
    ) => {
      // Assume any uncaught RangeError is due to Asterius GC failure
      // TODO: Work out why GC fails, or switch to ghc-wasm-meta
      if (error.name === "RangeError") {
        this.#runQueue.forEach((q) =>
          q.reject("Out of memory in Wasm heap. Reinitialising Pandoc.")
        );

        // Restart the instance
        this.instance = this.wasm.then((module) =>
          rts.newAsteriusInstance(Object.assign(req, { module }))
        );
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
    let q: any;
    return new Promise<string>((resolve, reject) => {
      q = { params, resolve, reject };
      this.#runQueue.push(q);
      this.instance
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
    const instance = await this.instance;
    return await instance.exports.getVersion();
  }
}

(globalThis as any).Pandoc = Pandoc;
export default Pandoc;
