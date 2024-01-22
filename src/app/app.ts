import * as Comlink from "comlink";
import { Pandoc, PandocParams } from "../pandoc";
import { base64ToArrayBuffer, arrayBufferToBase64 } from "../utils";

const PandocWorker: Comlink.Remote<typeof Pandoc> = Comlink.wrap(
  new Worker("./worker.js")
);
const pandocWorker = new PandocWorker();
(globalThis as any).pandocWorker = pandocWorker;

let dirty = false;
const params: PandocParams = {
  text: "",
  options: {
    from: "markdown",
    to: "html",
    'embed-resources': true,
  },
  citeproc: true,
};

const inputFormats = {
  markdown: { label: "markdown", binary: false },
  native: { label: "native", binary: false },
  json: { label: "json", binary: false },
  markdown_strict: { label: "markdown_strict", binary: false },
  markdown_phpextra: { label: "markdown_phpextra", binary: false },
  markdown_github: { label: "markdown_github", binary: false },
  markdown_mmd: { label: "markdown_mmd", binary: false },
  commonmark: { label: "commonmark", binary: false },
  commonmark_x: { label: "commonmark_x", binary: false },
  creole: { label: "creole", binary: false },
  dokuwiki: { label: "dokuwiki", binary: false },
  gfm: { label: "gfm", binary: false },
  rst: { label: "rst", binary: false },
  mediawiki: { label: "mediawiki", binary: false },
  vimwiki: { label: "vimwiki", binary: false },
  docbook: { label: "docbook", binary: false },
  opml: { label: "opml", binary: false },
  org: { label: "org", binary: false },
  textile: { label: "textile", binary: false },
  html: { label: "html", binary: false },
  bits: { label: "bits", binary: false },
  jats: { label: "jats", binary: false },
  jira: { label: "jira", binary: false },
  latex: { label: "latex", binary: false },
  haddock: { label: "haddock", binary: false },
  twiki: { label: "twiki", binary: false },
  tikiwiki: { label: "tikiwiki", binary: false },
  docx: { label: "docx", binary: true },
  odt: { label: "odt", binary: true },
  t2t: { label: "t2t", binary: false },
  epub: { label: "epub", binary: true },
  muse: { label: "muse", binary: false },
  man: { label: "man", binary: false },
  fb2: { label: "fb2", binary: false },
  ipynb: { label: "ipynb", binary: false },
  csv: { label: "csv", binary: false },
  tsv: { label: "tsv", binary: false },
  csljson: { label: "csljson", binary: false },
  bibtex: { label: "bibtex", binary: false },
  biblatex: { label: "biblatex", binary: false },
  endnotexml: { label: "endnotexml", binary: false },
  ris: { label: "ris", binary: false },
  rtf: { label: "rtf", binary: false },
};

const outputFormats = {
  html: { label: "HTML", binary: false, ext: ".html", mime: "text/html" },
  native: { label: "Native Haskell", binary: false },
  json: { label: "json", binary: false },
  docx: {
    label: "docx",
    binary: true,
    ext: ".docx",
    mime: "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  },
  odt: {
    label: "odt",
    binary: true,
    ext: ".odt",
    mime: "application/vnd.oasis.opendocument.text",
  },
  pptx: {
    label: "pptx",
    binary: true,
    ext: ".pptx",
    mime: "application/vnd.openxmlformats-officedocument.presentationml.presentation",
  },
  epub: {
    label: "epub",
    binary: true,
    ext: ".epub",
    mime: "application/epub+zip",
  },
  epub2: {
    label: "epub2",
    binary: true,
    ext: ".epub",
    mime: "application/epub+zip",
  },
  epub3: {
    label: "epub3",
    binary: true,
    ext: ".epub",
    mime: "application/epub+zip",
  },
  fb2: { label: "fb2", binary: false },
  ipynb: { label: "ipynb", binary: false },
  html4: { label: "html4", binary: false },
  html5: { label: "html5", binary: false },
  icml: { label: "icml", binary: false },
  s5: { label: "s5", binary: false },
  slidy: { label: "slidy", binary: false },
  slideous: { label: "slideous", binary: false },
  dzslides: { label: "dzslides", binary: false },
  revealjs: { label: "revealjs", binary: false },
  docbook: { label: "docbook", binary: false },
  docbook4: { label: "docbook4", binary: false },
  docbook5: { label: "docbook5", binary: false },
  jats: { label: "jats", binary: false },
  jats_articleauthoring: { label: "jats_articleauthoring", binary: false },
  jats_publishing: { label: "jats_publishing", binary: false },
  jats_archiving: { label: "jats_archiving", binary: false },
  jira: { label: "jira", binary: false },
  opml: { label: "opml", binary: false },
  opendocument: { label: "opendocument", binary: false },
  latex: { label: "latex", binary: false },
  beamer: { label: "beamer", binary: false },
  context: { label: "context", binary: false },
  texinfo: { label: "texinfo", binary: false },
  man: { label: "man", binary: false },
  ms: { label: "ms", binary: false },
  markdown: { label: "markdown", binary: false },
  markdown_strict: { label: "markdown_strict", binary: false },
  markdown_phpextra: { label: "markdown_phpextra", binary: false },
  markdown_github: { label: "markdown_github", binary: false },
  markdown_mmd: { label: "markdown_mmd", binary: false },
  plain: { label: "plain", binary: false },
  rst: { label: "rst", binary: false },
  mediawiki: { label: "mediawiki", binary: false },
  dokuwiki: { label: "dokuwiki", binary: false },
  xwiki: { label: "xwiki", binary: false },
  zimwiki: { label: "zimwiki", binary: false },
  textile: { label: "textile", binary: false },
  typst: { label: "typst", binary: false },
  rtf: { label: "rtf", binary: false },
  org: { label: "org", binary: false },
  asciidoc: { label: "asciidoc", binary: false },
  asciidoctor: { label: "asciidoctor", binary: false },
  asciidoc_legacy: { label: "asciidoc_legacy", binary: false },
  haddock: { label: "haddock", binary: false },
  commonmark: { label: "commonmark", binary: false },
  commonmark_x: { label: "commonmark_x", binary: false },
  gfm: { label: "gfm", binary: false },
  tei: { label: "tei", binary: false },
  muse: { label: "muse", binary: false },
  csljson: { label: "csljson", binary: false },
  bibtex: { label: "bibtex", binary: false },
  biblatex: { label: "biblatex", binary: false },
  markua: { label: "markua", binary: false },
  chunkedhtml: {
    label: "chunkedhtml",
    binary: true,
    ext: ".zip",
    mime: "application/zip",
  },
};

// Elements
const downloadMessage = document.getElementById("downloadMessage") as HTMLDivElement;
const inputSelect = document.getElementById("inputSelect") as HTMLSelectElement;
const inputTextArea = document.getElementById(
  "inputTextArea"
) as HTMLTextAreaElement;
const inputFile = document.getElementById("inputFile") as HTMLInputElement;
const supportFiles = document.getElementById(
  "supportFiles"
) as HTMLInputElement;
const outputTextArea = document.getElementById(
  "outputTextArea"
) as HTMLTextAreaElement;
const outputSelect = document.getElementById(
  "outputSelect"
) as HTMLSelectElement;
const versionText = document.getElementById("versionText") as HTMLSpanElement;
const downloadButton = document.getElementById(
  "downloadButton"
) as HTMLButtonElement;
const allControls = document.getElementById("allControls") as HTMLFieldSetElement;
const errorAlert = document.getElementById("errorAlert") as HTMLDivElement;
const errorAlertText = document.getElementById("errorAlertText") as HTMLElement;
const standaloneCheck = document.getElementById("standaloneCheck") as HTMLInputElement;
const embedCheck = document.getElementById("embedCheck") as HTMLInputElement;
const citeprocCheck = document.getElementById("citeprocCheck") as HTMLInputElement;


// Setup
pandocWorker.then(async (pandoc) => {
  await pandoc.init();
  allControls.disabled = false;
  const version = await pandoc.getVersion();
  versionText.innerText = version;
  downloadMessage.style.display = "none";
});

Object.entries(inputFormats).map(([name, props]) => {
  const opt = document.createElement("option");
  opt.value = name;
  opt.innerHTML = props.label;
  inputSelect.append(opt);
});

Object.entries(outputFormats).map(([name, props]) => {
  const opt = document.createElement("option");
  opt.value = name;
  opt.innerHTML = props.label;
  outputSelect.append(opt);
});

// UI
inputSelect.onchange = () => {
  params.options.from = inputSelect.value;
  if (inputFormats[params.options.from as keyof typeof inputFormats].binary) {
    inputTextArea.parentElement!.style.display = "none";
    inputFile.parentElement!.style.display = "block";
  } else {
    inputTextArea.parentElement!.style.display = "block";
    inputFile.parentElement!.style.display = "none";
  }
  dirty = true;
};

outputSelect.onchange = () => {
  params.options.to = outputSelect.value;
  if (outputFormats[params.options.to as keyof typeof outputFormats].binary) {
    outputTextArea.parentElement!.style.display = "none";
  } else {
    outputTextArea.parentElement!.style.display = "block";
  }
  dirty = true;
};

downloadButton.onclick = () => {
  const to = params.options.to as keyof typeof outputFormats;
  const data = outputFormats[to].binary
    ? base64ToArrayBuffer(outputTextArea.value)
    : Uint8Array.from(outputTextArea.value, (b) => b.charCodeAt(0));

  if ('mime' in outputFormats[to]) {
    const format = outputFormats[to] as typeof outputFormats['html'];
    downloadData(data, `output${format.ext}`, format.mime);
  } else {
    downloadData(data, "output.txt", "text/plain");
  }
};

inputTextArea.oninput = () => {
  params.text = inputTextArea.value;
  dirty = true;
};

inputFile.onchange = async () => {
  if (!inputFile.files) {
    return;
  }
  if (inputFile.files && inputFile.files.length > 1) {
    throw new Error("Can't upload multiple files, use support files.");
  }
  params.text = arrayBufferToBase64(await inputFile.files[0].arrayBuffer());
  dirty = true;
};

supportFiles.onchange = async () => {
  if (!supportFiles.files) {
    return;
  }
  params.files = Object.fromEntries(
    await Promise.all(
      Array.from(supportFiles.files).map(async (f) => [
        f.name,
        await f.arrayBuffer(),
      ])
    )
  );
  dirty = true;
};

standaloneCheck.onchange = () => {
  params.options.standalone = standaloneCheck.checked;
  dirty = true;
};

embedCheck.onchange = () => {
  params.options["self-contained"] = embedCheck.checked;
  params.options["embed-resources"] = embedCheck.checked;
  dirty = true;
};

citeprocCheck.onchange = () => {
  params.citeproc = citeprocCheck.checked;
  dirty = true;
};

// Data download
function downloadData(data: Uint8Array, name: string, mime: string) {
  const a = document.createElement("a");
  a.href = URL.createObjectURL(
    new Blob([data], {
      type: mime,
    })
  );
  a.download = name;
  a.style.display = "none";
  document.body.appendChild(a);
  a.click();
  a.remove();
}


// Pandoc runner
setInterval(() => {
  if (!dirty) {
    return;
  }
  dirty = false;

  pandocWorker
    .then((pandoc) => pandoc.runPandoc(params))
    .then((result) => {
      outputTextArea.value = result;
      if (outputFormats[params.options.to as keyof typeof outputFormats].binary) {
        outputTextArea.parentElement!.style.display = "none";
      } else {
        outputTextArea.parentElement!.style.display = "block";
      }
      downloadButton.parentElement!.style.display = "block";
      errorAlert.style.display = 'none';
    })
    .catch((err) => {
      console.error(err);
      errorAlert.style.display = 'block';
      errorAlertText.innerText = err;
      outputTextArea.value = '';
      outputTextArea.parentElement!.style.display = "none";
      downloadButton.parentElement!.style.display = "none";
    });
}, 100);
