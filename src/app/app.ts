import * as Comlink from "comlink";
import { Pandoc, PandocParams } from "../pandoc";
import { base64ToArrayBuffer, arrayBufferToBase64 } from "../utils";
import { inputFormats, outputFormats } from "./formats";

const PandocWorker: Comlink.Remote<typeof Pandoc> = Comlink.wrap(
  new Worker("./worker.js")
);
const pandocWorker = new PandocWorker();
(globalThis as any).pandocWorker = pandocWorker;

const status: {
  dirty: boolean;
  busy: boolean;
  data: Uint8Array | null;
} = {
  dirty: false,
  busy: false,
  data: new Uint8Array(0),
};

const params: PandocParams = {
  text: "",
  options: {
    from: "markdown",
    to: "html",
    "embed-resources": true,
  },
  citeproc: true,
};

// Elements
const downloadMessage = document.getElementById(
  "downloadMessage"
) as HTMLDivElement;
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
const allControls = document.getElementById(
  "allControls"
) as HTMLFieldSetElement;
const errorAlert = document.getElementById("errorAlert") as HTMLDivElement;
const errorAlertText = document.getElementById("errorAlertText") as HTMLElement;
const standaloneCheck = document.getElementById(
  "standaloneCheck"
) as HTMLInputElement;
const embedCheck = document.getElementById("embedCheck") as HTMLInputElement;
const citeprocCheck = document.getElementById(
  "citeprocCheck"
) as HTMLInputElement;

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
  outputTextArea.value = "";
  downloadButton.disabled = true;
  params.options.from = inputSelect.value;
  if (inputFormats[params.options.from as keyof typeof inputFormats].binary) {
    inputTextArea.parentElement!.style.display = "none";
    inputFile.parentElement!.style.display = "block";
  } else {
    inputTextArea.parentElement!.style.display = "block";
    inputFile.parentElement!.style.display = "none";
  }
  status.dirty = true;
};

outputSelect.onchange = () => {
  outputTextArea.value = "";
  downloadButton.disabled = true;
  params.options.to = outputSelect.value;
  if (outputFormats[params.options.to as keyof typeof outputFormats].binary) {
    outputTextArea.parentElement!.style.display = "none";
  } else {
    outputTextArea.parentElement!.style.display = "block";
  }
  status.dirty = true;
};

inputTextArea.oninput = () => {
  params.text = inputTextArea.value;
  status.dirty = true;
};

inputFile.onchange = async () => {
  if (!inputFile.files) {
    return;
  }
  if (inputFile.files && inputFile.files.length > 1) {
    throw new Error("Can't upload multiple files, use support files.");
  }
  params.text = arrayBufferToBase64(await inputFile.files[0].arrayBuffer());
  status.dirty = true;
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
  status.dirty = true;
};

standaloneCheck.onchange = () => {
  params.options.standalone = standaloneCheck.checked;
  status.dirty = true;
};

embedCheck.onchange = () => {
  params.options["self-contained"] = embedCheck.checked;
  params.options["embed-resources"] = embedCheck.checked;
  status.dirty = true;
};

citeprocCheck.onchange = () => {
  params.citeproc = citeprocCheck.checked;
  status.dirty = true;
};

// Data download
downloadButton.onclick = () => {
  const to = params.options.to as keyof typeof outputFormats;
  const data =
    status.data && outputFormats[to].binary
      ? status.data
      : Uint8Array.from(outputTextArea.value, (b) => b.charCodeAt(0));


  const format = outputFormats[to] as (typeof outputFormats)["html"];
  downloadData(data, `output${format.ext}`, format.mime);
};

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
  if (!status.dirty || status.busy) {
    return;
  }
  status.data = null;
  status.dirty = false;
  status.busy = true;

  pandocWorker
    .then((pandoc) => pandoc.run(params))
    .then((result) => {
      if (
        outputFormats[params.options.to as keyof typeof outputFormats].binary
      ) {
        outputTextArea.parentElement!.style.display = "none";
        status.data = base64ToArrayBuffer(result);
      } else {
        outputTextArea.parentElement!.style.display = "block";
        outputTextArea.value = result;
      }

      downloadButton.parentElement!.style.display = "block";
      errorAlert.style.display = "none";
      downloadButton.disabled = false;
    })
    .catch((err) => {
      console.error(err);
      errorAlert.style.display = "block";
      errorAlertText.innerText = err;
      outputTextArea.parentElement!.style.display = "none";
      downloadButton.parentElement!.style.display = "none";
    })
    .finally(() => {
      status.busy = false;
    });
}, 100);
