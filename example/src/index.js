import { Pandoc } from "pandoc-wasm";

const input = `# Heading

This is a simple markdown document.

## Subheading

_Italic_, **bold**, \`monospace\`, ~~strikethrough~~ text.

### Itemized lists

  * This
  * That
  * and the Other
`;
document.getElementById("input").innerHTML = input;

new Pandoc().init().then(async (pandoc) => {
  const result = await pandoc.run({
    text: input,
    options: { from: "markdown", to: "html" },
  });
  document.getElementById("output").innerHTML = result;
});
