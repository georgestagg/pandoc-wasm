export function arrayBufferToBase64(buffer: ArrayBuffer) {
  let binary = "";
  const bytes = new Uint8Array(buffer);
  const len = bytes.byteLength;
  for (let i = 0; i < len; i++) {
    binary += String.fromCharCode(bytes[i]);
  }
  return globalThis.btoa(binary);
}

export function base64ToArrayBuffer(base64: string) {
  return Uint8Array.from(globalThis.atob(base64), (b) => b.charCodeAt(0));
}
