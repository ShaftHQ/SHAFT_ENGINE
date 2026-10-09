(async () => {
  const loading = document.getElementById('trace-loading');
  const fail = message => {
    loading.textContent = message;
    loading.setAttribute('role', 'alert');
  };
  const payload = document.getElementById('trace-data');
  let text;
  try {
    const encoded = payload.textContent.trim();
    if (payload.dataset.encoding !== 'gzip+base64') {
      text = encoded;
    } else if (typeof DecompressionStream !== 'function') {
      fail('This browser cannot open the trace: it does not support DecompressionStream. Use a current Chrome, Edge, Firefox or Safari.');
      return;
    } else {
      const binary = atob(encoded);
      const bytes = new Uint8Array(binary.length);
      for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
      const stream = new Blob([bytes]).stream().pipeThrough(new DecompressionStream('gzip'));
      text = await new Response(stream).text();
    }
  } catch (error) {
    fail('The embedded trace payload could not be decoded: ' + (error && error.message ? error.message : error));
    return;
  }
  window.shaftTraceText = text;
  document.body.insertBefore(document.getElementById('trace-viewer-shell').content.cloneNode(true), loading);
  loading.remove();
  const main = document.createElement('script');
  main.textContent = document.getElementById('trace-viewer-main').textContent;
  document.body.appendChild(main);
})();
