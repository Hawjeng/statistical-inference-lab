const indexURL = 'https://cdn.jsdelivr.net/pyodide/v314.0.8/full/';
self.onmessage = async ({ data }) => {
  try {
    importScripts(indexURL + 'pyodide.js');
    const runtime = await loadPyodide({ indexURL });
    await runtime.loadPackagesFromImports(data.code);
    runtime.FS.writeFile(data.filename, data.csv, { encoding: 'utf8' });
    const lines = [];
    runtime.setStdout({ batched: line => lines.push(line) });
    runtime.setStderr({ batched: line => lines.push(line) });
    await runtime.runPythonAsync(data.code);
    self.postMessage(lines.join('\n'));
  } catch (error) {
    self.postMessage('Python 執行失敗\n' + error.message);
  }
};
