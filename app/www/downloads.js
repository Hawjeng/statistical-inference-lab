function saveLocalFile(content,name,mime){const blob=new Blob([content],{type:mime});const url=URL.createObjectURL(blob);const a=document.createElement('a');a.href=url;a.download=name;document.body.appendChild(a);a.click();a.remove();setTimeout(()=>URL.revokeObjectURL(url),30000);}
function downloadCurrentSVG(){const svg=document.querySelector('#survey_chart svg');if(!svg){alert('請先等待圖形完成。');return;}const clone=svg.cloneNode(true);clone.setAttribute('xmlns','http://www.w3.org/2000/svg');clone.setAttribute('width','960');clone.setAttribute('height',svg.viewBox.baseVal.height);const name=(document.querySelector('#variable')?.value||'zodiac')+'-CI.svg';saveLocalFile('<?xml version="1.0" encoding="UTF-8"?>\n'+new XMLSerializer().serializeToString(clone),name,'image/svg+xml;charset=utf-8');}
$(document).on('shiny:connected',function(){Shiny.addCustomMessageHandler('download-file',function(data){saveLocalFile(data.content,data.name,data.mime);});});
function analysisCode(button) {
  return button.closest('.code-columns').querySelector('.analysis-code code').textContent;
}
async function copyAnalysisCode(button) {
  const label = button.textContent;
  try {
    await navigator.clipboard.writeText(analysisCode(button));
    button.textContent = '已複製';
  } catch {
    button.textContent = '複製失敗，請選取語法';
  }
  setTimeout(() => { button.textContent = label; }, 2000);
}
function downloadAnalysisCode(button) {
  saveLocalFile(analysisCode(button) + '\n', button.dataset.filename, 'text/plain;charset=utf-8');
}
function downloadAnalysisData(button) {
  const csv = button.closest('.code-columns').querySelector('.analysis-data').textContent;
  saveLocalFile(csv + '\n', button.dataset.filename, 'text/csv;charset=utf-8');
}
function runAnalysisPython(button) {
  const columns = button.closest('.code-columns');
  const output = columns.querySelector('.python-console');
  const help = columns.querySelector('.python-help');
  const data = columns.querySelector('.analysis-data');

  button.disabled = true;
  help.hidden = true;
  output.textContent = '正在載入 Python 並執行語法。首次下載可能需要一點時間。';
  let worker;
  let timeout;
  function finish(message, failed) {
    clearTimeout(timeout);
    worker?.terminate();
    output.textContent = message;
    help.hidden = !failed;
    button.disabled = false;
  }
  try {
    worker = new Worker('python-console.js', { type: 'module' });
    worker.onmessage = ({ data }) => finish(data.output, !data.ok);
    worker.onerror = () => finish('瀏覽器無法啟動 Python 執行環境。', true);
    timeout = setTimeout(() => finish('Python 執行環境載入逾時。', true), 120000);
    worker.postMessage({
      code: analysisCode(button),
      filename: columns.querySelector('[onclick="downloadAnalysisData(this)"]').dataset.filename,
      csv: data.textContent
    });
  } catch (error) {
    finish('瀏覽器無法啟動 Python 執行環境。\n' + error.message, true);
  }
}
