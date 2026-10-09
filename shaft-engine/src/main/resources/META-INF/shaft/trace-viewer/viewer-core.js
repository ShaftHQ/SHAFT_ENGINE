// Pure helpers for the SHAFT trace viewer. The browser loads this before viewer.js;
// node unit tests (shaft-engine/src/test/js/trace-viewer-core.test.js) require it directly.
const RENDER_CHUNK = 200;
const BODY_PREVIEW_LIMIT = 2048;
const JAVA_KEYWORDS = new Set(('abstract assert boolean break byte case catch char class const continue default do double '
  + 'else enum extends final finally float for goto if implements import instanceof int interface long native new '
  + 'package private protected public record return short static strictfp super switch synchronized this throw '
  + 'throws transient try var void volatile while yield true false null').split(' '));
function esc(value){
  return String(value || '').replace(/[&<>"']/g, char => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[char]));
}
function statusClass(value){
  value = String(value || '').toLowerCase();
  if(value.includes('pass')) return 'passed';
  if(value.includes('fail') || value.includes('error')) return 'failed';
  if(value.includes('warn') || value.includes('skip')) return 'warn';
  return 'neutral';
}
function headerValue(headers, name){
  const wanted = String(name).toLowerCase();
  const match = Object.entries(headers || {}).find(([key]) => key.toLowerCase() === wanted);
  return match ? String(match[1]) : '';
}
function contentTypeOf(entry){
  const raw = headerValue(entry && entry.responseHeaders, 'content-type') || (entry && entry.mimeType) || '';
  return String(raw).split(';')[0].trim().toLowerCase();
}
function formatBody(text, contentType){
  const body = text == null ? '' : String(text);
  const truncated = body.length >= BODY_PREVIEW_LIMIT || /^\[omitted because .*\]$/.test(body);
  if (!body) return {kind:'empty', text:'', truncated:false};
  const type = String(contentType || '').toLowerCase();
  if (type.startsWith('image/') && /^[A-Za-z0-9+/=\s]+$/.test(body) && !truncated) {
    return {kind:'image', text:`data:${type};base64,${body.replace(/\s/g, '')}`, truncated};
  }
  const trimmed = body.trim();
  if (type.includes('json') || /^[\[{]/.test(trimmed)) {
    try { return {kind:'json', text:JSON.stringify(JSON.parse(trimmed), null, 2), truncated}; } catch (ignored) { /* not complete JSON */ }
  }
  return {kind:'text', text:body, truncated};
}
function parseStackFrames(stack){
  const frames = [];
  String(stack || '').split(/\r?\n/).forEach(line => {
    const match = /^\s*at\s+([^\s(]+)\(([^():]+)(?::(\d+))?\)/.exec(line);
    if (match) frames.push({text:line.trim(), method:match[1], file:match[2], line:match[3] ? Number(match[3]) : null});
  });
  return frames;
}
function callerLocation(caller){
  const match = /([A-Za-z0-9_$.-]+\.[A-Za-z]+):(\d+)/.exec(String(caller || ''));
  return match ? {file:match[1], line:Number(match[2])} : null;
}
function sameSourceFile(left, right){
  const base = value => String(value || '').split(/[\\/]/).pop();
  return Boolean(left && right) && base(left) === base(right);
}
function highlightJava(line){
  const pattern = /(\/\/.*$|\/\*.*?\*\/|"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*'|\b\d+(?:\.\d+)?[lLfFdD]?\b|@[A-Za-z_]\w*|\b[A-Za-z_$][\w$]*\b)/g;
  let html = '';
  let last = 0;
  String(line).replace(pattern, (token, ignored, offset) => {
    html += esc(String(line).slice(last, offset));
    last = offset + token.length;
    let kind = '';
    if (token.startsWith('//') || token.startsWith('/*')) kind = 'com';
    else if (token[0] === '"' || token[0] === "'") kind = 'str';
    else if (/^\d/.test(token)) kind = 'num';
    else if (token[0] === '@') kind = 'ann';
    else if (JAVA_KEYWORDS.has(token)) kind = 'kw';
    html += kind ? `<span class="tok-${kind}">${esc(token)}</span>` : esc(token);
    return token;
  });
  return html + esc(String(line).slice(last));
}
function artifactActionId(artifact){
  const metadata = (artifact && artifact.metadata) || {};
  if (metadata.actionId) return String(metadata.actionId);
  const id = String((artifact && artifact.id) || '');
  return id.startsWith('screenshot-') ? id.slice('screenshot-'.length) : '';
}
function groupArtifacts(artifacts){
  const groups = new Map();
  (artifacts || []).forEach(artifact => {
    const metadata = artifact.metadata || {};
    const key = metadata.sha256 && !artifact.omitted ? `sha256:${metadata.sha256}` : `path:${artifact.path}:${artifact.id}`;
    if (!groups.has(key)) groups.set(key, {key, primary:artifact, artifacts:[], actionIds:[]});
    const group = groups.get(key);
    group.artifacts.push(artifact);
    const actionId = artifactActionId(artifact);
    if (actionId && !group.actionIds.includes(actionId)) group.actionIds.push(actionId);
  });
  return [...groups.values()];
}
function readableArtifactName(artifact){
  const path = String((artifact && artifact.path) || 'Unknown');
  const base = path.split('/').pop();
  const hashed = /^([0-9a-f]{64})(\.[A-Za-z0-9]+)?$/i.exec(base);
  if (!hashed) return base;
  const kind = String((artifact && artifact.kind) || 'artifact');
  return `${kind} ${hashed[1].slice(0, 8)}${hashed[2] || ''}`;
}
function errorEntries(actionsList, exception){
  const entries = (actionsList || []).filter(action => statusClass(action.status) === 'failed').map(action => ({
    action, type:action.exception && action.exception.type || action.category || 'Action failed',
    message:[action.message, action.exception && action.exception.message].filter(Boolean).join(': ')
      || action.name || 'Failed action'}));
  if (exception && (exception.type || exception.message)
      && !entries.some(entry => entry.message && exception.message && entry.message.includes(exception.message))) {
    entries.push({action:null, type:exception.type || 'Exception', message:exception.message || ''});
  }
  return entries;
}
function parseSeleniumLocator(locator){
  const match = /^By\.(id|cssSelector|xpath|name|className|tagName|linkText|partialLinkText):\s*([\s\S]+)$/
    .exec(String(locator || '').trim());
  return match ? {strategy:match[1], value:match[2].trim()} : null;
}
function javaString(value){
  return '"' + String(value).replace(/\\/g, '\\\\').replace(/"/g, '\\"').replace(/\n/g, '\\n') + '"';
}
function xpathLiteral(value){
  const text = String(value);
  if (!text.includes("'")) return `'${text}'`;
  if (!text.includes('"')) return `"${text}"`;
  return 'concat(' + text.split("'").map(part => `'${part}'`).join(`, "'", `) + ')';
}
function attributeSelector(name, value){
  return `[${name}="${String(value).replace(/\\/g, '\\\\').replace(/"/g, '\\"')}"]`;
}
function looksGenerated(value){
  return /\d{4,}|[0-9a-f]{8,}|^(?:ember|react|mui|ng|radix|headlessui)[-_:]?\w*\d/i.test(String(value || ''));
}
// Ordered like SHAFT locator health advice: data-testid, stable id or name, tag plus text, then a CSS path.
function locatorCandidates(element){
  const info = element || {};
  const tag = String(info.tag || '*').toLowerCase();
  const candidates = [];
  if (info.testId) {
    const css = attributeSelector('data-testid', info.testId);
    candidates.push({kind:'data-testid', css, java:`By.cssSelector(${javaString(css)})`});
  }
  if (info.id && !looksGenerated(info.id)) {
    candidates.push({kind:'id', css:attributeSelector('id', info.id), java:`By.id(${javaString(info.id)})`});
  }
  if (info.name) {
    candidates.push({kind:'name', css:attributeSelector('name', info.name), java:`By.name(${javaString(info.name)})`});
  }
  const text = String(info.text || '').replace(/\s+/g, ' ').trim();
  if (text && text.length <= 80) {
    candidates.push({kind:'text', xpath:`//${tag}[normalize-space(.)=${xpathLiteral(text)}]`,
      java:`SHAFT.GUI.Locator.hasTagName(${javaString(tag)}).hasText(${javaString(text)}).build()`});
  }
  if (info.cssPath) {
    candidates.push({kind:'css-path', css:info.cssPath, java:`By.cssSelector(${javaString(info.cssPath)})`});
  }
  return candidates;
}
const ISO_TIME = /\b(\d{4}-\d{2}-\d{2}[T ]\d{2}:\d{2}:\d{2}(?:[.,]\d{1,9})?(?:Z|[+-]\d{2}:?\d{2})?)\b/;
function logLineTime(line){
  const match = ISO_TIME.exec(String(line || ''));
  if (!match) return null;
  const parsed = Date.parse(match[1].replace(' ', 'T').replace(',', '.'));
  return Number.isNaN(parsed) ? null : parsed;
}
function filmstripActions(actionsList, includeAll){
  const list = actionsList || [];
  return includeAll ? list : list.filter(action => Boolean(action && action.screenshot));
}
// Theme a host forced onto the page background, or null. Allure 3 paints its dark background
// onto HTML attachments with !important; a transparent root means the host set nothing.
function themeFromBackground(color){
  const match = /rgba?\(\s*([\d.]+)[,\s]+([\d.]+)[,\s]+([\d.]+)(?:[,\s/]+([\d.]+))?/.exec(String(color || ''));
  if (!match || (match[4] !== undefined && Number(match[4]) === 0)) return null;
  const luminance = 0.299 * Number(match[1]) + 0.587 * Number(match[2]) + 0.114 * Number(match[3]);
  return luminance < 128 ? 'dark' : 'light';
}
// Theme precedence: a manual choice, then ?theme=, then the hosting report, then a framed default of light.
function resolveTheme(stored, query, host, framed){
  for (const candidate of [stored, query, host]) {
    if (candidate === 'dark' || candidate === 'light') return candidate;
  }
  return framed ? 'light' : null;
}
const PANE_MIN = 200;
const PANE_MAX = 640;
function clampPaneWidth(width, available){
  const max = Math.max(PANE_MIN, Math.min(PANE_MAX, (Number(available) || PANE_MAX * 2) - 360));
  return Math.round(Math.max(PANE_MIN, Math.min(max, Number(width) || 320)));
}
// Next index in a list for a navigation key, or -1 when the key does not navigate.
function navigationIndex(key, current, length){
  if (!length) return -1;
  if (key === 'Home') return 0;
  if (key === 'End') return length - 1;
  const step = {ArrowRight:1, ArrowDown:1, j:1, ArrowLeft:-1, ArrowUp:-1, k:-1}[key];
  if (!step) return -1;
  return current < 0 ? 0 : Math.max(0, Math.min(length - 1, current + step));
}
if (typeof module !== 'undefined' && module.exports) {
  module.exports = {RENDER_CHUNK, BODY_PREVIEW_LIMIT, esc, statusClass, headerValue, contentTypeOf, formatBody,
    parseStackFrames, callerLocation, sameSourceFile, highlightJava, artifactActionId, groupArtifacts,
    readableArtifactName, errorEntries, parseSeleniumLocator, javaString, xpathLiteral, attributeSelector,
    looksGenerated, locatorCandidates, logLineTime, filmstripActions, themeFromBackground, resolveTheme,
    clampPaneWidth, navigationIndex, PANE_MIN, PANE_MAX};
}
