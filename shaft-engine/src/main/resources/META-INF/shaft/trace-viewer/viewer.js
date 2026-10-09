const trace = JSON.parse(window.shaftTraceText);
const truncation = JSON.parse(document.getElementById('trace-truncation').textContent);
const evidence = trace && trace.evidence && typeof trace.evidence === 'object'
    ? trace.evidence : trace;
const actions = Array.isArray(evidence.actions) ? evidence.actions : [];
const network = Array.isArray(evidence.network) ? evidence.network : [];
const consoleEvents = Array.isArray(evidence.console) ? evidence.console : [];
const playwright = evidence.playwright && typeof evidence.playwright === 'object'
    ? evidence.playwright : {status:'unavailable', reason:'', actions:[], correlations:[], snapshots:{}};
const nativeActions = Array.isArray(playwright.actions) ? playwright.actions : [];
const nativeSnapshots = playwright.snapshots && typeof playwright.snapshots === 'object'
    ? playwright.snapshots : {};
const browserObservability = evidence.browserObservability && typeof evidence.browserObservability === 'object'
    ? evidence.browserObservability : {warnings:[], webSockets:[]};
const webSockets = Array.isArray(browserObservability.webSockets)
    ? browserObservability.webSockets : [];
const artifacts = Array.isArray(trace.session && trace.session.artifacts)
    ? trace.session.artifacts : [];
const actionList = document.getElementById('action-list');
const actionSearch = document.getElementById('action-search');
const details = document.getElementById('details');
const tabContent = document.getElementById('tab-content');
function hashState(){
  const raw = String(location.hash || '').replace(/^#/, '');
  if (!raw.startsWith('action-')) return {actionId:null, params:new URLSearchParams()};
  const separator = raw.indexOf('?');
  const encodedId = separator < 0 ? raw.slice(7) : raw.slice(7, separator);
  try {
    return {actionId:decodeURIComponent(encodedId),
      params:new URLSearchParams(separator < 0 ? '' : raw.slice(separator + 1))};
  } catch (ignored) {
    return {actionId:null, params:new URLSearchParams()};
  }
}
function actionFromHash(){
  const state = hashState();
  return state.actionId ? actions.find(action => action.id === state.actionId) : null;
}
let selected = actionFromHash()
    || [...actions].reverse().find(action => action.status !== 'passed')
    || actions[0] || null;
let selectedNativeAction = null;
function selectAction(action, selectItsRange = true, historyMode = 'push'){
  selectedNativeAction = null;
  sourceLineOverride = null;
  selected = action;
  if (selectItsRange && action) {
    const start = actionStartMs(action);
    if (start != null) {
      rangeStartMs = start;
      rangeEndMs = Math.max(start, start + Math.max(0, action.durationMs || 0));
    }
  }
  updateHash(historyMode);
  renderNavigator();
  renderActions();
  renderDetails();
}
function actionStartMs(action){
  const t = Date.parse(action && action.startTime);
  return isNaN(t) ? null : t;
}
function networkStartMs(entry){
  return entry.timestamp ? entry.timestamp - (entry.durationMs || 0) : null;
}
function networkFailed(entry){
  return !entry.status || entry.status >= 400 || Boolean(entry.failureReason);
}
function consoleFailed(entry){
  const level = String(entry.level || '').toUpperCase();
  return level.includes('SEVERE') || level.includes('ERROR');
}
function timelineEntries(){
  const entries = [];
  actions.forEach(action => entries.push({t: actionStartMs(action), kind: 'action',
      status: statusClass(action.status), durationMs: action.durationMs,
      label: `${action.name || 'Action'}  ${action.locator || ''}`.trim(), action}));
  network.forEach(entry => entries.push({t: networkStartMs(entry), kind: 'network',
      status: networkFailed(entry) ? 'failed' : 'neutral', durationMs: entry.durationMs,
      label: `${entry.method || ''} ${entry.status || 'FAILED'} ${entry.url || ''}${entry.failureReason ? ' - ' + entry.failureReason : ''}`.trim()}));
  consoleEvents.forEach(entry => entries.push({t: entry.timestamp || null, kind: 'console',
      status: consoleFailed(entry) ? 'failed' : 'neutral',
      label: `${entry.level || ''} ${entry.message || ''}`.trim()}));
  return entries.sort((left, right) => (left.t ?? Infinity) - (right.t ?? Infinity));
}
const allEntries = timelineEntries();
const baseTime = allEntries.reduce((min, entry) => entry.t != null && (min == null || entry.t < min) ? entry.t : min, null);
const traceEnd = allEntries.reduce((max, entry) => entry.t == null ? max
    : Math.max(max == null ? entry.t : max, entry.t + Math.max(0, entry.durationMs || 0)), baseTime);
const traceDuration = Math.max(1, (traceEnd ?? 1) - (baseTime ?? 0));
let rangeStartMs = baseTime;
let rangeEndMs = traceEnd;
const initialHash = hashState();
const initialHashRange = initialHash.params;
if (baseTime != null && initialHashRange.has('start') && initialHashRange.has('end')) {
  const startOffset = Number(initialHashRange.get('start'));
  const endOffset = Number(initialHashRange.get('end'));
  if (Number.isFinite(startOffset) && Number.isFinite(endOffset)) {
    rangeStartMs = baseTime + Math.max(0, Math.min(traceDuration, startOffset));
    rangeEndMs = baseTime + Math.max(0, Math.min(traceDuration, endOffset));
     if (rangeStartMs > rangeEndMs) [rangeStartMs, rangeEndMs] = [rangeEndMs, rangeStartMs];
   }
} else if (baseTime != null && initialHash.actionId && selected) {
  const start = actionStartMs(selected);
  if (start != null) {
    rangeStartMs = start;
    rangeEndMs = Math.max(start, start + Math.max(0, selected.durationMs || 0));
  }
}
function offsetLabel(t){
  return t == null || baseTime == null ? '' : '+' + ((t - baseTime) / 1000).toFixed(3) + 's';
}
function selectedWindow(){
  return rangeStartMs == null || rangeEndMs == null ? null : [rangeStartMs, rangeEndMs];
}
function inWindow(t, range){
  return range != null && t != null && t >= range[0] && t <= range[1];
}
function intervalOverlaps(start, durationMs, range){
  if (start == null || range == null) return false;
  const end = start + Math.max(0, durationMs || 0);
  return end >= range[0] && start <= range[1];
}
function actionInWindow(action, range){
  return intervalOverlaps(actionStartMs(action), action.durationMs, range);
}
function updateHash(mode = 'replace'){
  if (mode === 'none' || !selected || !selected.id) return;
  const start = baseTime == null || rangeStartMs == null ? 0 : Math.round(rangeStartMs - baseTime);
  const end = baseTime == null || rangeEndMs == null ? traceDuration : Math.round(rangeEndMs - baseTime);
  const hash = '#action-' + encodeURIComponent(selected.id) + '?start=' + start + '&end=' + end;
  if (mode === 'push') history.pushState(null, '', hash);
  else history.replaceState(null, '', hash);
}
function renderChunked(container, items, renderItem, options = {}){
  const chunk = Math.max(RENDER_CHUNK, (options.ensureIndex ?? -1) + 1);
  let shown = 0;
  const showMore = () => {
    const end = Math.min(items.length, shown + (shown ? RENDER_CHUNK : chunk));
    const fragment = document.createDocumentFragment();
    for (; shown < end; shown++) fragment.appendChild(renderItem(items[shown], shown));
    const previous = container.querySelector(':scope > .list-more');
    if (previous) previous.remove();
    container.appendChild(fragment);
    if (shown >= items.length) return;
    const remaining = items.length - shown;
    const host = document.createElement(options.columns ? 'tr' : 'div');
    host.className = 'list-more';
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'secondary';
    button.textContent = `Show ${Math.min(RENDER_CHUNK, remaining)} more (${remaining} not shown)`;
    button.addEventListener('click', showMore);
    if (options.columns) {
      const cell = document.createElement('td');
      cell.colSpan = options.columns;
      cell.appendChild(button);
      host.appendChild(cell);
    } else {
      host.appendChild(button);
    }
    container.appendChild(host);
  };
  showMore();
}
const actionSearchText = new WeakMap();
function searchableAction(action){
  if (!actionSearchText.has(action)) {
    const {screenshot, domSnapshotBefore, domSnapshotAfter, ...rest} = action;
    actionSearchText.set(action, JSON.stringify(rest).toLowerCase());
  }
  return actionSearchText.get(action);
}
function renderSummary(){
  const test = trace.test || {};
  const exception = trace.exception || {};
  const failedActions = actions.filter(action => action.status === 'failed').length;
  const failedNetwork = network.filter(networkFailed).length;
  const consoleErrors = consoleEvents.filter(consoleFailed).length;
  const attempt = parseInt(test.attempt, 10) || 1;
  const retried = String(test.retried) === 'true';
  const attemptSuffix = attempt > 1 || retried ? ` - attempt ${attempt}${retried ? ' (retried)' : ''}` : '';
  document.getElementById('trace-subtitle').textContent = `${test.className || 'Unknown class'}.${test.methodName || 'unknown'}${attemptSuffix} - ${trace.generatedAt || ''}`;
  const attemptChip = attempt > 1 || retried
    ? `<span class="status-chip warn">attempt ${attempt}${retried ? ' retried' : ''}</span>`
    : '';
  document.getElementById('trace-summary').innerHTML = `
    <h2>Investigation</h2>
    <div class="trace-status-strip">
      <span class="status-chip ${statusClass(test.status)}">${esc(test.status || 'unknown')}</span>
      ${attemptChip}
      <span class="status-meta">${actions.length} actions${failedActions ? ` · ${failedActions} failed` : ''}</span>
      <span class="status-meta">${network.length} network${failedNetwork ? ` · ${failedNetwork} failed` : ''}</span>
      <span class="status-meta">${consoleErrors} console errors</span>
      <span class="status-meta">${esc(exception.type || 'No exception')}</span>
    </div>`;
  if (truncation.length) {
    document.getElementById('truncation-banner').hidden = false;
    const artifactReasons = new Map(artifacts.filter(artifact => artifact.omitted)
      .map(artifact => [artifact.path, artifact.metadata && artifact.metadata.omissionReason]));
    const omittedDetails = truncation.map(path => artifactReasons.get(path)
      ? `${path}: ${artifactReasons.get(path)}`
      : `${path}: exceeded shaft.trace.maxArtifactMb and was replaced with an omission marker`);
    document.getElementById('truncation-detail').textContent =
      `Some bundle entries were omitted: ${omittedDetails.join('; ')}.`;
  } else {
    document.getElementById('truncation-banner').hidden = true;
    document.getElementById('truncation-detail').textContent = '';
  }
}
const filmstrip = document.getElementById('trace-filmstrip');
const rangeStart = document.getElementById('range-start');
const rangeEnd = document.getElementById('range-end');
const rangeLabel = document.getElementById('range-label');
function applyRangeInputs(historyMode = 'none'){
  if (baseTime == null) return;
  selectedNativeAction = null;
  const startOffset = Math.max(0, Math.min(traceDuration, Number(rangeStart.value)));
  const endOffset = Math.max(0, Math.min(traceDuration, Number(rangeEnd.value)));
  rangeStartMs = baseTime + Math.min(startOffset, endOffset);
  rangeEndMs = baseTime + Math.max(startOffset, endOffset);
  updateHash(historyMode);
  renderNavigator();
  renderActions();
  renderDetails();
}
function renderNavigator(){
  const startOffset = baseTime == null || rangeStartMs == null ? 0 : Math.max(0, rangeStartMs - baseTime);
  const endOffset = baseTime == null || rangeEndMs == null ? traceDuration : Math.max(0, rangeEndMs - baseTime);
  rangeStart.max = String(traceDuration);
  rangeEnd.max = String(traceDuration);
  rangeStart.value = String(Math.min(startOffset, endOffset));
  rangeEnd.value = String(Math.max(startOffset, endOffset));
  rangeStart.disabled = baseTime == null;
  rangeEnd.disabled = baseTime == null;
  rangeLabel.value = baseTime == null ? 'No timed evidence'
      : `${offsetLabel(rangeStartMs)} to ${offsetLabel(rangeEndMs)}`;
  renderErrorMarkers();
  filmstrip.innerHTML = '';
  if (!actions.length) {
    filmstrip.textContent = 'No actions were recorded for the filmstrip.';
    return;
  }
  const range = selectedWindow();
  actions.forEach(action => {
    const button = document.createElement('button');
    button.type = 'button';
    button.setAttribute('role', 'option');
    button.setAttribute('aria-selected', String(Boolean(selected && selected.id === action.id)));
    button.tabIndex = selected && selected.id === action.id ? 0 : -1;
    button.dataset.actionId = action.id || '';
    button.className = `${selected && selected.id === action.id ? 'selected ' : ''}${actionInWindow(action, range) ? 'inwindow' : ''}`.trim();
    if (action.screenshot) {
      const image = document.createElement('img');
      image.alt = '';
      image.src = 'data:image/png;base64,' + action.screenshot;
      button.appendChild(image);
    } else {
      const missing = document.createElement('span');
      missing.className = 'filmstrip-missing';
      missing.textContent = 'No screenshot';
      button.appendChild(missing);
    }
    const label = document.createElement('span');
    label.textContent = `${offsetLabel(actionStartMs(action))} ${action.name || 'Action'}`.trim();
    button.appendChild(label);
    button.addEventListener('click', () => selectAction(action));
    filmstrip.appendChild(button);
  });
}
function renderActions(){
  actionList.innerHTML = '';
  if(!actions.length){ actionList.textContent = 'No structured actions recorded.'; return; }
  const query = actionSearch.value.toLowerCase();
  const visible = actions.filter(action => !query || searchableAction(action).includes(query));
  renderChunked(actionList, visible, action => {
    const button = document.createElement('button');
    button.className = `action ${action.status}${selected && selected.id === action.id ? ' selected' : ''}${actionInWindow(action, selectedWindow()) ? ' inwindow' : ''}`;
    button.innerHTML = `<strong>${esc(action.name || 'Action')}</strong><div class="muted">${esc(action.category)} - ${esc(action.status)} - ${esc(action.durationMs || 0)}ms${action.screenshot ? ' 📷' : ''}</div>`;
    button.addEventListener('click', () => selectAction(action));
    return button;
  }, {ensureIndex: selected ? visible.indexOf(selected) : -1});
}
function row(name, value){ return value ? `<dt>${esc(name)}</dt><dd>${esc(value)}</dd>` : ''; }
function nativeActionFor(action){
  if (selectedNativeAction) return selectedNativeAction;
  const callId = action && action.metadata && action.metadata.playwrightCallId;
  return callId ? nativeActions.find(candidate => candidate.callId === callId) || null : null;
}
function renderDetails(){
  const action = selected || {};
  document.getElementById('details-title').textContent = action.name ? `Action: ${action.name}` : 'Trace Details';
  const metadata = action.metadata || {};
  details.innerHTML = row('Status', action.status) + row('Category', action.category)
    + row('Expected', metadata.expected) + row('Actual', metadata.actual)
    + row('Locator', action.locator) + row('URL', action.url) + row('Caller', action.caller) + row('Started', action.startTime) + row('Duration', action.durationMs == null ? '' : `${action.durationMs}ms`) + row('Message', action.message);
  const native = nativeActionFor(action);
  details.innerHTML += row('Native fidelity', native ? 'Playwright correlated' : 'SHAFT capture')
    + row('Native source', native && (native.source || native.sourceReason))
    + row('Native error', native && native.error);
  renderTab(document.querySelector('.tabs button.selected').dataset.tab);
}
const timelinePanel = document.getElementById('timeline-panel');
const timelineList = document.getElementById('timeline-list');
let timelineFilter = 'all';
function matchesTimelineFilter(entry){
  if (timelineFilter === 'all') return true;
  if (timelineFilter === 'failed') return entry.status === 'failed';
  if (timelineFilter === 'validation') return entry.kind === 'action' && entry.action && entry.action.category === 'validation';
  return entry.kind === timelineFilter;
}
function renderTimeline(){
  timelineList.innerHTML = '';
  if (!allEntries.length) { timelineList.textContent = 'No timeline events were recorded.'; return; }
  const visibleEntries = allEntries.filter(matchesTimelineFilter);
  if (!visibleEntries.length) { timelineList.textContent = 'No timeline events match this filter.'; return; }
  const range = selectedWindow();
  renderChunked(timelineList, visibleEntries, entry => {
    const div = document.createElement('div');
    const isSelected = entry.action && selected && entry.action.id === selected.id;
    const overlaps = entry.kind === 'console'
        ? inWindow(entry.t, range)
        : intervalOverlaps(entry.t, entry.durationMs, range);
    div.className = `timeline-entry ${entry.status}${entry.action ? ' clickable' : ''}${isSelected ? ' selected' : ''}${overlaps ? ' inwindow' : ''}`;
    const duration = entry.durationMs ? ` (${entry.durationMs}ms)` : '';
    div.innerHTML = `<span class="time-cell">${esc(offsetLabel(entry.t))}</span><span class="badge kind-${entry.kind}">${entry.kind.toUpperCase()}</span><span class="timeline-label">${esc(entry.label)}${esc(duration)}</span>`;
    if (entry.action) {
      div.addEventListener('click', () => selectAction(entry.action));
    }
    return div;
  });
}
const networkPanel = document.getElementById('network-panel');
const networkRows = document.getElementById('network-rows');
const networkDetail = document.getElementById('network-detail');
const networkMethodFilter = document.getElementById('network-method-filter');
const networkStatusFilter = document.getElementById('network-status-filter');
const networkTextFilter = document.getElementById('network-text-filter');
const networkResultCount = document.getElementById('network-result-count');
let networkSort = {key:'method', direction:'ascending'};
function finiteNumber(value){ return typeof value === 'number' && Number.isFinite(value) ? value : null; }
function networkStatus(entry){
  const status = finiteNumber(entry.status);
  return status == null ? 'Unknown' : status > 0 ? String(status) : 'FAILED';
}
function networkType(entry){ return entry.type ? String(entry.type) : 'HTTP'; }
function headerSearchText(headers){
  return Object.entries(headers || {}).map(([name, value]) => `${name}: ${value}`).join(' ');
}
function networkSearchText(entry){
  return [entry.url, headerSearchText(entry.requestHeaders),
    headerSearchText(entry.responseHeaders), entry.bodyPreview]
    .filter(value => value != null).join(' ').toLowerCase();
}
function networkSize(entry){
  const request = finiteNumber(entry.requestSizeBytes);
  const response = finiteNumber(entry.responseSizeBytes);
  return request == null || response == null ? null : request + response;
}
function networkSortValue(entry, key){
  let value = null;
  if (key === 'time') value = finiteNumber(networkStartMs(entry));
  if (key === 'type') value = networkType(entry);
  if (key === 'method') value = entry.method ? String(entry.method) : null;
  if (key === 'status') value = finiteNumber(entry.status);
  if (key === 'duration') value = finiteNumber(entry.durationMs);
  if (key === 'size') value = networkSize(entry);
  if (key === 'contentType') value = contentTypeOf(entry) || null;
  return {missing:value == null, value};
}
function compareNetwork(left, right){
  const a = networkSortValue(left.entry, networkSort.key);
  const b = networkSortValue(right.entry, networkSort.key);
  if (a.missing !== b.missing) return a.missing ? 1 : -1;
  if (a.missing) return left.index - right.index;
  let comparison = typeof a.value === 'number' && typeof b.value === 'number'
    ? a.value - b.value : String(a.value).localeCompare(String(b.value));
  if (networkSort.direction === 'descending') comparison = -comparison;
  return comparison || left.index - right.index;
}
function populateNetworkFilters(){
  const append = (select, values) => values.forEach(value => {
    const option = document.createElement('option');
    option.value = value;
    option.textContent = value;
    select.appendChild(option);
  });
  append(networkMethodFilter, [...new Set(network.map(entry => String(entry.method || 'UNKNOWN')))].sort());
  append(networkStatusFilter, [...new Set(network.map(networkStatus))].sort());
}
function updateNetworkSortHeaders(){
  document.querySelectorAll('[data-network-sort]').forEach(button => {
    const header = button.closest('th');
    if (button.dataset.networkSort === networkSort.key) {
      header.setAttribute('aria-sort', networkSort.direction);
    } else {
      header.removeAttribute('aria-sort');
    }
  });
}
function renderNetwork(){
  const range = selectedWindow();
  networkRows.innerHTML = '';
  networkDetail.hidden = true;
  const query = networkTextFilter.value.trim().toLowerCase();
  const visible = network.map((entry, index) => ({entry, index})).filter(({entry}) =>
    (finiteNumber(networkStartMs(entry)) == null
      || intervalOverlaps(networkStartMs(entry), entry.durationMs, range))
    && (!networkMethodFilter.value || String(entry.method || 'UNKNOWN') === networkMethodFilter.value)
    && (!networkStatusFilter.value || networkStatus(entry) === networkStatusFilter.value)
    && (!query || networkSearchText(entry).includes(query)))
    .sort(compareNetwork);
  networkResultCount.textContent = `${visible.length} network ${visible.length === 1 ? 'exchange' : 'exchanges'}`;
  document.getElementById('network-hint').textContent = !network.length
    ? 'No network exchanges were recorded.'
    : !visible.length ? 'No network exchanges match the selected range and filters.'
    : 'Use View request details to inspect headers and body preview.';
  renderChunked(networkRows, visible, ({entry}) => {
    const tr = document.createElement('tr');
    const timed = finiteNumber(networkStartMs(entry)) != null;
    tr.className = `${networkFailed(entry) ? 'failed ' : ''}${timed ? 'inwindow' : ''}`.trim();
    const duration = finiteNumber(entry.durationMs);
    const size = networkSize(entry);
    tr.innerHTML = `<td class="time-cell">${esc(offsetLabel(networkStartMs(entry)))}</td><td>${esc(networkType(entry))}</td><td>${esc(entry.method || 'Unknown')}</td><td>${esc(networkStatus(entry))}</td><td>${duration == null ? 'Unknown' : `${duration}ms`}</td><td>${size == null ? 'Unknown' : `${size} B`}</td><td>${esc(contentTypeOf(entry) || 'Unknown')}</td><td>${esc(entry.url || 'Unknown')}</td><td><button type="button" class="secondary">View request details</button></td>`;
    tr.querySelector('button').addEventListener('click', () => showNetworkDetail(entry));
    return tr;
  }, {columns: 9});
  updateNetworkSortHeaders();
}
function headerRows(target, headers){
  const entries = Object.entries(headers || {});
  target.innerHTML = entries.length
    ? entries.map(([name, value]) => `<tr><th scope="row">${esc(name)}</th><td>${esc(value)}</td></tr>`).join('')
    : '<tr><td class="muted">No headers were recorded.</td></tr>';
}
function showNetworkDetail(entry){
  networkDetail.hidden = false;
  const contentType = contentTypeOf(entry);
  document.getElementById('network-detail-title').textContent = `${entry.method || 'Request'} ${entry.url || ''}`.trim();
  const duration = finiteNumber(entry.durationMs);
  document.getElementById('network-detail-general').innerHTML = row('URL', entry.url) + row('Method', entry.method)
    + row('Status', networkStatus(entry)) + row('Type', networkType(entry)) + row('Content type', contentType)
    + row('Duration', duration == null ? '' : `${duration}ms`)
    + row('Request size', finiteNumber(entry.requestSizeBytes) == null ? '' : `${entry.requestSizeBytes} B`)
    + row('Response size', finiteNumber(entry.responseSizeBytes) == null ? '' : `${entry.responseSizeBytes} B`)
    + row('Failure', entry.failureReason) + row('Provider', entry.provider);
  headerRows(document.getElementById('network-request-headers'), entry.requestHeaders);
  headerRows(document.getElementById('network-response-headers'), entry.responseHeaders);
  const requestBytes = finiteNumber(entry.requestSizeBytes);
  document.getElementById('network-request-body').textContent = entry.requestBody
    ? entry.requestBody
    : requestBytes ? `Request body (${requestBytes} B) is not retained in the trace; only its size is recorded.`
    : 'No request body was sent.';
  const body = formatBody(entry.bodyPreview, contentType);
  const bodyText = document.getElementById('network-response-body');
  const bodyImage = document.getElementById('network-response-image');
  bodyImage.hidden = body.kind !== 'image';
  bodyImage.src = body.kind === 'image' ? body.text : '';
  bodyText.hidden = body.kind === 'image';
  bodyText.textContent = body.kind === 'empty' ? 'No response body preview was recorded.' : body.kind === 'image' ? '' : body.text;
  bodyText.dataset.kind = body.kind;
  document.getElementById('network-body-truncated').hidden = !body.truncated;
  document.getElementById('network-detail-raw').textContent = JSON.stringify(entry, null, 2);
}
const consolePanel = document.getElementById('console-panel');
const consoleRows = document.getElementById('console-rows');
const consoleDetail = document.getElementById('console-detail');
const consoleSourceFilter = document.getElementById('console-source-filter');
const consoleLevelFilter = document.getElementById('console-level-filter');
const consoleTextFilter = document.getElementById('console-text-filter');
const consoleResultCount = document.getElementById('console-result-count');
let consoleSort = {key:'time', direction:'ascending'};
function consoleSearchText(entry){
  return String(entry.message || '').toLowerCase();
}
function consoleSortValue(entry, key){
  const value = key === 'time' ? finiteNumber(entry.timestamp)
    : entry[key] ? String(entry[key]) : null;
  return {missing:value == null, value};
}
function compareConsole(left, right){
  const a = consoleSortValue(left.entry, consoleSort.key);
  const b = consoleSortValue(right.entry, consoleSort.key);
  if (a.missing !== b.missing) return a.missing ? 1 : -1;
  if (a.missing) return left.index - right.index;
  let comparison = typeof a.value === 'number' && typeof b.value === 'number'
    ? a.value - b.value : String(a.value).localeCompare(String(b.value));
  if (consoleSort.direction === 'descending') comparison = -comparison;
  return comparison || left.index - right.index;
}
function populateConsoleFilters(){
  const append = (select, values) => values.forEach(value => {
    const option = document.createElement('option');
    option.value = value;
    option.textContent = value;
    select.appendChild(option);
  });
  append(consoleSourceFilter, [...new Set(consoleEvents.map(entry => String(entry.source || 'Unknown')))].sort());
  append(consoleLevelFilter, [...new Set(consoleEvents.map(entry => String(entry.level || 'Unknown')))].sort());
}
function updateConsoleSortHeaders(){
  document.querySelectorAll('[data-console-sort]').forEach(button => {
    const header = button.closest('th');
    if (button.dataset.consoleSort === consoleSort.key) {
      header.setAttribute('aria-sort', consoleSort.direction);
    } else {
      header.removeAttribute('aria-sort');
    }
  });
}
function renderConsole(){
  const range = selectedWindow();
  consoleRows.innerHTML = '';
  consoleDetail.hidden = true;
  const query = consoleTextFilter.value.trim().toLowerCase();
  const visible = consoleEvents.map((entry, index) => ({entry, index})).filter(({entry}) =>
    (finiteNumber(entry.timestamp) == null || inWindow(entry.timestamp, range))
    && (!consoleSourceFilter.value || String(entry.source || 'Unknown') === consoleSourceFilter.value)
    && (!consoleLevelFilter.value || String(entry.level || 'Unknown') === consoleLevelFilter.value)
    && (!query || consoleSearchText(entry).includes(query)))
    .sort(compareConsole);
  consoleResultCount.textContent = `${visible.length} console ${visible.length === 1 ? 'message' : 'messages'}`;
  document.getElementById('console-hint').textContent = !consoleEvents.length
    ? 'No console messages were recorded.'
    : !visible.length ? 'No console messages match the selected range and filters.'
    : 'Use View message details to inspect the structured message.';
  renderChunked(consoleRows, visible, ({entry}) => {
    const tr = document.createElement('tr');
    const timed = finiteNumber(entry.timestamp) != null;
    tr.className = `${consoleFailed(entry) ? 'failed ' : ''}${timed ? 'inwindow' : ''}`.trim();
    tr.innerHTML = `<td class="time-cell">${timed ? esc(offsetLabel(entry.timestamp)) : 'Unknown'}</td><td>${esc(entry.source || 'Unknown')}</td><td>${esc(entry.level || 'Unknown')}</td><td>${esc(entry.message || 'Unknown')}</td><td><button type="button" class="secondary">View message details</button></td>`;
    tr.querySelector('button').addEventListener('click', () => {
      consoleDetail.hidden = false;
      consoleDetail.textContent = JSON.stringify(entry, null, 2);
    });
    return tr;
  }, {columns: 5});
  updateConsoleSortHeaders();
}
const websocketPanel = document.getElementById('websocket-panel');
const websocketRows = document.getElementById('websocket-rows');
const websocketDetail = document.getElementById('websocket-detail');
const websocketDirectionFilter = document.getElementById('websocket-direction-filter');
const websocketTypeFilter = document.getElementById('websocket-type-filter');
const websocketTextFilter = document.getElementById('websocket-text-filter');
function populateWebSocketFilters(){
  const add = (select, values) => values.forEach(value => {
    const option = document.createElement('option');
    option.value = value; option.textContent = value; select.appendChild(option);
  });
  add(websocketDirectionFilter, [...new Set(webSockets.map(entry => String(entry.direction || 'none')))].sort());
  add(websocketTypeFilter, [...new Set(webSockets.map(entry => String(entry.type || 'unknown')))].sort());
}
function renderWebSockets(){
  websocketRows.innerHTML = '';
  websocketDetail.hidden = true;
  const query = websocketTextFilter.value.trim().toLowerCase();
  const visible = webSockets.filter(entry =>
    (!websocketDirectionFilter.value || String(entry.direction || 'none') === websocketDirectionFilter.value)
    && (!websocketTypeFilter.value || String(entry.type || 'unknown') === websocketTypeFilter.value)
    && (!query || [entry.url, entry.text, entry.sha256, entry.reason]
      .filter(Boolean).join(' ').toLowerCase().includes(query)));
  document.getElementById('websocket-result-count').textContent = `${visible.length} WebSocket ${visible.length === 1 ? 'event' : 'events'}`;
  document.getElementById('websocket-hint').textContent = !webSockets.length
    ? 'WebSocket capture is unavailable for this provider or no socket activity was observed.'
    : !visible.length ? 'No WebSocket events match the active filters.'
    : 'Captured lifecycle and frame evidence is bounded and redacted before display.';
  visible.forEach(entry => {
    const tr = document.createElement('tr');
    const payload = entry.text || entry.sha256 || entry.reason || 'None';
    tr.innerHTML = `<td>${esc(entry.type || 'Unknown')}</td><td>${esc(entry.direction || 'None')}</td><td>${esc(entry.url || 'Unavailable')}</td><td>${esc(entry.opcode == null ? 'N/A' : entry.opcode)}</td><td>${esc(payload)}</td><td><button type="button" class="secondary">Inspect event</button></td>`;
    tr.querySelector('button').addEventListener('click', () => {
      websocketDetail.hidden = false;
      websocketDetail.textContent = JSON.stringify(entry, null, 2);
    });
    websocketRows.appendChild(tr);
  });
}
const mobileActions = () => actions.filter(action =>
    String(action.category || '').startsWith('mobile/'));
const mobileRows = document.getElementById('mobile-rows');
const mobileDetail = document.getElementById('mobile-detail');
const mobileResultCount = document.getElementById('mobile-result-count');
let mobileCategory = 'all';
const mobileLabels = {
  'mobile/app':'App', 'mobile/context':'Context', 'mobile/device':'Device',
  'mobile/logs':'Logs', 'mobile/performance':'Performance',
  'mobile/recording':'Recording', 'mobile/evidence':'Evidence'
};
function mobileLabel(action){
  return mobileLabels[String(action.category || '')] || 'Other';
}
function mobileSummary(action){
  const entries = Object.entries(action.metadata || {});
  return entries.length ? entries.map(([key, value]) => `${key}=${value}`).join(', ') : 'No metadata';
}
function renderMobile(){
  mobileRows.innerHTML = '';
  mobileDetail.hidden = true;
  mobileDetail.textContent = '';
  const all = mobileActions();
  const range = selectedWindow();
  const visible = all.filter(action =>
    (actionStartMs(action) == null || actionInWindow(action, range))
    && (mobileCategory === 'all' || action.category === mobileCategory));
  mobileResultCount.textContent = `${visible.length} mobile ${visible.length === 1 ? 'action' : 'actions'}`;
  document.getElementById('mobile-hint').textContent = !all.length
    ? 'No mobile actions were recorded.'
    : !visible.length ? 'No mobile actions match the selected range and category.'
    : 'Inspect captured mobile operations and safe scalar metadata.';
  visible.forEach(action => {
    const tr = document.createElement('tr');
    const timed = actionStartMs(action) != null;
    tr.className = `${statusClass(action.status)}${timed ? ' inwindow' : ''}`;
    tr.innerHTML = `<td class="time-cell">${timed ? esc(offsetLabel(actionStartMs(action))) : 'Unknown'}</td><td>${esc(mobileLabel(action))}</td><td>${esc(action.name || 'Unknown')}</td><td>${esc(action.status || 'Unknown')}</td><td>${esc(mobileSummary(action))}</td><td><button type="button" class="secondary">Inspect action</button></td>`;
    tr.querySelector('button').addEventListener('click', () => {
      selectAction(action, false);
      mobileDetail.hidden = false;
      mobileDetail.textContent = JSON.stringify(action, null, 2);
    });
    mobileRows.appendChild(tr);
  });
  document.querySelectorAll('#mobile-category-filter button').forEach(button => {
    const selectedCategory = button.dataset.mobileCategory === mobileCategory;
    button.classList.toggle('selected', selectedCategory);
    button.setAttribute('aria-pressed', String(selectedCategory));
  });
}
const artifactRows = document.getElementById('artifact-rows');
const artifactResultCount = document.getElementById('artifact-result-count');
const nativeTraceHandoff = document.getElementById('native-trace-handoff');
const artifactKindFilter = document.getElementById('artifact-kind-filter');
let artifactSort = {key:'', direction:'ascending'};
function actionLabel(actionId){
  const action = actions.find(candidate => candidate.id === actionId);
  return action ? `${action.name || 'Action'} (${actionId})` : actionId;
}
function artifactSortValue(group, key){
  const artifact = group.primary;
  if (key === 'name') return readableArtifactName(artifact).toLowerCase();
  if (key === 'kind') return String(artifact.kind || '');
  if (key === 'size') return Number((artifact.metadata || {}).sizeBytes) || 0;
  return 0;
}
function populateArtifactFilters(){
  const kinds = [...new Set(artifacts.map(artifact => String(artifact.kind || 'Unknown')))].sort();
  artifactKindFilter.innerHTML = '<option value="">All kinds</option>';
  kinds.forEach(kind => {
    const option = document.createElement('option');
    option.value = kind;
    option.textContent = kind;
    artifactKindFilter.appendChild(option);
  });
}
async function copyText(text){
  try {
    await navigator.clipboard.writeText(text);
    return true;
  } catch (ignored) {
    return false;
  }
}
function renderArtifacts(){
  artifactRows.innerHTML = '';
  const allGroups = groupArtifacts(artifacts);
  const groups = allGroups.filter(group => !artifactKindFilter.value
      || String(group.primary.kind || 'Unknown') === artifactKindFilter.value);
  if (artifactSort.key) {
    groups.sort((left, right) => {
      const a = artifactSortValue(left, artifactSort.key);
      const b = artifactSortValue(right, artifactSort.key);
      const comparison = typeof a === 'number' ? a - b : String(a).localeCompare(String(b));
      return artifactSort.direction === 'descending' ? -comparison : comparison;
    });
  }
  const references = groups.reduce((total, group) => total + group.artifacts.length, 0);
  artifactResultCount.textContent = `${groups.length} trace ${groups.length === 1 ? 'artifact' : 'artifacts'}`
    + (references !== groups.length ? ` (${references} references)` : '');
  document.getElementById('artifact-hint').textContent = artifacts.length
    ? 'Artifact paths are relative to the downloaded SHAFT trace ZIP. Identical content is listed once.'
    : 'No artifact graph was recorded for this trace.';
  groups.forEach(group => {
    const artifact = group.primary;
    const tr = document.createElement('tr');
    const status = artifact.omitted ? 'Omitted' : 'Available';
    const metadata = artifact.metadata || {};
    const reason = artifact.omitted
      ? metadata.omissionReason || 'No omission reason was recorded.' : '';
    const size = metadata.sizeBytes ? metadata.sizeBytes + ' B' : '';
    const digest = metadata.sha256 ? metadata.sha256.slice(0, 12) : '';
    const usedBy = group.actionIds.map(actionLabel).join(', ');
    tr.innerHTML = `<td><span class="artifact-name" title="${esc(artifact.path || 'Unknown')}">${esc(readableArtifactName(artifact))}</span><button type="button" class="secondary copy-button" aria-label="Copy full path and digest">Copy</button></td><td>${esc(artifact.kind || 'Unknown')}</td><td>${esc(artifact.mimeType || 'Unknown')}</td><td>${status}</td><td>${esc(size)}</td><td>${esc(digest)}</td><td>${esc(usedBy)}</td><td>${esc(reason)}</td>`;
    tr.querySelector('.copy-button').addEventListener('click', async event => {
      const button = event.currentTarget;
      const copied = await copyText(`${artifact.path || ''}${metadata.sha256 ? '\nsha256:' + metadata.sha256 : ''}`);
      button.textContent = copied ? 'Copied' : 'Copy failed';
    });
    artifactRows.appendChild(tr);
  });
  document.querySelectorAll('[data-artifact-sort]').forEach(button => {
    const header = button.closest('th');
    if (button.dataset.artifactSort === artifactSort.key) header.setAttribute('aria-sort', artifactSort.direction);
    else header.removeAttribute('aria-sort');
  });
  const nativeTrace = artifacts.find(artifact => artifact.kind === 'native-trace');
  nativeTraceHandoff.hidden = !nativeTrace;
  nativeTraceHandoff.textContent = !nativeTrace ? '' : nativeTrace.omitted
    ? `Native Playwright trace omitted: ${(nativeTrace.metadata && nativeTrace.metadata.omissionReason) || 'No omission reason was recorded.'}`
    : `Native Playwright trace is available as ${nativeTrace.path} in the downloaded SHAFT trace ZIP. Extract it, then open it with Playwright show-trace ${nativeTrace.path}.`;
}
const sourcePanel = document.getElementById('source-panel');
let sourceLineOverride = null;
function traceSource(){ return trace.source || {}; }
function actionSourceLine(action){
  const source = traceSource();
  const location = callerLocation(action && action.caller);
  if (location && sameSourceFile(location.file, source.file || source.frame)) return location.line;
  return null;
}
function failingSourceLine(){
  const line = parseInt(traceSource().line, 10);
  return Number.isFinite(line) ? line : null;
}
function sourceLines(source){
  if (source.fileContent) return String(source.fileContent).split('\n').map((text, index) => ({number:index + 1, text}));
  return String(source.snippet || '').split(/\r?\n/)
    .map(line => /^[> ] ?\s*(\d+): ?(.*)$/.exec(line))
    .filter(Boolean)
    .map(match => ({number:Number(match[1]), text:match[2]}));
}
function renderSource(){
  const source = traceSource();
  const action = selected || {};
  const failingLine = failingSourceLine();
  const targetLine = sourceLineOverride ?? actionSourceLine(action) ?? failingLine;
  const name = source.file || source.frame || 'Unknown source';
  const lines = sourceLines(source);
  document.getElementById('source-hint').textContent = lines.length
    ? `${name}${targetLine ? `:${targetLine}` : ''}`
    : source.snippet || source.frame ? `The source file was not embedded: ${source.snippet || source.frame}` : 'No source context was captured.';
  const frames = parseStackFrames((action.exception && action.exception.stacktrace)
    || (trace.exception && trace.exception.stacktrace));
  const frameHost = document.getElementById('source-frames');
  frameHost.innerHTML = '';
  frames.slice(0, 30).forEach(frame => {
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'secondary';
    button.textContent = `${frame.method.split('.').slice(-2).join('.')} (${frame.file}${frame.line ? ':' + frame.line : ''})`;
    const navigable = Boolean(frame.line) && sameSourceFile(frame.file, source.file || source.frame) && Boolean(source.fileContent);
    button.disabled = !navigable;
    button.title = navigable ? frame.text : `${frame.text} (source not embedded)`;
    button.addEventListener('click', () => { sourceLineOverride = frame.line; renderSource(); });
    frameHost.appendChild(button);
  });
  const list = document.getElementById('source-lines');
  list.innerHTML = '';
  const fragment = document.createDocumentFragment();
  lines.forEach(({number, text}) => {
    const item = document.createElement('li');
    item.id = `source-line-${number}`;
    item.className = `source-line${number === targetLine ? ' selected' : ''}${number === failingLine ? ' failed' : ''}`;
    if (number === targetLine) item.setAttribute('aria-current', 'true');
    item.innerHTML = `<span class="ln">${number}</span><code>${highlightJava(text)}</code>`;
    fragment.appendChild(item);
  });
  list.appendChild(fragment);
  const current = list.querySelector('[aria-current="true"]');
  if (current) list.scrollTop = Math.max(0, current.offsetTop - list.clientHeight / 2);
}
function renderCall(){
  const action = selected || {};
  const metadata = action.metadata || {};
  const native = nativeActionFor(action);
  const params = (native && native.params) || {};
  const strict = metadata.strict ?? params.strict;
  const html = row('Action', action.name) + row('Category', action.category) + row('Status', action.status)
    + row('Started', action.startTime) + row('Duration', action.durationMs == null ? '' : `${action.durationMs}ms`)
    + row('Locator', action.locator || params.selector) + row('Strict mode', strict == null ? '' : String(strict))
    + row('Key', metadata.key || params.key) + row('Expected', metadata.expected) + row('Actual', metadata.actual)
    + row('URL', action.url) + row('Caller', action.caller) + row('Backend', action.backend)
    + row('Playwright call', native && (native.title || native.method)) + row('Message', action.message);
  document.getElementById('call-details').innerHTML = html;
  document.getElementById('call-empty').hidden = Boolean(html);
}
function actionabilitySteps(action){
  const steps = [];
  Object.entries((action && action.actionability) || {}).forEach(([name, value]) =>
    steps.push(`${name}: ${typeof value === 'object' ? JSON.stringify(value) : value}`));
  const native = nativeActionFor(action);
  ((native && native.logs) || []).forEach(line => steps.push(String(line)));
  return steps;
}
function renderLog(){
  const steps = actionabilitySteps(selected || {});
  document.getElementById('actionability-steps').innerHTML = steps.map(step => `<li>${esc(step)}</li>`).join('');
  document.getElementById('actionability-empty').hidden = steps.length > 0;
  document.getElementById('test-log').textContent = Array.isArray(trace.timeline) && trace.timeline.length
    ? trace.timeline.join('\n') : 'No test log lines were recorded.';
}
function showSourceFor(action, line){
  if (action && action !== selected) selectAction(action, false);
  sourceLineOverride = line;
  renderTab('source');
}
function renderErrors(){
  const entries = errorEntries(actions, trace.exception);
  document.getElementById('errors-hint').textContent = entries.length
    ? `${entries.length} ${entries.length === 1 ? 'error' : 'errors'} recorded.` : 'No errors were recorded.';
  const list = document.getElementById('error-list');
  list.innerHTML = '';
  entries.forEach(entry => {
    const item = document.createElement('li');
    const line = entry.action ? actionSourceLine(entry.action) ?? failingSourceLine() : failingSourceLine();
    item.innerHTML = `<strong>${esc(entry.type)}</strong><div>${esc(entry.message)}</div><div class="error-actions"></div>`;
    const buttons = item.querySelector('.error-actions');
    if (entry.action) {
      const select = document.createElement('button');
      select.type = 'button';
      select.className = 'secondary';
      select.textContent = 'Select action';
      select.addEventListener('click', () => selectAction(entry.action));
      buttons.appendChild(select);
    }
    if (line && sourceLines(traceSource()).some(candidate => candidate.number === line)) {
      const jump = document.createElement('button');
      jump.type = 'button';
      jump.className = 'secondary error-source';
      jump.textContent = `Show source line ${line}`;
      jump.addEventListener('click', () => showSourceFor(entry.action, line));
      buttons.appendChild(jump);
    }
    list.appendChild(item);
  });
}
function renderErrorMarkers(){
  const host = document.getElementById('trace-error-markers');
  host.innerHTML = '';
  if (baseTime == null) return;
  actions.filter(action => statusClass(action.status) === 'failed' && actionStartMs(action) != null).forEach(action => {
    const marker = document.createElement('button');
    marker.type = 'button';
    marker.className = 'error-marker';
    marker.style.left = `${Math.max(0, Math.min(100, (actionStartMs(action) - baseTime) / traceDuration * 100))}%`;
    marker.setAttribute('aria-label', `Failed: ${action.name || 'Action'} at ${offsetLabel(actionStartMs(action))}`);
    marker.title = marker.getAttribute('aria-label');
    marker.addEventListener('click', () => selectAction(action));
    host.appendChild(marker);
  });
}
const domSnapshotPanel = document.getElementById('dom-snapshot-panel');
const domSnapshotFrame = document.getElementById('dom-snapshot-frame');
const snapshotCsp = `<meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; img-src data:; connect-src 'none'; object-src 'none'; base-uri 'none'; form-action 'none'">`;
function snapshotDocument(html){
  const parsed = new DOMParser().parseFromString(
      html || '<p>No DOM snapshot captured for this action.</p>', 'text/html');
  parsed.querySelectorAll('script,style,link,base,meta,iframe,object,embed,video,audio,source,track')
      .forEach(element => element.remove());
  const resourceAttributes = ['src', 'srcset', 'href', 'xlink:href', 'poster', 'background',
    'action', 'formaction', 'ping', 'cite', 'manifest', 'style'];
  parsed.querySelectorAll('*').forEach(element =>
    resourceAttributes.forEach(attribute => element.removeAttribute(attribute)));
  return snapshotCsp + parsed.documentElement.outerHTML;
}
function nativeSnapshot(name){
  const snapshot = name && nativeSnapshots[name];
  return snapshot && snapshot.status === 'available' && snapshot.content
    ? snapshot.content : '';
}
function preferredSnapshot(action, side){
  const native = nativeActionFor(action);
  const nativeName = native && native[side + 'Snapshot'];
  const content = nativeSnapshot(nativeName);
  if (content) return snapshotCsp + content;
  const fallback = side === 'after' ? action.domSnapshotAfter : action.domSnapshotBefore;
  return fallback ? snapshotDocument(fallback) : '';
}
let selectedDomSide = 'before';
function renderDomSnapshot(){
  const action = selected || {};
  const html = domSnapshotFrame && preferredSnapshot(action, selectedDomSide);
  if (domSnapshotFrame) {
    domSnapshotFrame.srcdoc = html || snapshotDocument('');
  }
  document.querySelectorAll('#dom-snapshot-tabs button').forEach(button =>
      button.classList.toggle('selected', button.dataset.dom === selectedDomSide));
}
const screenshotPanel = document.getElementById('screenshot-panel');
const screenshotImage = document.getElementById('screenshot-image');
const screenshotEmpty = document.getElementById('screenshot-empty');
function renderScreenshot(){
  const action = selected || {};
  const hasScreenshot = Boolean(action.screenshot);
  screenshotImage.hidden = !hasScreenshot;
  screenshotEmpty.hidden = hasScreenshot;
  if (hasScreenshot) {
    screenshotImage.src = 'data:image/png;base64,' + action.screenshot;
  }
}
const comparisonPanel = document.getElementById('comparison-panel');
const comparisonBefore = document.getElementById('comparison-before');
const comparisonInput = document.getElementById('comparison-input');
const comparisonAction = document.getElementById('comparison-action');
const comparisonAfter = document.getElementById('comparison-after');
function renderComparison(){
  const action = selected || {};
  const native = nativeActionFor(action);
  const before = preferredSnapshot(action, 'before');
  const input = nativeSnapshot(native && native.inputSnapshot);
  const after = preferredSnapshot(action, 'after');
  const hasBefore = Boolean(before);
  const hasInput = Boolean(input);
  const hasAction = hasInput || Boolean(action.screenshot);
  const hasAfter = Boolean(after);
  comparisonBefore.hidden = !hasBefore;
  document.getElementById('comparison-before-empty').hidden = hasBefore;
  comparisonBefore.srcdoc = before;
  comparisonInput.hidden = !hasInput;
  comparisonInput.srcdoc = hasInput ? snapshotCsp + input : '';
  comparisonAction.hidden = hasInput || !action.screenshot;
  document.getElementById('comparison-action-empty').hidden = hasAction;
  comparisonAction.src = !hasInput && action.screenshot ? 'data:image/png;base64,' + action.screenshot : '';
  comparisonAfter.hidden = !hasAfter;
  document.getElementById('comparison-after-empty').hidden = hasAfter;
  comparisonAfter.srcdoc = after;
}
const nativeEvidencePanel = document.getElementById('native-evidence-panel');
const nativeEvidenceRows = document.getElementById('native-evidence-rows');
function renderNativeEvidence(){
  nativeEvidenceRows.innerHTML = '';
  document.getElementById('native-evidence-hint').textContent = playwright.status === 'available'
    ? `${nativeActions.length} native Playwright ${nativeActions.length === 1 ? 'action' : 'actions'} retained offline.`
    : `Native Playwright evidence ${playwright.status || 'unavailable'}${playwright.reason ? ': ' + playwright.reason : '.'}`;
  const selectedNative = nativeActionFor(selected || {});
  nativeActions.forEach(native => {
    const tr = document.createElement('tr');
    const correlation = selectedNative && selectedNative.callId === native.callId
      ? 'Selected SHAFT action' : (playwright.correlations || []).some(item => item.playwrightCallId === native.callId)
        ? 'Correlated' : 'Native only';
    tr.innerHTML = `<td>${esc(correlation)}</td><td>${esc(native.title || native.method || native.callId || 'Unknown')}</td><td>${esc(native.source || native.sourceReason || 'Unavailable')}</td><td>${esc((native.logs || []).join('\n') || 'None')}</td><td>${esc(native.error || 'None')}<br><button type="button" class="secondary">Inspect native action</button></td>`;
    tr.querySelector('button').addEventListener('click', () => {
      selectedNativeAction = native;
      document.getElementById('details-title').textContent = `Native action: ${native.title || native.method || native.callId}`;
      details.innerHTML = row('Provider', 'Playwright') + row('Call ID', native.callId)
        + row('Source', native.source || native.sourceReason) + row('Logs', (native.logs || []).join('\n'))
        + row('Error', native.error);
      renderTab('comparison');
    });
    nativeEvidenceRows.appendChild(tr);
  });
}
function renderTab(tab){
  const action = selected || {};
  const panels = {timeline: timelinePanel, nativeEvidence: nativeEvidencePanel, comparison: comparisonPanel, domSnapshot: domSnapshotPanel, screenshot: screenshotPanel, network: networkPanel, console: consolePanel, webSockets: websocketPanel, mobile: document.getElementById('mobile-panel'), artifacts: document.getElementById('artifact-panel'), source: sourcePanel, call: document.getElementById('call-panel'), log: document.getElementById('log-panel'), errors: document.getElementById('errors-panel')};
  tabContent.hidden = tab in panels;
  Object.entries(panels).forEach(([name, panel]) => panel.hidden = name !== tab);
  if (tab === 'timeline') {
    renderTimeline();
  } else if (tab === 'nativeEvidence') {
    renderNativeEvidence();
  } else if (tab === 'comparison') {
    renderComparison();
  } else if (tab === 'domSnapshot') {
    renderDomSnapshot();
  } else if (tab === 'screenshot') {
    renderScreenshot();
  } else if (tab === 'network') {
    renderNetwork();
  } else if (tab === 'console') {
    renderConsole();
  } else if (tab === 'webSockets') {
    renderWebSockets();
  } else if (tab === 'mobile') {
    renderMobile();
  } else if (tab === 'artifacts') {
    renderArtifacts();
  } else if (tab === 'source') {
    renderSource();
  } else if (tab === 'call') {
    renderCall();
  } else if (tab === 'log') {
    renderLog();
  } else if (tab === 'errors') {
    renderErrors();
  } else {
    const data = tab === 'json' ? trace
        : tab === 'exception' && action.exception && (action.exception.type || action.exception.message) ? action.exception
        : tab === 'browserObservability' ? evidence.browserObservability
        : trace[tab];
    tabContent.textContent = typeof data === 'string' ? data : JSON.stringify(data || {}, null, 2);
  }
  document.querySelectorAll('#action-tabs button').forEach(button => button.classList.toggle('selected', button.dataset.tab === tab));
}
async function copyJson(){
  await navigator.clipboard.writeText(JSON.stringify(trace, null, 2));
}
actionSearch.addEventListener('input', renderActions);
rangeStart.addEventListener('input', () => applyRangeInputs('none'));
rangeEnd.addEventListener('input', () => applyRangeInputs('none'));
rangeStart.addEventListener('change', () => applyRangeInputs('push'));
rangeEnd.addEventListener('change', () => applyRangeInputs('push'));
document.getElementById('show-all-range').addEventListener('click', () => {
  rangeStartMs = baseTime;
  rangeEndMs = traceEnd;
  updateHash('push');
  renderNavigator();
  renderActions();
  renderDetails();
});
filmstrip.addEventListener('keydown', event => {
  if (event.key !== 'ArrowLeft' && event.key !== 'ArrowRight') return;
  const options = [...filmstrip.querySelectorAll('button[role="option"]')];
  const current = Math.max(0, options.indexOf(document.activeElement));
  const next = event.key === 'ArrowRight'
      ? Math.min(options.length - 1, current + 1)
      : Math.max(0, current - 1);
  if (options[next]) {
    event.preventDefault();
    const nextActionId = options[next].dataset.actionId;
    options[next].click();
    const renderedOption = [...filmstrip.querySelectorAll('button[role="option"]')]
        .find(option => option.dataset.actionId === nextActionId);
    if (renderedOption) renderedOption.focus();
  }
});
function restoreLocationState(){
  const state = hashState();
  const action = state.actionId ? actions.find(candidate => candidate.id === state.actionId) : null;
  if (!action) return;
  const hasRange = state.params.has('start') && state.params.has('end');
  const startOffset = hasRange ? Number(state.params.get('start')) : NaN;
  const endOffset = hasRange ? Number(state.params.get('end')) : NaN;
  if (baseTime != null && Number.isFinite(startOffset) && Number.isFinite(endOffset)) {
    rangeStartMs = baseTime + Math.max(0, Math.min(traceDuration, Math.min(startOffset, endOffset)));
    rangeEndMs = baseTime + Math.max(0, Math.min(traceDuration, Math.max(startOffset, endOffset)));
  } else {
    const start = actionStartMs(action);
    if (start != null) {
      rangeStartMs = start;
      rangeEndMs = Math.max(start, start + Math.max(0, action.durationMs || 0));
    }
  }
  selectAction(action, false, 'none');
}
window.addEventListener('popstate', restoreLocationState);
window.addEventListener('hashchange', restoreLocationState);
document.querySelectorAll('#timeline-filters button').forEach(button => button.addEventListener('click', () => {
  timelineFilter = button.dataset.filter;
  document.querySelectorAll('#timeline-filters button').forEach(other => other.classList.toggle('selected', other === button));
  renderTimeline();
}));
[networkMethodFilter, networkStatusFilter, networkTextFilter]
    .forEach(control => control.addEventListener('input', renderNetwork));
document.querySelectorAll('[data-network-sort]').forEach(button => button.addEventListener('click', () => {
  const key = button.dataset.networkSort;
  networkSort = networkSort.key === key
    ? {key, direction:networkSort.direction === 'ascending' ? 'descending' : 'ascending'}
    : {key, direction:'ascending'};
  renderNetwork();
}));
[consoleSourceFilter, consoleLevelFilter, consoleTextFilter]
    .forEach(control => control.addEventListener('input', renderConsole));
[websocketDirectionFilter, websocketTypeFilter, websocketTextFilter]
    .forEach(control => control.addEventListener('input', renderWebSockets));
document.querySelectorAll('[data-console-sort]').forEach(button => button.addEventListener('click', () => {
  const key = button.dataset.consoleSort;
  consoleSort = consoleSort.key === key
    ? {key, direction:consoleSort.direction === 'ascending' ? 'descending' : 'ascending'}
    : {key, direction:'ascending'};
  renderConsole();
}));
document.querySelectorAll('#mobile-category-filter button').forEach(button => button.addEventListener('click', () => {
  mobileCategory = button.dataset.mobileCategory;
  renderMobile();
}));
document.querySelectorAll('#action-tabs button').forEach(button => button.addEventListener('click', () => renderTab(button.dataset.tab)));
document.querySelectorAll('#dom-snapshot-tabs button').forEach(button => button.addEventListener('click', () => { selectedDomSide = button.dataset.dom; renderDomSnapshot(); }));
populateNetworkFilters();
populateConsoleFilters();
populateWebSocketFilters();
populateArtifactFilters();
artifactKindFilter.addEventListener('input', renderArtifacts);
document.querySelectorAll('[data-artifact-sort]').forEach(button => button.addEventListener('click', () => {
  const key = button.dataset.artifactSort;
  artifactSort = artifactSort.key === key
    ? {key, direction:artifactSort.direction === 'ascending' ? 'descending' : 'ascending'}
    : {key, direction:'ascending'};
  renderArtifacts();
}));
document.getElementById('network-detail-close').addEventListener('click', () => { networkDetail.hidden = true; });
renderSummary();
renderNavigator();
renderActions();
renderDetails();
window.shaftTraceReadyMs = performance.now();
window.shaftTraceReady = true;
