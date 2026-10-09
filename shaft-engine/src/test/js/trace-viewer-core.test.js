'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const path = require('node:path');
const core = require(path.join(__dirname, '..', '..', 'main', 'resources', 'META-INF', 'shaft', 'trace-viewer', 'viewer-core.js'));

test('esc neutralizes markup', () => {
  assert.equal(core.esc('<img src=x onerror="a">'), '&lt;img src=x onerror=&quot;a&quot;&gt;');
  assert.equal(core.esc(null), '');
});

test('contentTypeOf reads the response header case-insensitively and drops parameters', () => {
  assert.equal(core.contentTypeOf({responseHeaders: {'Content-Type': 'application/json; charset=utf-8'}}), 'application/json');
  assert.equal(core.contentTypeOf({}), '');
});

test('formatBody pretty-prints JSON, previews images and marks truncation', () => {
  const json = core.formatBody('{"a":[1,2]}', 'application/json');
  assert.equal(json.kind, 'json');
  assert.equal(json.text, '{\n  "a": [\n    1,\n    2\n  ]\n}');
  assert.equal(core.formatBody('iVBORw0KGgo=', 'image/png').kind, 'image');
  assert.equal(core.formatBody('', 'text/plain').kind, 'empty');
  const long = core.formatBody('x'.repeat(core.BODY_PREVIEW_LIMIT), 'text/plain');
  assert.equal(long.kind, 'text');
  assert.equal(long.truncated, true);
  assert.equal(core.formatBody('{"unterminated":', 'application/json').kind, 'text');
});

test('parseStackFrames extracts method, file and line', () => {
  const frames = core.parseStackFrames('java.lang.AssertionError: boom\n\tat com.acme.CheckoutTest.pay(CheckoutTest.java:42)\n\tat jdk.internal.Native.invoke(Native Method)');
  assert.deepEqual(frames.map(frame => [frame.method, frame.file, frame.line]),
    [['com.acme.CheckoutTest.pay', 'CheckoutTest.java', 42], ['jdk.internal.Native.invoke', 'Native Method', null]]);
});

test('callerLocation and sameSourceFile match by file name', () => {
  assert.deepEqual(core.callerLocation('com.acme.CheckoutTest.pay(CheckoutTest.java:42)'), {file: 'CheckoutTest.java', line: 42});
  assert.equal(core.callerLocation('unknown'), null);
  assert.equal(core.sameSourceFile('src/test/java/com/acme/CheckoutTest.java', 'CheckoutTest.java'), true);
  assert.equal(core.sameSourceFile('A.java', 'B.java'), false);
});

test('highlightJava wraps tokens and escapes markup', () => {
  const html = core.highlightJava('public String s = "<b>"; // note');
  assert.match(html, /<span class="tok-kw">public<\/span>/);
  assert.match(html, /<span class="tok-str">&quot;&lt;b&gt;&quot;<\/span>/);
  assert.match(html, /<span class="tok-com">\/\/ note<\/span>/);
  assert.doesNotMatch(html, /<b>/);
});

test('groupArtifacts collapses identical digests and records every action', () => {
  const digest = 'a'.repeat(64);
  const groups = core.groupArtifacts([
    {id: 'screenshot-action-1', kind: 'screenshot', path: `resources/${digest}.png`, metadata: {sha256: digest}},
    {id: 'screenshot-action-2', kind: 'screenshot', path: `resources/${digest}.png`, metadata: {sha256: digest}},
    {id: 'dom-1', kind: 'dom-snapshot', path: 'resources/b.html', metadata: {sha256: 'b', actionId: 'action-1'}},
    {id: 'gone', kind: 'screenshot', path: 'resources/c.png', omitted: true, metadata: {sha256: digest}},
  ]);
  assert.equal(groups.length, 3);
  assert.deepEqual(groups[0].actionIds, ['action-1', 'action-2']);
  assert.equal(groups[0].artifacts.length, 2);
});

test('readableArtifactName shortens content-addressed paths only', () => {
  assert.equal(core.readableArtifactName({kind: 'screenshot', path: `resources/${'0123abcd'.repeat(8)}.png`}), 'screenshot 0123abcd.png');
  assert.equal(core.readableArtifactName({kind: 'network', path: 'shaft-network.har'}), 'shaft-network.har');
});

test('errorEntries lists failed actions and an unmatched test exception', () => {
  const entries = core.errorEntries([
    {id: 'a', status: 'passed'},
    {id: 'b', status: 'failed', message: 'mismatch', exception: {message: 'expected receipt'}},
  ], {type: 'java.lang.AssertionError', message: 'checkout failed'});
  assert.equal(entries.length, 2);
  assert.equal(entries[0].message, 'mismatch: expected receipt');
  assert.equal(entries[1].action, null);
});

test('parseSeleniumLocator reads By strategies and ignores other formats', () => {
  assert.deepEqual(core.parseSeleniumLocator('By.id: pay'), {strategy: 'id', value: 'pay'});
  assert.deepEqual(core.parseSeleniumLocator('By.cssSelector: main > button.pay'), {strategy: 'cssSelector', value: 'main > button.pay'});
  assert.deepEqual(core.parseSeleniumLocator('By.xpath: //a[text()="x"]'), {strategy: 'xpath', value: '//a[text()="x"]'});
  assert.equal(core.parseSeleniumLocator('<legacy>'), null);
});

test('locatorCandidates follow locator-health order and escape Java and XPath text', () => {
  const kinds = core.locatorCandidates({tag: 'BUTTON', testId: 'pay', id: 'pay-button', name: 'pay', text: 'Pay now', cssPath: 'main > button'})
    .map(candidate => candidate.kind);
  assert.deepEqual(kinds, ['data-testid', 'id', 'name', 'text', 'css-path']);
  const generated = core.locatorCandidates({tag: 'div', id: 'react-select-12345', text: 'He said "hi" it\'s'});
  assert.deepEqual(generated.map(candidate => candidate.kind), ['text']);
  assert.equal(generated[0].java, 'SHAFT.GUI.Locator.hasTagName("div").hasText("He said \\"hi\\" it\'s").build()');
  assert.equal(generated[0].xpath, '//div[normalize-space(.)=concat(\'He said "hi" it\', "\'", \'s\')]');
  assert.equal(core.locatorCandidates({tag: 'input', testId: 'a"b'})[0].java, 'By.cssSelector("[data-testid=\\"a\\\\\\"b\\"]")');
});

test('logLineTime reads ISO timestamps only', () => {
  assert.equal(core.logLineTime('2026-10-09T08:00:00.250Z [main] click'), Date.parse('2026-10-09T08:00:00.250Z'));
  assert.equal(core.logLineTime('10:15:00 no date'), null);
});

test('filmstripActions keeps captured frames unless all actions are requested', () => {
  const list = [{id: 'a', screenshot: 'x'}, {id: 'b'}, {id: 'c', screenshot: 'y'}];
  assert.deepEqual(core.filmstripActions(list, false).map(action => action.id), ['a', 'c']);
  assert.equal(core.filmstripActions(list, true).length, 3);
});

test('themeFromBackground reads a host-forced background and ignores a transparent root', () => {
  assert.equal(core.themeFromBackground('rgb(28, 28, 30)'), 'dark');
  assert.equal(core.themeFromBackground('rgb(247, 249, 251)'), 'light');
  assert.equal(core.themeFromBackground('rgba(0, 0, 0, 0)'), null);
  assert.equal(core.themeFromBackground(''), null);
});

test('resolveTheme prefers a manual choice, then the query, then the host, then the browser scheme', () => {
  assert.equal(core.resolveTheme('dark', 'light', 'light'), 'dark');
  assert.equal(core.resolveTheme(null, 'dark', 'light'), 'dark');
  assert.equal(core.resolveTheme(null, 'bogus', 'dark'), 'dark');
  assert.equal(core.resolveTheme(null, null, null), null); // #6752: Allure 2 paints nothing and follows the OS scheme.
});

test('clampPaneWidth keeps the actions pane between its bounds and leaves room for details', () => {
  assert.equal(core.clampPaneWidth(50, 1440), core.PANE_MIN);
  assert.equal(core.clampPaneWidth(5000, 1440), core.PANE_MAX);
  assert.equal(core.clampPaneWidth(500, 760), 400);
  assert.equal(core.clampPaneWidth('x', 1440), 320);
});

test('navigationIndex moves within bounds and ignores other keys', () => {
  assert.equal(core.navigationIndex('ArrowRight', 0, 3), 1);
  assert.equal(core.navigationIndex('j', 2, 3), 2);
  assert.equal(core.navigationIndex('k', 0, 3), 0);
  assert.equal(core.navigationIndex('End', 0, 3), 2);
  assert.equal(core.navigationIndex('Home', 2, 3), 0);
  assert.equal(core.navigationIndex('ArrowDown', -1, 3), 0);
  assert.equal(core.navigationIndex('x', 0, 3), -1);
  assert.equal(core.navigationIndex('j', 0, 0), -1);
});
