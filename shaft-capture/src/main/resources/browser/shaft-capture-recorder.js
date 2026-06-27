(channel) => {
  if (globalThis.__shaftCaptureInstalled) {
    return;
  }
  globalThis.__shaftCaptureInstalled = true;
  globalThis.__shaftCaptureQueue = globalThis.__shaftCaptureQueue || [];

  const testIdAttributes = ["data-testid", "data-test", "data-qa"];
  const STORAGE_KEY = "shaft.capture.recorder.ui";
  const text = value => String(value || "").replace(/\s+/g, " ").trim().slice(0, 500);
  const valueText = value => String(value == null ? "" : value).slice(0, 1000);
  const dynamic = value =>
    /[0-9]{8,}/.test(value || "") ||
    /[a-f0-9]{8}-[a-f0-9-]{27,}/i.test(value || "");
  const cssEscape = value => {
    if (globalThis.CSS && typeof globalThis.CSS.escape === "function") {
      return globalThis.CSS.escape(value);
    }
    return String(value).replace(/[^a-zA-Z0-9_-]/g, "\\$&");
  };
  const persisted = () => {
    try {
      return JSON.parse(sessionStorage.getItem(STORAGE_KEY) || "{}");
    } catch (ignored) {
      return {};
    }
  };
  const uiState = {
    paused: false,
    stopped: false,
    assertionMode: false,
    locatorMode: false,
    locatorPreferences: {},
    actions: [],
    nextId: 1,
    lastUrl: String(location.href || ""),
    ...persisted()
  };
  uiState.actions = Array.isArray(uiState.actions) ? uiState.actions.slice(-80) : [];
  uiState.nextId = Number(uiState.nextId || uiState.actions.length + 1);
  uiState.assertionMode = Boolean(uiState.assertionMode);
  uiState.locatorMode = Boolean(uiState.locatorMode);
  uiState.locatorPreferences = uiState.locatorPreferences && typeof uiState.locatorPreferences === "object"
    ? uiState.locatorPreferences
    : {};
  globalThis.__shaftCaptureUiState = uiState;
  const topLevel = (() => {
    try {
      return globalThis.top === globalThis;
    } catch (ignored) {
      return false;
    }
  })();
  const persist = () => {
    try {
      sessionStorage.setItem(STORAGE_KEY, JSON.stringify({
        paused: uiState.paused,
        stopped: uiState.stopped,
        assertionMode: uiState.assertionMode,
        locatorMode: uiState.locatorMode,
        locatorPreferences: uiState.locatorPreferences,
        actions: uiState.actions.slice(-80),
        nextId: uiState.nextId,
        lastUrl: uiState.lastUrl
      }));
    } catch (ignored) {
      // Storage can be unavailable in sandboxed frames.
    }
  };
  const send = payload => {
    payload.timestamp = Date.now();
    globalThis.__shaftCaptureQueue.push(payload);
    if (typeof channel === "function") {
      channel(JSON.stringify(payload));
    }
  };
  const framePath = () => {
    const path = [];
    let current = globalThis;
    try {
      while (current !== current.top) {
        const frame = current.frameElement;
        path.unshift(text(frame && (frame.id || frame.name)) || "frame");
        current = current.parent;
      }
    } catch (ignored) {
      path.unshift("cross-origin-frame");
    }
    return path;
  };
  const page = () => ({
    url: String(location.href || ""),
    title: text(document.title),
    framePath: framePath(),
    width: Number(globalThis.innerWidth || 0),
    height: Number(globalThis.innerHeight || 0)
  });
  const visibleLocation = () => text(String(location.href || "").split(/[?#]/)[0]);
  const sendControl = (action, data) =>
    send({kind: "control", page: page(), data: {action, ...(data || {})}});
  const sendCheckpoint = (description, kind) =>
    send({
      kind: "checkpoint",
      page: page(),
      data: {
        description: text(description) || "Captured browser checkpoint",
        kind: kind || "USER_MARKER"
      }
    });
  const sendVerification = (target, data) =>
    send({kind: "verification", page: page(), target, data});
  const inferredRole = element => {
    const explicit = text(element.getAttribute("role")).toLowerCase();
    if (explicit) return explicit;
    const tag = String(element.localName || "").toLowerCase();
    const type = String(element.type || "").toLowerCase();
    if (tag === "button" || type === "button" || type === "submit") return "button";
    if (tag === "a" && element.hasAttribute("href")) return "link";
    if (tag === "select") return "combobox";
    if (type === "checkbox") return "checkbox";
    if (type === "radio") return "radio";
    if (tag === "textarea" || tag === "input") return "textbox";
    return "";
  };
  const label = element => {
    if (element.labels && element.labels.length) return text(element.labels[0].innerText);
    const parent = element.closest && element.closest("label");
    return text(parent && parent.innerText);
  };
  const accessibleName = element =>
    text(element.getAttribute("aria-label")) ||
    label(element) ||
    text(element.getAttribute("alt")) ||
    text(element.getAttribute("title")) ||
    text(element.innerText);
  const targetName = target =>
    text(target && (
      target.accessibleName ||
      target.label ||
      (target.attributes && target.attributes.placeholder) ||
      target.logicalElementId ||
      target.tagName
    )) || "element";
  const icon = name => ({
    pause: `<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M8 5v14"></path><path d="M16 5v14"></path></svg>`,
    play: `<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M8 5v14l11-7z"></path></svg>`,
    assert: `<svg viewBox="0 0 24 24" aria-hidden="true"><circle cx="12" cy="12" r="9"></circle><path d="m8.5 12.5 2.5 2.5 4.5-6"></path></svg>`,
    locator: `<svg viewBox="0 0 24 24" aria-hidden="true"><circle cx="12" cy="12" r="3"></circle><path d="M12 2v4"></path><path d="M12 18v4"></path><path d="M2 12h4"></path><path d="M18 12h4"></path></svg>`,
    checkpoint: `<svg viewBox="0 0 24 24" aria-hidden="true"><circle cx="12" cy="12" r="9"></circle><path d="M12 8v8"></path><path d="M8 12h8"></path></svg>`,
    stop: `<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M7 7h10v10H7z"></path></svg>`,
    edit: `<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M12 20h9"></path><path d="M16.5 3.5a2.1 2.1 0 0 1 3 3L7 19l-4 1 1-4Z"></path></svg>`,
    pin: `<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M12 17v5"></path><path d="M5 17h14"></path><path d="m8 4 8 8"></path><path d="M7 5l5-3 5 5-3 5Z"></path></svg>`
  })[name] || "";
  const describe = (kind, target, data) => {
    const name = targetName(target);
    switch (kind) {
      case "click":
        return (Number(data.clickCount || 1) > 1 ? "Double-click " : "Click ") + name;
      case "input":
        return "Type into " + name;
      case "select":
        return "Select option in " + name;
      case "toggle":
        return "Toggle " + name + (data.checked ? " on" : " off");
      case "upload":
        return "Upload file to " + name;
      case "keyboard":
        return "Press " + (Array.isArray(data.keys) ? data.keys.join("+") : "key") + " on " + name;
      default:
        return "Capture " + kind + " on " + name;
    }
  };
  const announce = value => {
    const description = text(value);
    if (!description) return;
    uiState.actions.push({id: uiState.nextId++, text: description, timestamp: Date.now()});
    uiState.actions = uiState.actions.slice(-80);
    persist();
    renderActions();
  };
  const count = selector => {
    try {
      return document.querySelectorAll(selector).length;
    } catch (ignored) {
      return 0;
    }
  };
  const isControlElement = element =>
    Boolean(element && element.closest && element.closest("[data-shaft-capture-control]"));
  const cssPath = element => {
    const parts = [];
    let current = element;
    while (current && current.nodeType === Node.ELEMENT_NODE && parts.length < 6) {
      if (current.id) {
        parts.unshift("#" + cssEscape(current.id));
        break;
      }
      let part = String(current.localName || "*").toLowerCase();
      const siblings = current.parentElement
        ? Array.from(current.parentElement.children).filter(item => item.localName === current.localName)
        : [];
      if (siblings.length > 1) {
        part += `:nth-of-type(${siblings.indexOf(current) + 1})`;
      }
      parts.unshift(part);
      const root = current.getRootNode && current.getRootNode();
      current = root && root.host ? root.host : current.parentElement;
    }
    return parts.join(" > ");
  };
  const locators = element => {
    const result = [];
    const visible = Boolean(element.getClientRects && element.getClientRects().length);
    const add = (strategy, expression, selector, stable, signals) => {
      if (!expression) return;
      result.push({
        strategy,
        expression: text(expression),
        uniquenessCount: selector ? count(selector) : 1,
        visible,
        stable,
        signals
      });
    };
    const role = inferredRole(element);
    const name = accessibleName(element);
    if (role && name) add("ROLE", `${role}:${name}`, "", true, ["ACCESSIBLE"]);
    const targetLabel = label(element);
    if (targetLabel) add("LABEL", targetLabel, "", true, ["ACCESSIBLE", "LABEL_ASSOCIATED"]);
    testIdAttributes.forEach(attribute => {
      const value = element.getAttribute(attribute);
      if (value) {
        const selector = `[${attribute}="${cssEscape(value)}"]`;
        add("TEST_ID", selector, selector, !dynamic(value), ["TEST_ATTRIBUTE", "STABLE_ATTRIBUTE"]);
      }
    });
    if (element.id) {
      const selector = `#${cssEscape(element.id)}`;
      add("ID", element.id, selector, !dynamic(element.id), ["STABLE_ATTRIBUTE"]);
    }
    if (element.name) {
      const selector = `[name="${cssEscape(element.name)}"]`;
      add("NAME", element.name, selector, !dynamic(element.name), ["STABLE_ATTRIBUTE"]);
    }
    const generated = cssPath(element);
    add("CSS", generated, generated, false, ["GENERATED", "POSITIONAL"]);
    return result;
  };
  const locatorStrategyScores = {
    ROLE: 100,
    ACCESSIBLE_NAME: 95,
    LABEL: 90,
    TEST_ID: 85,
    ID: 80,
    NAME: 70,
    CSS: 50,
    XPATH: 25
  };
  const locatorSignalWeights = {
    USER_PROVIDED: 500,
    ACCESSIBLE: 15,
    LABEL_ASSOCIATED: 12,
    TEST_ATTRIBUTE: 10,
    STABLE_ATTRIBUTE: 8,
    GENERATED: -5,
    POSITIONAL: -15,
    DYNAMIC_VALUE: -25
  };
  const signed = value => value > 0 ? "+" + value : String(value);
  const locatorScore = candidate => {
    const uniqueness = candidate.uniquenessCount === 1
      ? 30
      : candidate.uniquenessCount === 0 ? -20 : -10;
    return (locatorStrategyScores[candidate.strategy] || 0)
      + uniqueness
      + (candidate.visible ? 10 : -10)
      + (candidate.stable ? 10 : -10)
      + (candidate.signals || []).reduce((total, signal) =>
        total + (locatorSignalWeights[signal] || 0), 0);
  };
  const locatorRationale = candidate => [
    "strategy " + signed(locatorStrategyScores[candidate.strategy] || 0),
    "matches " + signed(candidate.uniquenessCount === 1 ? 30 : candidate.uniquenessCount === 0 ? -20 : -10),
    candidate.visible ? "visible +10" : "hidden -10",
    candidate.stable ? "stable +10" : "volatile -10",
    "signals " + signed((candidate.signals || []).reduce((total, signal) =>
      total + (locatorSignalWeights[signal] || 0), 0))
  ].join(" | ");
  const allPageElements = () =>
    Array.from(document.querySelectorAll("body *")).filter(element => !isControlElement(element));
  const queryAll = selector => {
    try {
      return Array.from(document.querySelectorAll(selector));
    } catch (ignored) {
      return null;
    }
  };
  const xpathAll = expression => {
    try {
      const result = document.evaluate(expression, document, null,
        XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
      return Array.from({length: result.snapshotLength}, (_, index) => result.snapshotItem(index))
        .filter(element => element && element.nodeType === Node.ELEMENT_NODE);
    } catch (ignored) {
      return null;
    }
  };
  const locatorProbe = candidate => {
    const expression = candidate.expression;
    let matches = [];
    if (candidate.strategy === "CSS" || candidate.strategy === "TEST_ID") {
      matches = queryAll(expression);
    } else if (candidate.strategy === "ID") {
      matches = queryAll("#" + cssEscape(expression));
    } else if (candidate.strategy === "NAME") {
      matches = queryAll(`[name="${cssEscape(expression)}"]`);
    } else if (candidate.strategy === "ROLE") {
      const separator = expression.indexOf(":");
      const role = separator < 0 ? expression : expression.slice(0, separator);
      const name = separator < 0 ? "" : expression.slice(separator + 1);
      matches = allPageElements().filter(element =>
        inferredRole(element) === role && (!name || accessibleName(element) === name));
    } else if (candidate.strategy === "LABEL") {
      matches = allPageElements().filter(element => label(element) === expression);
    } else if (candidate.strategy === "ACCESSIBLE_NAME") {
      matches = allPageElements().filter(element => accessibleName(element) === expression);
    } else if (candidate.strategy === "XPATH") {
      matches = xpathAll(expression);
    }
    if (matches == null) return {status: "failed", count: 0};
    const matchingElements = matches.filter(element => !isControlElement(element));
    if (matchingElements.length === 1) return {status: "unique", count: 1};
    if (matchingElements.length === 0) return {status: "no match", count: 0};
    return {status: "multi-match", count: matchingElements.length};
  };
  const isPreferredLocator = (logicalElementId, candidate) => {
    const preference = uiState.locatorPreferences[logicalElementId];
    return Boolean(preference
      && preference.strategy === candidate.strategy
      && preference.expression === candidate.expression);
  };
  const withUserProvidedSignal = candidate => ({
    ...candidate,
    signals: Array.from(new Set([...(candidate.signals || []), "USER_PROVIDED"]))
  });
  const applyLocatorPreference = (logicalElementId, candidates) =>
    candidates.map(candidate => isPreferredLocator(logicalElementId, candidate)
      ? withUserProvidedSignal(candidate)
      : candidate);
  const withLocatorPreference = (target, candidate) => ({
    ...target,
    locators: (target.locators || []).map(item =>
      item.strategy === candidate.strategy && item.expression === candidate.expression
        ? withUserProvidedSignal(item)
        : item)
  });
  const eventElement = event => {
    const path = event.composedPath ? event.composedPath() : [];
    return path.find(item => item && item.nodeType === Node.ELEMENT_NODE) || event.target;
  };
  const snapshot = event => {
    const element = eventElement(event);
    if (!element || element.nodeType !== Node.ELEMENT_NODE) return null;
    if (isControlElement(element)) return null;
    const attributes = {};
    ["id", "name", "type", "placeholder", "autocomplete", "data-testid",
      "data-test", "data-qa", "aria-label", "role"].forEach(name => {
      if (element.hasAttribute && element.hasAttribute(name)) {
        attributes[name] = text(element.getAttribute(name));
      }
    });
    const logical = text(
      element.getAttribute && (
        element.getAttribute("data-testid") ||
        element.getAttribute("data-test") ||
        element.id ||
        element.name
      )
    ) || `${String(element.localName || "element")}-${Math.abs(cssPath(element).split("")
      .reduce((hash, character) => ((hash << 5) - hash) + character.charCodeAt(0), 0))}`;
    const locatorCandidates = applyLocatorPreference(logical, locators(element));
    return {
      logicalElementId: logical,
      tagName: String(element.localName || "").toLowerCase(),
      role: inferredRole(element),
      accessibleName: accessibleName(element),
      label: label(element),
      attributes,
      locators: locatorCandidates,
      visible: Boolean(element.getClientRects && element.getClientRects().length),
      enabled: !Boolean(element.disabled),
      selected: Boolean(element.checked || element.selected)
    };
  };
  const emit = (kind, event, data) => {
    if (uiState.paused || uiState.stopped || uiState.locatorMode) return;
    const target = snapshot(event);
    if (target) {
      const action = data || {};
      announce(describe(kind, target, action));
      send({kind, page: page(), target, data: action});
    }
  };
  const isTextInput = element => {
    const tag = String(element && element.localName || "").toLowerCase();
    const type = String(element && element.type || "text").toLowerCase();
    return tag === "textarea" || (tag === "input" &&
      !["button", "submit", "reset", "checkbox", "radio", "file", "hidden"].includes(type));
  };
  const assertionKind = value => {
    const normalized = text(value).toLowerCase().replace(/[_-]+/g, " ");
    switch (normalized) {
      case "visible":
      case "element visible":
        return "ELEMENT_VISIBLE";
      case "enabled":
      case "element enabled":
        return "ELEMENT_ENABLED";
      case "selected":
      case "element selected":
        return "ELEMENT_SELECTED";
      case "text equals":
      case "text equal":
      case "value equals":
      case "value equal":
        return "TEXT_EQUALS";
      case "text contains":
      case "value contains":
        return "TEXT_CONTAINS";
      case "attribute equals":
      case "attribute equal":
        return "ATTRIBUTE_EQUALS";
      case "url equals":
      case "url equal":
        return "URL_EQUALS";
      case "url contains":
        return "URL_CONTAINS";
      case "title equals":
      case "title equal":
        return "TITLE_EQUALS";
      default:
        return "";
    }
  };
  const assertionLabel = kind => kind.toLowerCase().replace(/_/g, " ");
  const expectsValue = kind =>
    ["TEXT_EQUALS", "TEXT_CONTAINS", "ATTRIBUTE_EQUALS", "URL_EQUALS", "URL_CONTAINS", "TITLE_EQUALS"]
      .includes(kind);
  const pageAssertion = kind => ["URL_EQUALS", "URL_CONTAINS", "TITLE_EQUALS"].includes(kind);
  const defaultAttribute = element =>
    ["value", "aria-label", "title", "href", "id", "name"]
      .find(name => element && element.hasAttribute && element.hasAttribute(name)) || "value";
  const currentValue = (kind, element, attributeName) => {
    if (kind === "URL_EQUALS" || kind === "URL_CONTAINS") return valueText(location.href);
    if (kind === "TITLE_EQUALS") return valueText(document.title);
    if (kind === "ATTRIBUTE_EQUALS") return valueText(element && element.getAttribute(attributeName));
    if (isTextInput(element)) return valueText(element.value);
    return text(element && (element.innerText || element.textContent));
  };
  const captureAssertion = event => {
    if (!uiState.assertionMode || uiState.locatorMode || uiState.stopped || uiState.paused) return false;
    const element = eventElement(event);
    if (isControlElement(element)) return false;
    event.preventDefault();
    event.stopImmediatePropagation();
    const target = snapshot(event);
    if (!target) return true;
    const selected = prompt(
      "Assertion type",
      "visible, enabled, selected, text equals, text contains, attribute equals, url equals, url contains, title equals");
    const kind = assertionKind(selected);
    if (!kind) {
      const unsupported = text(selected);
      if (unsupported) {
        uiState.assertionMode = false;
        persist();
        announce("Unsupported assertion: " + unsupported);
        sendCheckpoint("Unsupported assertion type: " + unsupported, "ASSERTION");
        setStatus();
      }
      return true;
    }
    const data = {verification: kind};
    let attributeName = "";
    if (kind === "ATTRIBUTE_EQUALS") {
      attributeName = text(prompt("Attribute name", defaultAttribute(element)));
      if (!attributeName) return true;
      data.attributeName = attributeName;
    }
    if (expectsValue(kind)) {
      const expected = prompt("Expected value", currentValue(kind, element, attributeName));
      if (expected === null) return true;
      data.expected = valueText(expected);
    }
    uiState.assertionMode = false;
    persist();
    announce("Assert " + assertionLabel(kind) + " on " + (pageAssertion(kind) ? "page" : targetName(target)));
    sendVerification(pageAssertion(kind) ? null : target, data);
    setStatus();
    return true;
  };

  const styles = () => {
    if (document.getElementById("shaft-capture-ui-style")) return;
    const style = document.createElement("style");
    style.id = "shaft-capture-ui-style";
    style.textContent = `
      #shaft-capture-ui {
        --shaft-primary: #006ec0;
        --shaft-primary-rgb: 0, 110, 192;
        --shaft-deep: #102a31;
        --shaft-deep-alt: #181f2a;
        --shaft-muted: #c8d6e7;
        --shaft-on-dark: #ffffff;
        --shaft-bg: #f7f9fb;
        --shaft-surface: #ffffff;
        --shaft-text: #17202a;
        --shaft-text-muted: #5f6f81;
        --shaft-border: #d9e2ec;
        --shaft-pass: #14804a;
        --shaft-warn: #b7791f;
        --shaft-fail: #c53030;
        position: fixed;
        right: 16px;
        bottom: 16px;
        width: min(380px, calc(100vw - 32px));
        max-height: min(520px, calc(100vh - 32px));
        z-index: 2147483647;
        display: flex;
        flex-direction: column;
        overflow: hidden;
        overflow-x: hidden;
        border: 1px solid rgba(var(--shaft-primary-rgb), .36);
        border-radius: 8px;
        background: var(--shaft-surface);
        color: var(--shaft-text);
        box-shadow: 0 18px 45px rgba(24, 31, 42, .22);
        font: 13px/1.35 system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      }
      #shaft-capture-ui * { box-sizing: border-box; font: inherit; letter-spacing: 0; }
      #shaft-capture-ui header {
        display: flex;
        align-items: center;
        gap: 8px;
        padding: 8px 10px;
        background: linear-gradient(135deg, var(--shaft-deep), var(--shaft-deep-alt));
        color: var(--shaft-on-dark);
      }
      #shaft-capture-ui .brand-mark {
        width: 28px;
        height: 28px;
        display: inline-grid;
        place-items: center;
        flex: 0 0 auto;
        border: 1px solid rgba(var(--shaft-primary-rgb), .42);
        border-radius: 8px;
        background: rgba(var(--shaft-primary-rgb), .18);
        color: var(--shaft-on-dark);
        font-weight: 700;
      }
      #shaft-capture-ui strong { flex: 1; min-width: 0; font-weight: 700; overflow-wrap: anywhere; }
      #shaft-capture-ui button {
        min-width: 28px;
        height: 28px;
        display: inline-grid;
        place-items: center;
        border: 1px solid var(--shaft-border);
        border-radius: 6px;
        background: var(--shaft-surface);
        color: var(--shaft-primary);
        cursor: pointer;
        line-height: 0;
        padding: 0;
      }
      #shaft-capture-ui button:hover { background: rgba(var(--shaft-primary-rgb), .08); }
      #shaft-capture-ui button:disabled { cursor: default; opacity: .5; }
      #shaft-capture-ui button[aria-pressed="true"] {
        background: var(--shaft-primary);
        color: var(--shaft-on-dark);
      }
      #shaft-capture-ui button svg {
        width: 16px;
        height: 16px;
        fill: none;
        stroke: currentColor;
        stroke-width: 2;
        stroke-linecap: round;
        stroke-linejoin: round;
      }
      #shaft-capture-ui .status-chip {
        display: inline-flex;
        align-items: center;
        max-width: 100%;
        min-height: 26px;
        margin: 8px 10px 0;
        padding: 4px 9px;
        border: 1px solid var(--shaft-border);
        border-radius: 999px;
        background: rgba(var(--shaft-primary-rgb), .08);
        color: var(--shaft-text);
        font-size: 12px;
        font-weight: 700;
        overflow-wrap: anywhere;
      }
      #shaft-capture-status {
        color: var(--shaft-text);
      }
      #shaft-capture-actions {
        overflow-y: auto;
        overflow-x: hidden;
        padding: 8px 10px 10px;
      }
      #shaft-capture-actions ol {
        margin: 0;
        padding: 0 0 0 22px;
      }
      #shaft-capture-actions li {
        margin: 0 0 7px;
        padding: 0;
      }
      #shaft-capture-actions div {
        display: grid;
        grid-template-columns: 1fr 32px;
        gap: 6px;
        align-items: start;
      }
      #shaft-capture-actions span {
        min-width: 0;
        overflow-wrap: anywhere;
      }
      #shaft-capture-locator-panel {
        border-top: 1px solid var(--shaft-border);
        padding: 8px 10px 10px;
        background: var(--shaft-bg);
      }
      #shaft-capture-locator-panel[hidden] { display: none; }
      #shaft-capture-locator-panel h2 {
        margin: 0 0 6px;
        font-size: 13px;
        line-height: 1.25;
      }
      #shaft-capture-locator-panel ol {
        display: grid;
        gap: 6px;
        margin: 0;
        padding: 0;
        list-style: none;
      }
      #shaft-capture-locator-panel li {
        display: grid;
        grid-template-columns: 1fr 32px;
        gap: 8px;
        align-items: start;
        padding: 7px;
        border: 1px solid var(--shaft-border);
        border-radius: 6px;
        background: var(--shaft-surface);
      }
      #shaft-capture-locator-panel li[data-pinned="true"] {
        border-color: var(--shaft-primary);
        box-shadow: inset 3px 0 0 var(--shaft-primary);
      }
      #shaft-capture-locator-panel .locator-expression,
      #shaft-capture-locator-panel .locator-meta {
        display: block;
        overflow-wrap: anywhere;
      }
      #shaft-capture-locator-panel .locator-expression {
        font-family: ui-monospace, Consolas, monospace;
        font-size: 12px;
      }
      #shaft-capture-locator-panel .locator-meta {
        color: var(--shaft-text-muted);
        font-size: 12px;
      }
      #shaft-capture-locator-highlight {
        position: fixed;
        z-index: 2147483646;
        pointer-events: none;
        border: 2px solid #006ec0;
        border-radius: 6px;
        box-shadow: 0 0 0 3px rgba(0, 110, 192, .2);
      }
    `;
    (document.head || document.documentElement).appendChild(style);
  };
  const setStatus = () => {
    const status = document.getElementById("shaft-capture-status");
    const pause = document.getElementById("shaft-capture-pause");
    const assert = document.getElementById("shaft-capture-assert");
    const pick = document.getElementById("shaft-capture-pick");
    const stop = document.getElementById("shaft-capture-stop");
    if (!status || !pause || !assert || !pick || !stop) return;
    if ((uiState.stopped || uiState.paused) && uiState.locatorMode) {
      uiState.locatorMode = false;
      clearLocatorHighlight();
      renderLocatorPanel(null);
      persist();
    }
    status.textContent = uiState.stopped
      ? "Stopped. Waiting for SHAFT to close the browser."
      : uiState.paused
        ? "Paused. Browser actions are not being captured."
        : uiState.locatorMode
          ? "Locator picker active."
          : uiState.assertionMode
            ? "Assertion mode. Click an element to capture a verification."
            : "Recording browser actions.";
    pause.innerHTML = icon(uiState.paused ? "play" : "pause");
    pause.title = uiState.paused ? "Resume recording" : "Pause recording";
    pause.setAttribute("aria-label", pause.title);
    assert.setAttribute("aria-pressed", String(uiState.assertionMode));
    assert.disabled = uiState.stopped || uiState.paused || uiState.locatorMode;
    pick.setAttribute("aria-pressed", String(uiState.locatorMode));
    pick.disabled = uiState.stopped || uiState.paused;
    stop.disabled = uiState.stopped;
  };
  const editAction = item => {
    const updated = text(prompt("Edit captured action", item.text));
    if (!updated || updated === item.text) return;
    item.text = updated;
    persist();
    sendCheckpoint("Edited captured action " + item.id + ": " + updated, "USER_MARKER");
    renderActions();
  };
  function renderActions() {
    const list = document.getElementById("shaft-capture-action-list");
    if (!list) return;
    list.textContent = "";
    uiState.actions.forEach(item => {
      const row = document.createElement("li");
      const body = document.createElement("div");
      const labelNode = document.createElement("span");
      labelNode.textContent = item.text;
      const edit = document.createElement("button");
      edit.type = "button";
      edit.innerHTML = icon("edit");
      edit.title = "Edit captured action";
      edit.setAttribute("aria-label", "Edit captured action");
      edit.addEventListener("click", () => editAction(item));
      body.append(labelNode, edit);
      row.append(body);
      list.appendChild(row);
    });
    setStatus();
  }
  const renderLocatorPanel = target => {
    const panel = document.getElementById("shaft-capture-locator-panel");
    if (!panel) return;
    panel.textContent = "";
    if (!target) {
      panel.hidden = true;
      return;
    }
    panel.hidden = false;
    const heading = document.createElement("h2");
    heading.textContent = targetName(target);
    const list = document.createElement("ol");
    (target.locators || [])
      .map(candidate => ({
        candidate,
        score: locatorScore(candidate),
        probe: locatorProbe(candidate)
      }))
      .sort((left, right) => right.score - left.score
        || left.candidate.strategy.localeCompare(right.candidate.strategy)
        || left.candidate.expression.localeCompare(right.candidate.expression))
      .forEach(item => {
        const row = document.createElement("li");
        const pinned = isPreferredLocator(target.logicalElementId, item.candidate);
        row.dataset.pinned = String(pinned);
        const body = document.createElement("div");
        const expression = document.createElement("span");
        expression.className = "locator-expression";
        expression.textContent = item.candidate.strategy + " " + item.candidate.expression;
        const meta = document.createElement("span");
        meta.className = "locator-meta";
        meta.textContent = "score " + item.score
          + " | " + item.probe.status + " (" + item.probe.count + ")"
          + " | stable " + (item.candidate.stable ? "yes" : "no")
          + " | " + locatorRationale(item.candidate);
        body.append(expression, meta);
        const pin = document.createElement("button");
        pin.type = "button";
        pin.innerHTML = icon("pin");
        pin.title = pinned ? "Pinned locator" : "Pin locator";
        pin.setAttribute("aria-label", pin.title);
        pin.disabled = pinned;
        pin.addEventListener("click", () => pinLocator(target, item.candidate));
        row.append(body, pin);
        list.appendChild(row);
      });
    panel.append(heading, list);
  };
  const clearLocatorHighlight = () => {
    const highlight = document.getElementById("shaft-capture-locator-highlight");
    if (highlight) highlight.remove();
  };
  const updateLocatorHighlight = element => {
    if (!element || element.nodeType !== Node.ELEMENT_NODE || isControlElement(element)) {
      clearLocatorHighlight();
      return;
    }
    const rect = element.getBoundingClientRect();
    if (!rect.width || !rect.height) {
      clearLocatorHighlight();
      return;
    }
    let highlight = document.getElementById("shaft-capture-locator-highlight");
    if (!highlight) {
      highlight = document.createElement("div");
      highlight.id = "shaft-capture-locator-highlight";
      highlight.setAttribute("data-shaft-capture-control", "true");
      document.documentElement.appendChild(highlight);
    }
    highlight.style.left = Math.max(0, rect.left) + "px";
    highlight.style.top = Math.max(0, rect.top) + "px";
    highlight.style.width = rect.width + "px";
    highlight.style.height = rect.height + "px";
  };
  const pinLocator = (target, candidate) => {
    uiState.locatorPreferences[target.logicalElementId] = {
      strategy: candidate.strategy,
      expression: candidate.expression
    };
    const updatedTarget = withLocatorPreference(target, candidate);
    persist();
    announce("Pin " + candidate.strategy + " locator for " + targetName(target));
    send({
      kind: "locator_preference",
      page: page(),
      target: updatedTarget,
      data: {
        logicalElementId: target.logicalElementId,
        strategy: candidate.strategy,
        expression: candidate.expression
      }
    });
    renderLocatorPanel(updatedTarget);
  };
  const captureLocatorPick = event => {
    if (!uiState.locatorMode || uiState.stopped || uiState.paused) return false;
    const element = eventElement(event);
    if (isControlElement(element)) return false;
    event.preventDefault();
    event.stopImmediatePropagation();
    const target = snapshot(event);
    if (target) renderLocatorPanel(target);
    return true;
  };
  const renderPanel = () => {
    if (!document.body || document.getElementById("shaft-capture-ui")) return;
    styles();
    const panel = document.createElement("section");
    panel.id = "shaft-capture-ui";
    panel.setAttribute("data-shaft-capture-control", "true");
    panel.innerHTML = `
      <header>
        <span class="brand-mark" aria-hidden="true">S</span>
        <strong>SHAFT Capture</strong>
        <button id="shaft-capture-pause" type="button"></button>
        <button id="shaft-capture-assert" type="button" title="Toggle assertion mode" aria-label="Toggle assertion mode" aria-pressed="false">${icon("assert")}</button>
        <button id="shaft-capture-pick" type="button" title="Toggle locator picker" aria-label="Toggle locator picker" aria-pressed="false">${icon("locator")}</button>
        <button id="shaft-capture-checkpoint" type="button" title="Add checkpoint" aria-label="Add checkpoint">${icon("checkpoint")}</button>
        <button id="shaft-capture-stop" type="button" title="Stop recording" aria-label="Stop recording">${icon("stop")}</button>
      </header>
      <div id="shaft-capture-status" class="status-chip"></div>
      <div id="shaft-capture-locator-panel" hidden></div>
      <div id="shaft-capture-actions"><ol id="shaft-capture-action-list"></ol></div>
    `;
    document.body.appendChild(panel);
    document.getElementById("shaft-capture-pause").addEventListener("click", () => {
      if (uiState.stopped) return;
      uiState.paused = !uiState.paused;
      if (uiState.paused) {
        uiState.locatorMode = false;
        clearLocatorHighlight();
        renderLocatorPanel(null);
      }
      persist();
      sendControl(uiState.paused ? "PAUSE" : "RESUME");
      setStatus();
    });
    document.getElementById("shaft-capture-assert").addEventListener("click", () => {
      if (uiState.stopped || uiState.paused) return;
      uiState.assertionMode = !uiState.assertionMode;
      if (uiState.assertionMode) {
        uiState.locatorMode = false;
        clearLocatorHighlight();
        renderLocatorPanel(null);
      }
      persist();
      setStatus();
    });
    document.getElementById("shaft-capture-pick").addEventListener("click", () => {
      if (uiState.stopped || uiState.paused) return;
      uiState.locatorMode = !uiState.locatorMode;
      if (uiState.locatorMode) {
        uiState.assertionMode = false;
      } else {
        clearLocatorHighlight();
        renderLocatorPanel(null);
      }
      persist();
      setStatus();
    });
    document.getElementById("shaft-capture-checkpoint").addEventListener("click", () => {
      if (uiState.stopped) return;
      const description = text(prompt("Checkpoint description", "Review this page state"));
      if (!description) return;
      announce("Checkpoint: " + description);
      sendCheckpoint(description, "USER_MARKER");
    });
    document.getElementById("shaft-capture-stop").addEventListener("click", () => {
      if (uiState.stopped) return;
      uiState.stopped = true;
      persist();
      announce("Stop recording");
      sendControl("STOP");
      setStatus();
    });
    renderActions();
  };
  const schedulePanel = () => {
    renderPanel();
    if (!document.body) {
      setTimeout(schedulePanel, 50);
    }
  };

  addEventListener("mousemove", event => {
    if (!uiState.locatorMode || uiState.stopped || uiState.paused) return;
    updateLocatorHighlight(eventElement(event));
  }, true);
  addEventListener("click", event => {
    if (captureLocatorPick(event)) return;
    if (captureAssertion(event)) return;
    const element = event.composedPath ? event.composedPath()[0] : event.target;
    const type = String(element && element.type || "").toLowerCase();
    const tag = String(element && element.localName || "").toLowerCase();
    if (["checkbox", "radio", "file"].includes(type) || tag === "select" || tag === "option") return;
    emit("click", event, {
      button: Number(event.button || 0),
      clickCount: Number(event.detail || 1)
    });
  }, true);
  addEventListener("dblclick", event => {
    if (captureLocatorPick(event)) return;
    emit("click", event, {
    button: Number(event.button || 0),
    clickCount: 2
    });
  }, true);
  addEventListener("input", event => {
    if (!isTextInput(event.target)) return;
    emit("input", event, {value: String(event.target.value || "")});
  }, true);
  addEventListener("change", event => {
    const element = event.target;
    const tag = String(element && element.localName || "").toLowerCase();
    const type = String(element && element.type || "").toLowerCase();
    if (tag === "select") {
      const option = element.options && element.selectedIndex >= 0
        ? element.options[element.selectedIndex]
        : null;
      emit("select", event, {
        value: String(element.value || ""),
        visibleText: text(option && option.text),
        index: Number(element.selectedIndex || 0)
      });
    } else if (type === "checkbox" || type === "radio") {
      emit("toggle", event, {checked: Boolean(element.checked)});
    } else if (type === "file") {
      const file = element.files && element.files.length ? element.files[0] : null;
      emit("upload", event, {
        fileName: file ? String(file.name || "") : "",
        mediaType: file ? String(file.type || "") : "",
        sizeBytes: file ? Number(file.size || 0) : 0
      });
    } else if (isTextInput(element)) {
      emit("input", event, {value: String(element.value || ""), committed: true});
    }
  }, true);
  addEventListener("keydown", event => {
    const modifiers = [];
    if (event.ctrlKey) modifiers.push("CONTROL");
    if (event.altKey) modifiers.push("ALT");
    if (event.metaKey) modifiers.push("META");
    if (event.shiftKey) modifiers.push("SHIFT");
    const key = String(event.key || "");
    const named = key.length > 1 && !["Shift", "Control", "Alt", "Meta"].includes(key);
    if (!named && modifiers.length === 0) return;
    emit("keyboard", event, {keys: [...modifiers, key.toUpperCase()]});
  }, true);

  if (topLevel) {
    schedulePanel();
    if (uiState.actions.length === 0) {
      announce("Open " + visibleLocation());
    }
    setInterval(() => {
      const current = String(location.href || "");
      if (current !== uiState.lastUrl) {
        uiState.lastUrl = current;
        persist();
        announce("Navigate to " + visibleLocation());
      }
    }, 500);
  }
}
