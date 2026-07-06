(channel, sink) => {
  if (globalThis.__shaftCaptureInstalled) {
    return;
  }
  globalThis.__shaftCaptureInstalled = true;
  globalThis.__shaftCaptureQueue = globalThis.__shaftCaptureQueue || [];

  const testIdAttributes = ["data-testid", "data-test", "data-qa"];
  const STORAGE_KEY = "shaft.capture.recorder.ui";
  const eventSink = sink && typeof sink === "object" ? sink : {};
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
    readinessState: "READY",
    readinessWarnings: [],
    actions: [],
    pendingSignals: [],
    nextId: 1,
    instanceId: "",
    currentInputActionKey: "",
    lastUrl: String(location.href || ""),
    ...persisted()
  };
  uiState.actions = Array.isArray(uiState.actions) ? uiState.actions.slice(-80) : [];
  uiState.pendingSignals = Array.isArray(uiState.pendingSignals) ? uiState.pendingSignals.slice(-200) : [];
  uiState.nextId = Number(uiState.nextId || uiState.actions.length + 1);
  uiState.instanceId = text(uiState.instanceId) || String(Date.now()) + "-" + Math.random().toString(36).slice(2);
  uiState.currentInputActionKey = text(uiState.currentInputActionKey);
  uiState.assertionMode = Boolean(uiState.assertionMode);
  uiState.locatorMode = Boolean(uiState.locatorMode);
  uiState.locatorPreferences = uiState.locatorPreferences && typeof uiState.locatorPreferences === "object"
    ? uiState.locatorPreferences
    : {};
  uiState.readinessState = ["READY", "RISKY", "BLOCKED"].includes(uiState.readinessState)
    ? uiState.readinessState
    : "READY";
  uiState.readinessWarnings = Array.isArray(uiState.readinessWarnings)
    ? uiState.readinessWarnings.slice(-20)
    : [];
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
        readinessState: uiState.readinessState,
        readinessWarnings: uiState.readinessWarnings.slice(-20),
        actions: uiState.actions.slice(-80),
        pendingSignals: uiState.pendingSignals.slice(-200),
        nextId: uiState.nextId,
        instanceId: uiState.instanceId,
        currentInputActionKey: uiState.currentInputActionKey,
        lastUrl: uiState.lastUrl
      }));
    } catch (ignored) {
      // Storage can be unavailable in sandboxed frames.
    }
  };
  const postToSink = payload => {
    if (!eventSink.url || !eventSink.token || typeof fetch !== "function") return;
    try {
      fetch(String(eventSink.url), {
        method: "POST",
        mode: "no-cors",
        keepalive: true,
        headers: {"Content-Type": "text/plain"},
        body: JSON.stringify({token: String(eventSink.token), payload})
      }).catch(() => {});
    } catch (ignored) {
      // Fallback polling still drains the in-page queue.
    }
  };
  const send = payload => {
    payload.timestamp = Date.now();
    uiState.pendingSignals.push(payload);
    uiState.pendingSignals = uiState.pendingSignals.slice(-200);
    persist();
    globalThis.__shaftCaptureQueue.push(payload);
    postToSink(payload);
    if (typeof channel === "function") {
      channel(JSON.stringify(payload));
    }
  };
  globalThis.__shaftCaptureDrain = () => {
    const memorySignals = globalThis.__shaftCaptureQueue ? globalThis.__shaftCaptureQueue.splice(0) : [];
    const pendingSignals = uiState.pendingSignals.slice();
    uiState.pendingSignals = [];
    persist();
    return pendingSignals.length ? pendingSignals : memorySignals;
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
  const domSnapshot = () => {
    try {
      const root = document.documentElement;
      if (!root) return "";
      const clone = root.cloneNode(true);
      clone.querySelectorAll("script, style, #shaft-capture-ui, #shaft-capture-ui-style, "
        + "#shaft-capture-locator-highlight").forEach(element => element.remove());
      clone.querySelectorAll("input, textarea, select").forEach(element => {
        element.removeAttribute("value");
        element.removeAttribute("checked");
        element.removeAttribute("selected");
      });
      return String(clone.outerHTML || "").replace(/\s+/g, " ").trim().slice(0, 20000);
    } catch (ignored) {
      return "";
    }
  };
  const page = () => ({
    url: String(location.href || ""),
    title: text(document.title),
    framePath: framePath(),
    width: Number(globalThis.innerWidth || 0),
    height: Number(globalThis.innerHeight || 0),
    domSnapshot: domSnapshot()
  });
  const visibleLocation = () => text(String(location.href || "").split(/[?#]/)[0]);
  const sendControl = (action, data) =>
    send({kind: "control", page: page(), data: {action, ...(data || {})}});
  // Checkpoint kinds (USER_MARKER/FLOW_START/FLOW_END/RECOVERY/ASSERTION) remain internal
  // CaptureEvent/Checkpoint metadata reachable through the CLI and MCP checkpoint APIs; this
  // overlay intentionally exposes no standalone checkpoint control of its own.
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
    stop: `<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M7 7h10v10H7z"></path></svg>`,
    edit: `<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M12 20h9"></path><path d="M16.5 3.5a2.1 2.1 0 0 1 3 3L7 19l-4 1 1-4Z"></path></svg>`,
    delete: `<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M3 6h18"></path><path d="M8 6V4h8v2"></path><path d="M6 6l1 14h10l1-14"></path><path d="M10 11v5"></path><path d="M14 11v5"></path></svg>`,
    up: `<svg viewBox="0 0 24 24" aria-hidden="true"><path d="m18 15-6-6-6 6"></path></svg>`,
    down: `<svg viewBox="0 0 24 24" aria-hidden="true"><path d="m6 9 6 6 6-6"></path></svg>`,
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
  const clientActionId = item => uiState.instanceId + "-" + item.id;
  const announce = (value, mergeKey, details) => {
    const description = text(value);
    if (!description) return null;
    if (mergeKey && uiState.currentInputActionKey === mergeKey) {
      let existing = null;
      for (let index = uiState.actions.length - 1; index >= 0; index--) {
        if (uiState.actions[index].mergeKey === mergeKey) {
          existing = uiState.actions[index];
          break;
        }
      }
      if (existing) {
        existing.text = description;
        existing.timestamp = Date.now();
        existing.details = details || existing.details || {};
        persist();
        renderActions();
        return existing;
      }
    }
    const item = {
      id: uiState.nextId++,
      text: description,
      timestamp: Date.now(),
      mergeKey: mergeKey || "",
      details: details || {}
    };
    uiState.actions.push(item);
    uiState.actions = uiState.actions.slice(-80);
    uiState.currentInputActionKey = mergeKey || "";
    persist();
    renderActions();
    return item;
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
  const readinessRank = state => ({READY: 0, RISKY: 1, BLOCKED: 2})[state] || 0;
  const setReadiness = (state, warning) => {
    if (readinessRank(state) > readinessRank(uiState.readinessState)) {
      uiState.readinessState = state;
    }
    if (warning) {
      uiState.readinessWarnings.push(text(warning));
      uiState.readinessWarnings = Array.from(new Set(uiState.readinessWarnings)).slice(-20);
    }
    persist();
    setStatus();
  };
  const positionalLocator = candidate =>
    ["CSS", "XPATH"].includes(candidate.strategy)
    && ((candidate.signals || []).includes("POSITIONAL")
      || /\[\d+]|:nth-(?:child|of-type)\(/.test(candidate.expression || ""));
  const sensitiveTarget = target => {
    const attributes = target.attributes || {};
    const value = [
      target.logicalElementId,
      target.accessibleName,
      target.label,
      attributes.type,
      attributes.name,
      attributes.id,
      attributes.autocomplete
    ].join(" ").toLowerCase();
    return value.includes("password") || value.includes("token") || value.includes("secret");
  };
  const submitTarget = target => {
    const attributes = target.attributes || {};
    const value = [target.accessibleName, target.label, attributes.value, attributes.type]
      .join(" ").toLowerCase();
    return attributes.type === "submit"
      || ["submit", "pay", "checkout", "place order", "confirm", "sign in", "log in", "continue"]
        .some(item => value.includes(item));
  };
  const readinessFor = (kind, target) => {
    const locators = (target.locators || []).slice()
      .sort((left, right) => locatorScore(right) - locatorScore(left));
    if (!locators.length) {
      return {state: "BLOCKED", warning: "Step " + uiState.nextId + " has no locator evidence."};
    }
    const best = locators[0];
    if (best.uniquenessCount === 0) {
      return {state: "BLOCKED", warning: "Step " + uiState.nextId + " locator has no current match."};
    }
    if (kind === "input" && sensitiveTarget(target)) {
      return {state: "BLOCKED", warning: "Step " + uiState.nextId + " uses redacted required input."};
    }
    if (best.uniquenessCount > 1) {
      return {state: "RISKY", warning: "Step " + uiState.nextId + " locator has multiple matches."};
    }
    if (positionalLocator(best)) {
      return {state: "RISKY", warning: "Step " + uiState.nextId + " uses generated positional "
        + best.strategy + "."};
    }
    if (kind === "click" && submitTarget(target)) {
      return {state: "RISKY", warning: "Step " + uiState.nextId
        + " needs a follow-up assertion after form submission."};
    }
    return null;
  };
  const bestLocatorSummary = target => {
    const best = (target.locators || []).slice()
      .sort((left, right) => locatorScore(right) - locatorScore(left))[0];
    if (!best) return "";
    return best.strategy + " " + best.expression
      + " | score " + locatorScore(best)
      + " | matches " + best.uniquenessCount;
  };
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
  const withLocatorPreference = (target, candidate) => {
    const existing = target.locators || [];
    const known = existing.some(item =>
      item.strategy === candidate.strategy && item.expression === candidate.expression);
    return {
      ...target,
      locators: known
        ? existing.map(item =>
          item.strategy === candidate.strategy && item.expression === candidate.expression
            ? withUserProvidedSignal(item)
            : item)
        : [...existing, withUserProvidedSignal(candidate)]
    };
  };
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
      const readiness = readinessFor(kind, target);
      if (readiness) setReadiness(readiness.state, readiness.warning);
      const mergeKey = kind === "input" ? "input:" + target.logicalElementId : "";
      const item = announce(describe(kind, target, action), mergeKey, {
        kind,
        target,
        locator: bestLocatorSummary(target),
        readiness: readiness ? readiness.state : "READY",
        warning: readiness ? readiness.warning : ""
      });
      if (kind !== "input") {
        uiState.currentInputActionKey = "";
        persist();
      }
      if (item) {
        action.clientActionId = clientActionId(item);
      }
      send({kind, page: page(), target, data: action});
    }
  };
  const isTextInput = element => {
    const tag = String(element && element.localName || "").toLowerCase();
    const type = String(element && element.type || "text").toLowerCase();
    return tag === "textarea" || (tag === "input" &&
      !["button", "submit", "reset", "checkbox", "radio", "file", "hidden"].includes(type));
  };
  // Deterministic, fixed assertion catalogs. These mirror CaptureEvent.VerificationKind
  // 1:1 so every choice maps directly to a SHAFT Validations API call in CaptureGenerator;
  // nothing here is generated or inferred at runtime.
  const ELEMENT_ASSERTIONS = [
    {kind: "ELEMENT_VISIBLE", label: "Element is visible", needsValue: false, needsAttribute: false},
    {kind: "ELEMENT_ENABLED", label: "Element is enabled", needsValue: false, needsAttribute: false},
    {kind: "ELEMENT_SELECTED", label: "Element is selected", needsValue: false, needsAttribute: false},
    {kind: "TEXT_EQUALS", label: "Text equals", needsValue: true, needsAttribute: false},
    {kind: "TEXT_CONTAINS", label: "Text contains", needsValue: true, needsAttribute: false},
    {kind: "ATTRIBUTE_EQUALS", label: "Attribute equals", needsValue: true, needsAttribute: true},
    {kind: "ELEMENT_IMAGE_MATCHES", label: "Image matches reference", needsValue: false, needsAttribute: false}
  ];
  const BROWSER_ASSERTIONS = [
    {kind: "URL_EQUALS", label: "URL equals", needsValue: true, needsAttribute: false},
    {kind: "URL_CONTAINS", label: "URL contains", needsValue: true, needsAttribute: false},
    {kind: "TITLE_EQUALS", label: "Title equals", needsValue: true, needsAttribute: false},
    {kind: "TITLE_CONTAINS", label: "Title contains", needsValue: true, needsAttribute: false},
    {kind: "PAGE_TEXT_CONTAINS", label: "Page text contains", needsValue: true, needsAttribute: false}
  ];
  const currentValue = (kind, element, attributeName) => {
    if (kind === "URL_EQUALS" || kind === "URL_CONTAINS") return valueText(location.href);
    if (kind === "TITLE_EQUALS" || kind === "TITLE_CONTAINS") return valueText(document.title);
    if (kind === "PAGE_TEXT_CONTAINS") return valueText(document.body && document.body.innerText);
    if (kind === "ATTRIBUTE_EQUALS") return valueText(element && element.getAttribute(attributeName));
    if (isTextInput(element)) return valueText(element.value);
    return text(element && (element.innerText || element.textContent));
  };
  const defaultAttribute = element =>
    ["value", "aria-label", "title", "href", "id", "name"]
      .find(name => element && element.hasAttribute && element.hasAttribute(name)) || "value";
  // Renders the same SHAFT locator syntax CaptureGenerator emits (see
  // CaptureGenerator#locatorExpression) so the picker shows exactly what generated code will use.
  const shaftLocatorSyntax = candidate => {
    const expression = String(candidate.expression || "");
    switch (candidate.strategy) {
      case "TEST_ID":
      case "CSS":
        return `SHAFT.GUI.Locator.cssSelector("${expression}")`;
      case "ID":
        return `SHAFT.GUI.Locator.id("${expression}")`;
      case "NAME":
        return `SHAFT.GUI.Locator.name("${expression}")`;
      case "XPATH":
        return `By.xpath("${expression}")`;
      case "ROLE":
      case "ACCESSIBLE_NAME":
      case "LABEL":
      default:
        return `SHAFT.GUI.Locator.hasAnyTagName().containsText("${expression}").build()`;
    }
  };
  const closeDialog = () => {
    const dialog = document.getElementById("shaft-capture-dialog");
    if (dialog) dialog.remove();
  };
  const showDialog = (title, fields, onSubmit) => {
    const panel = document.getElementById("shaft-capture-ui");
    if (!panel) return;
    closeDialog();
    const dialog = document.createElement("form");
    dialog.id = "shaft-capture-dialog";
    dialog.setAttribute("data-shaft-capture-control", "true");
    const heading = document.createElement("h2");
    heading.textContent = title;
    dialog.appendChild(heading);
    fields.forEach(field => {
      const labelNode = document.createElement("label");
      const caption = document.createElement("span");
      caption.textContent = field.label;
      let input;
      if (field.type === "select") {
        input = document.createElement("select");
        (field.options || []).forEach(option => {
          const item = document.createElement("option");
          item.value = option.value;
          item.textContent = option.label;
          input.appendChild(item);
        });
      } else {
        input = document.createElement("input");
        input.type = "text";
      }
      input.name = field.name;
      input.value = field.value || "";
      input.placeholder = field.placeholder || "";
      labelNode.append(caption, input);
      dialog.appendChild(labelNode);
    });
    const buttons = document.createElement("div");
    buttons.className = "dialog-actions";
    const cancel = document.createElement("button");
    cancel.type = "button";
    cancel.textContent = "Cancel";
    cancel.addEventListener("click", closeDialog);
    const save = document.createElement("button");
    save.type = "submit";
    save.textContent = "Save";
    buttons.append(cancel, save);
    dialog.appendChild(buttons);
    dialog.addEventListener("submit", event => {
      event.preventDefault();
      const data = {};
      fields.forEach(field => {
        data[field.name] = valueText(dialog.elements[field.name] && dialog.elements[field.name].value);
      });
      closeDialog();
      onSubmit(data);
    });
    const status = document.getElementById("shaft-capture-status");
    if (status && status.nextSibling) {
      panel.insertBefore(dialog, status.nextSibling);
    } else {
      panel.appendChild(dialog);
    }
    const first = dialog.querySelector("select,input");
    if (first) first.focus();
  };
  // --- Single "Assertion" entry point -----------------------------------------------------
  // Two deterministic branches: Element (pick -> scored locator -> fixed catalog -> data)
  // and Browser (fixed catalog -> data). No checkpoint kind is ever surfaced as its own
  // button; USER_MARKER/FLOW_START/FLOW_END/RECOVERY remain internal event metadata only,
  // reachable from the CLI/MCP checkpoint API rather than this overlay.
  const closeAssertionPanel = () => {
    const panel = document.getElementById("shaft-capture-assertion-panel");
    if (panel) panel.remove();
  };
  const manualLocatorCandidate = (strategy, expression) => ({
    strategy,
    expression: text(expression),
    uniquenessCount: 1,
    visible: true,
    stable: true,
    signals: ["USER_PROVIDED"]
  });
  const detectManualStrategy = value => {
    const trimmed = String(value || "").trim();
    if (!trimmed) return null;
    if (trimmed.startsWith("/") || trimmed.startsWith("(") || trimmed.startsWith(".")) return "XPATH";
    return "CSS";
  };
  const topLocatorCandidates = target => (target.locators || [])
    .map(candidate => ({candidate, score: locatorScore(candidate)}))
    .sort((left, right) => right.score - left.score
      || left.candidate.strategy.localeCompare(right.candidate.strategy)
      || left.candidate.expression.localeCompare(right.candidate.expression))
    .slice(0, 4);
  const renderAssertionPanel = (title, body) => {
    const host = document.getElementById("shaft-capture-ui");
    if (!host) return null;
    closeAssertionPanel();
    const panel = document.createElement("section");
    panel.id = "shaft-capture-assertion-panel";
    panel.setAttribute("data-shaft-capture-control", "true");
    const heading = document.createElement("h2");
    heading.textContent = title;
    panel.appendChild(heading);
    body.forEach(node => panel.appendChild(node));
    const status = document.getElementById("shaft-capture-status");
    if (status && status.nextSibling) {
      host.insertBefore(panel, status.nextSibling);
    } else {
      host.appendChild(panel);
    }
    return panel;
  };
  const assertionButton = (label, onClick, kind) => {
    const button = document.createElement("button");
    button.type = "button";
    button.className = "assertion-choice";
    button.textContent = label;
    if (kind) button.dataset.assertionKind = kind;
    button.addEventListener("click", onClick);
    return button;
  };
  const cancelAssertionRow = onCancel => {
    const row = document.createElement("div");
    row.className = "dialog-actions";
    const cancel = document.createElement("button");
    cancel.type = "button";
    cancel.textContent = "Cancel";
    cancel.addEventListener("click", onCancel);
    row.appendChild(cancel);
    return row;
  };
  const finishAssertion = (target, payload, summaryText) => {
    announce(summaryText, "", target ? {
      kind: "verification",
      target,
      locator: bestLocatorSummary(target),
      readiness: "READY",
      warning: ""
    } : {kind: "verification", readiness: "READY", warning: ""});
    sendVerification(target, payload);
    closeAssertionPanel();
    uiState.assertionMode = false;
    uiState.locatorMode = false;
    persist();
    setStatus();
  };
  const showAssertionDataStep = (catalogEntry, target, element) => {
    const list = document.createElement("div");
    list.className = "assertion-data-step";
    const fields = [];
    if (catalogEntry.needsAttribute) {
      const label = document.createElement("label");
      const caption = document.createElement("span");
      caption.textContent = "Attribute";
      const input = document.createElement("input");
      input.type = "text";
      input.name = "attributeName";
      input.value = defaultAttribute(element);
      label.append(caption, input);
      list.appendChild(label);
      fields.push(input);
    }
    let valueInput = null;
    if (catalogEntry.needsValue) {
      const label = document.createElement("label");
      const caption = document.createElement("span");
      caption.textContent = "Expected value";
      valueInput = document.createElement("input");
      valueInput.type = "text";
      valueInput.name = "expected";
      valueInput.value = currentValue(catalogEntry.kind, element,
        fields.length ? fields[0].value : "");
      label.append(caption, valueInput);
      list.appendChild(label);
    }
    const confirm = document.createElement("button");
    confirm.type = "button";
    confirm.textContent = "Save assertion";
    confirm.addEventListener("click", () => {
      const payload = {verification: catalogEntry.kind, negated: false};
      if (catalogEntry.needsAttribute) {
        payload.attributeName = text(fields[0].value);
        if (!payload.attributeName) return;
      }
      if (catalogEntry.needsValue) {
        payload.expected = valueText(valueInput.value);
      }
      const name = target ? targetName(target) : "page";
      finishAssertion(target, payload, "Assert " + catalogEntry.label.toLowerCase() + " on " + name);
    });
    const actions = cancelAssertionRow(closeAssertionPanel);
    actions.appendChild(confirm);
    renderAssertionPanel(catalogEntry.label, [list, actions]);
    const first = list.querySelector("input");
    if (first) first.focus();
  };
  const showAssertionCatalogStep = (catalog, target, element) => {
    const list = document.createElement("div");
    list.className = "assertion-catalog-step";
    catalog.forEach(entry => {
      list.appendChild(assertionButton(entry.label,
        () => showAssertionDataStep(entry, target, element), entry.kind));
    });
    const actions = cancelAssertionRow(closeAssertionPanel);
    renderAssertionPanel(target ? "Element assertion" : "Browser assertion", [list, actions]);
  };
  const showLocatorPickStep = (target, element) => {
    const list = document.createElement("ol");
    list.className = "assertion-locator-step";
    topLocatorCandidates(target).forEach(item => {
      const row = document.createElement("li");
      const expression = document.createElement("span");
      expression.className = "locator-expression";
      expression.textContent = shaftLocatorSyntax(item.candidate);
      const meta = document.createElement("span");
      meta.className = "locator-meta";
      meta.textContent = item.candidate.strategy + " | score " + item.score
        + " | matches " + item.candidate.uniquenessCount;
      const choose = document.createElement("button");
      choose.type = "button";
      choose.textContent = "Use this locator";
      choose.addEventListener("click", () =>
        showAssertionCatalogStep(ELEMENT_ASSERTIONS, withLocatorPreference(target, item.candidate), element));
      row.append(expression, meta, choose);
      list.appendChild(row);
    });
    const manualLabel = document.createElement("label");
    const manualCaption = document.createElement("span");
    manualCaption.textContent = "Manual locator (XPath or CSS)";
    const manualInput = document.createElement("input");
    manualInput.type = "text";
    manualInput.name = "manualLocator";
    manualInput.placeholder = "//button[@id='submit'] or #submit";
    manualLabel.append(manualCaption, manualInput);
    const manualUse = document.createElement("button");
    manualUse.type = "button";
    manualUse.textContent = "Use manual locator";
    manualUse.addEventListener("click", () => {
      const strategy = detectManualStrategy(manualInput.value);
      if (!strategy) return;
      const candidate = manualLocatorCandidate(strategy, manualInput.value);
      showAssertionCatalogStep(ELEMENT_ASSERTIONS, withLocatorPreference(target, candidate), element);
    });
    const manualRow = document.createElement("div");
    manualRow.className = "assertion-manual-locator";
    manualRow.append(manualLabel, manualUse);
    const actions = cancelAssertionRow(closeAssertionPanel);
    renderAssertionPanel(targetName(target), [list, manualRow, actions]);
  };
  const beginElementAssertion = () => {
    uiState.assertionMode = true;
    uiState.locatorMode = false;
    clearLocatorHighlight();
    renderLocatorPanel(null);
    persist();
    announce("Element assertion: click the target element");
    setStatus();
  };
  const beginBrowserAssertion = () => {
    closeAssertionPanel();
    showAssertionCatalogStep(BROWSER_ASSERTIONS, null, null);
  };
  const showAssertionEntryPoint = () => {
    if (uiState.stopped) return;
    const list = document.createElement("div");
    list.className = "assertion-catalog-step";
    list.appendChild(assertionButton("Element", () => {
      closeAssertionPanel();
      beginElementAssertion();
    }));
    list.appendChild(assertionButton("Browser", beginBrowserAssertion));
    const actions = cancelAssertionRow(closeAssertionPanel);
    renderAssertionPanel("Add assertion", [list, actions]);
  };
  const captureAssertion = event => {
    if (!uiState.assertionMode || uiState.locatorMode || uiState.stopped || uiState.paused) return false;
    const element = eventElement(event);
    if (isControlElement(element)) return false;
    event.preventDefault();
    event.stopImmediatePropagation();
    const target = snapshot(event);
    if (!target) return true;
    uiState.assertionMode = false;
    persist();
    showLocatorPickStep(target, element);
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
      #shaft-capture-status[data-readiness="RISKY"] {
        border-color: var(--shaft-warn);
        background: rgba(183, 121, 31, .12);
      }
      #shaft-capture-status[data-readiness="BLOCKED"] {
        border-color: var(--shaft-fail);
        background: rgba(197, 48, 48, .12);
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
      #shaft-capture-actions .action-row {
        display: grid;
        grid-template-columns: 1fr repeat(5, 28px);
        gap: 6px;
        align-items: start;
      }
      #shaft-capture-actions span,
      #shaft-capture-actions small {
        min-width: 0;
        overflow-wrap: anywhere;
      }
      #shaft-capture-actions small {
        display: block;
        margin-top: 3px;
        color: var(--shaft-text-muted);
        font-size: 12px;
      }
      #shaft-capture-dialog {
        display: grid;
        gap: 7px;
        margin: 8px 10px 0;
        padding: 8px;
        border: 1px solid var(--shaft-border);
        border-radius: 6px;
        background: var(--shaft-bg);
      }
      #shaft-capture-dialog h2 {
        margin: 0;
        font-size: 13px;
        font-weight: 700;
      }
      #shaft-capture-dialog label {
        display: grid;
        gap: 3px;
        color: var(--shaft-text-muted);
        font-size: 12px;
      }
      #shaft-capture-dialog input,
      #shaft-capture-dialog select {
        min-width: 0;
        height: 30px;
        border: 1px solid var(--shaft-border);
        border-radius: 6px;
        background: var(--shaft-surface);
        color: var(--shaft-text);
        padding: 4px 7px;
      }
      #shaft-capture-dialog .dialog-actions {
        display: flex;
        justify-content: flex-end;
        gap: 6px;
      }
      #shaft-capture-dialog .dialog-actions button {
        width: auto;
        min-width: 64px;
        padding: 0 10px;
        line-height: 1;
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
      #shaft-capture-assertion-panel {
        display: grid;
        gap: 8px;
        margin: 8px 10px 0;
        padding: 8px;
        border: 1px solid var(--shaft-border);
        border-radius: 6px;
        background: var(--shaft-bg);
      }
      #shaft-capture-assertion-panel h2 {
        margin: 0;
        font-size: 13px;
        font-weight: 700;
      }
      #shaft-capture-assertion-panel label {
        display: grid;
        gap: 3px;
        color: var(--shaft-text-muted);
        font-size: 12px;
      }
      #shaft-capture-assertion-panel input {
        min-width: 0;
        height: 30px;
        border: 1px solid var(--shaft-border);
        border-radius: 6px;
        background: var(--shaft-surface);
        color: var(--shaft-text);
        padding: 4px 7px;
      }
      #shaft-capture-assertion-panel .assertion-catalog-step {
        display: grid;
        gap: 6px;
      }
      #shaft-capture-assertion-panel button.assertion-choice {
        width: auto;
        height: auto;
        justify-content: flex-start;
        padding: 7px 10px;
        text-align: left;
      }
      #shaft-capture-assertion-panel .assertion-locator-step {
        display: grid;
        gap: 6px;
        margin: 0;
        padding: 0;
        list-style: none;
      }
      #shaft-capture-assertion-panel .assertion-locator-step li {
        display: grid;
        gap: 4px;
        padding: 7px;
        border: 1px solid var(--shaft-border);
        border-radius: 6px;
        background: var(--shaft-surface);
      }
      #shaft-capture-assertion-panel .assertion-locator-step .locator-expression {
        font-family: ui-monospace, Consolas, monospace;
        font-size: 12px;
        overflow-wrap: anywhere;
      }
      #shaft-capture-assertion-panel .assertion-locator-step .locator-meta {
        color: var(--shaft-text-muted);
        font-size: 12px;
      }
      #shaft-capture-assertion-panel .assertion-locator-step button {
        width: auto;
        justify-self: start;
        padding: 0 10px;
      }
      #shaft-capture-assertion-panel .assertion-manual-locator {
        display: grid;
        gap: 6px;
      }
      #shaft-capture-assertion-panel .assertion-data-step {
        display: grid;
        gap: 7px;
      }
      #shaft-capture-assertion-panel .dialog-actions {
        display: flex;
        justify-content: flex-end;
        gap: 6px;
      }
      #shaft-capture-assertion-panel .dialog-actions button {
        width: auto;
        min-width: 64px;
        padding: 0 10px;
        line-height: 1;
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
    if ((uiState.stopped || uiState.paused) && uiState.assertionMode) {
      uiState.assertionMode = false;
      closeAssertionPanel();
      persist();
    }
    const assertionPanelOpen = Boolean(document.getElementById("shaft-capture-assertion-panel"));
    const latestWarning = uiState.readinessWarnings[uiState.readinessWarnings.length - 1] || "";
    const base = uiState.readinessState + " | " + uiState.actions.length + " events";
    const mode = uiState.stopped
      ? "Stopped. Waiting for SHAFT to close the browser."
      : uiState.paused
        ? "Paused. Browser actions are not being captured."
        : uiState.locatorMode
          ? "Locator picker active."
          : uiState.assertionMode
            ? "Assertion mode. Click an element to capture the assertion target."
            : assertionPanelOpen
              ? "Choose an assertion."
              : "Recording browser actions.";
    status.dataset.readiness = uiState.readinessState;
    status.textContent = latestWarning ? base + " | " + latestWarning : base + " | " + mode;
    pause.innerHTML = icon(uiState.paused ? "play" : "pause");
    pause.title = uiState.paused ? "Resume recording" : "Pause recording";
    pause.setAttribute("aria-label", pause.title);
    assert.setAttribute("aria-pressed", String(uiState.assertionMode || assertionPanelOpen));
    assert.disabled = uiState.stopped || uiState.paused || uiState.locatorMode;
    pick.setAttribute("aria-pressed", String(uiState.locatorMode));
    pick.disabled = uiState.stopped || uiState.paused;
    stop.disabled = uiState.stopped;
  };
  const editAction = item => {
    showDialog("Edit captured action", [
      {name: "description", label: "Description", value: item.text}
    ], data => {
      const updated = text(data.description);
      if (!updated || updated === item.text) return;
      item.text = updated;
      persist();
      send({
        kind: "step_update",
        page: page(),
        data: {
          clientActionId: clientActionId(item),
          description: updated
        }
      });
      renderActions();
    });
  };
  const moveStep = (item, direction) => {
    const index = uiState.actions.findIndex(action => action.id === item.id);
    const offset = direction === "up" ? -1 : 1;
    const next = index + offset;
    if (index < 0 || next < 0 || next >= uiState.actions.length) return;
    const copy = uiState.actions.slice();
    [copy[index], copy[next]] = [copy[next], copy[index]];
    uiState.actions = copy;
    persist();
    send({
      kind: "step_reorder",
      page: page(),
      data: {
        clientActionId: clientActionId(item),
        direction
      }
    });
    renderActions();
  };
  const quickAssertion = item => {
    const target = item.details && item.details.target;
    if (!target) return;
    announce("Assert element visible on " + targetName(target), "", {
      kind: "verification",
      target,
      locator: bestLocatorSummary(target),
      readiness: "READY",
      warning: ""
    });
    sendVerification(target, {verification: "ELEMENT_VISIBLE"});
  };
  const deleteAction = item => {
    uiState.actions = uiState.actions.filter(action => action.id !== item.id);
    if (item.mergeKey && uiState.currentInputActionKey === item.mergeKey) {
      uiState.currentInputActionKey = "";
    }
    persist();
    send({
      kind: "step_delete",
      page: page(),
      data: {
        clientActionId: clientActionId(item)
      }
    });
    renderActions();
  };
  function renderActions() {
    const list = document.getElementById("shaft-capture-action-list");
    if (!list) return;
    list.textContent = "";
    uiState.actions.forEach((item, index) => {
      const row = document.createElement("li");
      const body = document.createElement("div");
      body.className = "action-row";
      const textWrap = document.createElement("span");
      const labelNode = document.createElement("span");
      labelNode.textContent = item.text;
      textWrap.appendChild(labelNode);
      const detail = document.createElement("small");
      const details = item.details || {};
      detail.textContent = [details.locator, details.warning].filter(Boolean).join(" | ");
      if (detail.textContent) textWrap.appendChild(detail);
      const assert = document.createElement("button");
      assert.type = "button";
      assert.innerHTML = icon("assert");
      assert.title = "Add visible assertion for this target";
      assert.setAttribute("aria-label", "Add visible assertion for this target");
      assert.disabled = !details.target;
      assert.addEventListener("click", () => quickAssertion(item));
      const up = document.createElement("button");
      up.type = "button";
      up.innerHTML = icon("up");
      up.title = "Move captured action up";
      up.setAttribute("aria-label", "Move captured action up");
      up.disabled = index === 0;
      up.addEventListener("click", () => moveStep(item, "up"));
      const down = document.createElement("button");
      down.type = "button";
      down.innerHTML = icon("down");
      down.title = "Move captured action down";
      down.setAttribute("aria-label", "Move captured action down");
      down.disabled = index === uiState.actions.length - 1;
      down.addEventListener("click", () => moveStep(item, "down"));
      const edit = document.createElement("button");
      edit.type = "button";
      edit.innerHTML = icon("edit");
      edit.title = "Edit captured action";
      edit.setAttribute("aria-label", "Edit captured action");
      edit.addEventListener("click", () => editAction(item));
      const remove = document.createElement("button");
      remove.type = "button";
      remove.innerHTML = icon("delete");
      remove.title = "Delete captured action";
      remove.setAttribute("aria-label", "Delete captured action");
      remove.addEventListener("click", () => deleteAction(item));
      body.append(textWrap, assert, up, down, edit, remove);
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
        <button id="shaft-capture-assert" type="button" title="Add assertion" aria-label="Add assertion" aria-pressed="false">${icon("assert")}</button>
        <button id="shaft-capture-pick" type="button" title="Toggle locator picker" aria-label="Toggle locator picker" aria-pressed="false">${icon("locator")}</button>
        <button id="shaft-capture-stop" type="button" title="Stop recording" aria-label="Stop recording">${icon("stop")}</button>
      </header>
      <div id="shaft-capture-status" class="status-chip shaft-capture-readiness"></div>
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
      if (uiState.assertionMode || document.getElementById("shaft-capture-assertion-panel")) {
        uiState.assertionMode = false;
        closeAssertionPanel();
        persist();
        setStatus();
        return;
      }
      uiState.locatorMode = false;
      clearLocatorHighlight();
      renderLocatorPanel(null);
      persist();
      setStatus();
      showAssertionEntryPoint();
    });
    document.getElementById("shaft-capture-pick").addEventListener("click", () => {
      if (uiState.stopped || uiState.paused) return;
      uiState.locatorMode = !uiState.locatorMode;
      if (uiState.locatorMode) {
        uiState.assertionMode = false;
        closeAssertionPanel();
      } else {
        clearLocatorHighlight();
        renderLocatorPanel(null);
      }
      persist();
      setStatus();
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
  setInterval(() => {
    const current = String(location.href || "");
    if (uiState.stopped || uiState.paused || current === uiState.lastUrl) return;
    setReadiness("RISKY", "Step " + uiState.nextId + " needs a follow-up assertion after navigation.");
    uiState.lastUrl = current;
    announce("Navigate to " + visibleLocation());
    persist();
  }, 500);

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
  const editingKeys = new Set(["Backspace", "Delete", "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight"]);
  const isEditingKey = key => editingKeys.has(key);
  addEventListener("input", event => {
    if (!isTextInput(event.target)) return;
    emit("input", event, {value: String(event.target.value || "")});
  }, true);
  addEventListener("blur", event => {
    const element = event.target;
    if (!isTextInput(element)) return;
    const target = snapshot(event);
    if (!target) return;
    const key = "input:" + target.logicalElementId;
    if (uiState.currentInputActionKey === key) {
      uiState.currentInputActionKey = "";
      persist();
    }
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
    // Shift alone (capital letters, shifted symbols like "!" or "@") produces a normal
    // printable character already captured by the "input" listener; only Ctrl/Meta, or
    // Ctrl+Alt held together (AltGr text composition on some layouts), read as a real
    // shortcut here, so typing a sentence does not fragment into one action per shifted
    // character and does not flush the in-progress "input" merge early.
    const hasShortcutModifier = event.metaKey || (event.ctrlKey !== event.altKey);
    const element = eventElement(event);
    // Suppress standalone editing key actions (Backspace, Delete, arrow keys) when
    // focus is inside a text-entry element. These are coalesced into the input's
    // final value, emitted on blur/change/submit or before the next non-typing action.
    // Non-input contexts (e.g. Backspace as page-navigation on a focused button)
    // still record the keyboard action.
    if (isEditingKey(key) && isTextInput(element) && !hasShortcutModifier) {
      return;
    }
    if (!named && !hasShortcutModifier) return;
    emit("keyboard", event, {keys: [...modifiers, key.toUpperCase()]});
  }, true);
  addEventListener("pagehide", persist, true);
  addEventListener("beforeunload", persist, true);

  // A fresh top-level document (e.g. after a cross-origin navigation) always starts with
  // empty page-scoped sessionStorage, even mid-recording. Before announcing a brand-new
  // session, ask the loopback sink -- which is not origin-scoped -- whether this browser
  // is already mid-recording, so the panel continues instead of appearing to have reset.
  const announceStart = () => {
    if (!eventSink.url || !eventSink.token || typeof fetch !== "function") {
      announce("Open " + visibleLocation());
      return;
    }
    fetch(String(eventSink.url) + "?token=" + encodeURIComponent(String(eventSink.token)))
      .then(response => response.ok ? response.json() : null)
      .then(state => {
        const eventCount = state ? Number(state.eventCount) : 0;
        if (!eventCount) {
          announce("Open " + visibleLocation());
          return;
        }
        uiState.instanceId = text(state.instanceId) || uiState.instanceId;
        uiState.nextId = Math.max(uiState.nextId, eventCount + 1);
        if (["READY", "RISKY", "BLOCKED"].includes(state.readinessState)) {
          uiState.readinessState = state.readinessState;
        }
        persist();
        announce("Continuing recording after navigating to a new domain ("
          + eventCount + " action" + (eventCount === 1 ? "" : "s") + " already captured)");
      })
      .catch(() => announce("Open " + visibleLocation()));
  };

  if (topLevel) {
    schedulePanel();
    if (uiState.actions.length === 0) {
      announceStart();
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
