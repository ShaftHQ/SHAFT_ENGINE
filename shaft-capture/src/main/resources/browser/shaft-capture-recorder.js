(channel, sink) => {
  if (globalThis.__shaftCaptureInstalled) {
    return;
  }
  globalThis.__shaftCaptureInstalled = true;
  globalThis.__shaftCaptureQueue = globalThis.__shaftCaptureQueue || [];

  const testIdAttributes = ["data-testid", "data-test", "data-qa"];
  const stepsEndpoint = {url: "", token: ""};
  // Loopback sink injected at build time so BiDi preload instances also get dual delivery
  // (script channel + HTTP sink); the fallback installer passes it as the sink argument.
  const injectedSink = {url: "", token: ""};
  const STORAGE_KEY = "shaft.capture.recorder.ui";
  const eventSink = sink && typeof sink === "object" ? sink : injectedSink;
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
  const topLevel = (() => {
    try {
      return globalThis.top === globalThis;
    } catch (ignored) {
      return false;
    }
  })();
  // The UI-state storage key is page-scoped but shared by every same-origin frame. Only the
  // top-level frame (the only one that owns the overlay) may read or write it: a subframe
  // restoring the top page's lastUrl announced phantom "Navigate to" rows for its own URL, and
  // its pagehide persist raced the top frame's, leaking those rows into the next page (#3432).
  const persisted = () => {
    if (!topLevel) return {};
    try {
      return JSON.parse(sessionStorage.getItem(STORAGE_KEY) || "{}");
    } catch (ignored) {
      return {};
    }
  };
  const uiState = {
    paused: false,
    stopped: false,
    minimized: false,
    coachDismissed: false,
    // #3536 C1: the opt-in suppressed-events log follows the exact coachDismissed precedent
    // (default here, sanitize below, write out in persist()).
    suppressedLogVisible: false,
    assertionMode: false,
    locatorMode: false,
    locatorPreferences: {},
    panelPosition: null,
    readinessState: "READY",
    readinessWarnings: [],
    suppressedEvents: [],
    actions: [],
    pendingSignals: [],
    nextId: 1,
    instanceId: "",
    currentInputActionKey: "",
    lastUrl: String(location.href || ""),
    lastInteractionAt: 0,
    ...persisted()
  };
  uiState.actions = Array.isArray(uiState.actions) ? uiState.actions.slice(-80) : [];
  uiState.pendingSignals = Array.isArray(uiState.pendingSignals) ? uiState.pendingSignals.slice(-200) : [];
  uiState.nextId = Number(uiState.nextId || uiState.actions.length + 1);
  uiState.instanceId = text(uiState.instanceId) || String(Date.now()) + "-" + Math.random().toString(36).slice(2);
  uiState.currentInputActionKey = text(uiState.currentInputActionKey);
  uiState.assertionMode = Boolean(uiState.assertionMode);
  uiState.locatorMode = Boolean(uiState.locatorMode);
  uiState.minimized = Boolean(uiState.minimized);
  uiState.coachDismissed = Boolean(uiState.coachDismissed);
  uiState.suppressedLogVisible = Boolean(uiState.suppressedLogVisible);
  uiState.lastInteractionAt = Number(uiState.lastInteractionAt) || 0;
  uiState.locatorPreferences = uiState.locatorPreferences && typeof uiState.locatorPreferences === "object"
    ? uiState.locatorPreferences
    : {};
  uiState.panelPosition = uiState.panelPosition && typeof uiState.panelPosition === "object"
      && Number.isFinite(uiState.panelPosition.left) && Number.isFinite(uiState.panelPosition.top)
    ? uiState.panelPosition
    : null;
  uiState.readinessState = ["READY", "RISKY", "BLOCKED"].includes(uiState.readinessState)
    ? uiState.readinessState
    : "READY";
  uiState.readinessWarnings = Array.isArray(uiState.readinessWarnings)
    ? uiState.readinessWarnings.slice(-20)
    : [];
  uiState.suppressedEvents = Array.isArray(uiState.suppressedEvents)
    ? uiState.suppressedEvents.slice(-50)
    : [];
  globalThis.__shaftCaptureUiState = uiState;
  const persist = () => {
    if (!topLevel) return;
    try {
      sessionStorage.setItem(STORAGE_KEY, JSON.stringify({
        paused: uiState.paused,
        stopped: uiState.stopped,
        minimized: uiState.minimized,
        coachDismissed: uiState.coachDismissed,
        suppressedLogVisible: uiState.suppressedLogVisible,
        assertionMode: uiState.assertionMode,
        locatorMode: uiState.locatorMode,
        locatorPreferences: uiState.locatorPreferences,
        panelPosition: uiState.panelPosition,
        readinessState: uiState.readinessState,
        readinessWarnings: uiState.readinessWarnings.slice(-20),
        suppressedEvents: uiState.suppressedEvents.slice(-50),
        actions: uiState.actions.slice(-80),
        pendingSignals: uiState.pendingSignals.slice(-200),
        nextId: uiState.nextId,
        instanceId: uiState.instanceId,
        currentInputActionKey: uiState.currentInputActionKey,
        lastUrl: uiState.lastUrl,
        lastInteractionAt: uiState.lastInteractionAt
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
  // Recorded steps are the source of truth on the server (CaptureSessionStore, reachable via the
  // loopback control endpoint). Page-scoped sessionStorage does not survive a cross-origin
  // navigation, so every fresh page instance re-synchronizes its visible step list from the
  // server instead of trusting only what this page happens to remember.
  let stepsSyncInFlight = false;
  const syncStepsFromServer = () => {
    if (!stepsEndpoint.url || !stepsEndpoint.token || typeof fetch !== "function" || stepsSyncInFlight) {
      return;
    }
    stepsSyncInFlight = true;
    fetch(stepsEndpoint.url + "?token=" + encodeURIComponent(stepsEndpoint.token), {method: "GET"})
      .then(response => (response.ok ? response.json() : null))
      .then(steps => {
        if (Array.isArray(steps)) applyServerSteps(steps);
      })
      .catch(() => {
        // The control endpoint may be briefly unavailable during startup or shutdown;
        // the locally persisted state remains the best-effort fallback.
      })
      .finally(() => {
        stepsSyncInFlight = false;
      });
  };
  const applyServerSteps = steps => {
    // Keyed by the same client action id the server persisted (locally announced rows carry it
    // as instanceId-id, rehydrated rows as remoteId), so a sync preserves the local row object
    // and its details (locator summary, quick-assert target, warnings) instead of replacing
    // every recorded row with a details-less skeleton after each navigation.
    const localByRemoteId = new Map(
      uiState.actions.map(item => [clientActionId(item), item]));
    const previousKeys = uiState.actions.map(clientActionId).join("|");
    const merged = steps
      .filter(step => step && step.clientActionId)
      // A soft-deleted step is gone locally but the server only learns about the delete after
      // the undo grace window; never let a sync resurrect it in the meantime.
      .filter(step => !(pendingDelete && String(step.clientActionId) === clientActionId(pendingDelete.item)))
      .sort((left, right) => Number(left.sequence || 0) - Number(right.sequence || 0))
      .map(step => {
        const existing = localByRemoteId.get(step.clientActionId);
        if (existing) {
          existing.text = text(step.description) || existing.text;
          return existing;
        }
        return {
          id: uiState.nextId++,
          remoteId: String(step.clientActionId),
          text: text(step.description) || "Captured action",
          timestamp: Date.now(),
          mergeKey: "",
          details: {}
        };
      });
    if (merged.length === 0) return;
    uiState.actions = merged.slice(-80);
    const changed = uiState.actions.map(clientActionId).join("|") !== previousKeys;
    uiState.currentInputActionKey = "";
    persist();
    renderActions();
    if (changed && previousKeys) {
      noteRefreshed("Steps refreshed from session (" + uiState.actions.length + " step"
        + (uiState.actions.length === 1 ? "" : "s") + ").");
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
  const clientActionId = item => text(item.remoteId) || (uiState.instanceId + "-" + item.id);
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
  let lastClickEmission = null;
  // Last recorded input per element: form submission re-fires "change" with a value that was
  // already recorded, which used to append a duplicate type step (issue #3426 B2).
  const lastInputEmissions = new Map();
  const emit = (kind, event, data) => {
    if (uiState.paused || uiState.stopped || uiState.locatorMode) return;
    // While the assertion wizard is open the user is configuring an assertion, not driving the
    // page: stray page clicks/keys in that window are never intended test steps (issue #3426 B3).
    if (document.getElementById("shaft-capture-assertion-panel")) return;
    const target = snapshot(event);
    if (target) {
      // Navigations observed shortly after this moment are consequences of this interaction
      // (link click, form submit, redirect chain), not navigations the user performed.
      uiState.lastInteractionAt = Date.now();
      const action = data || {};
      if (kind === "input") {
        const currentValue = String(action.value || "");
        const previous = lastInputEmissions.get(target.logicalElementId);
        if (Boolean(action.committed) && previous && previous.value === currentValue) {
          // Same value, already recorded: forward the commit so the server flushes any pending
          // input merge, but never as a new visible step (issue #3426 B2).
          send({kind, page: page(), target, data: {
            ...action,
            clientActionId: previous.clientActionId,
            stepDescription: previous.stepDescription
          }});
          return;
        }
      }
      // Root cause: one native double-click fires click(detail 1), click(detail 2), then
      // dblclick. event.detail>=2 authoritatively marks the second click as a continuation of
      // the first, so revoke the single-click row + its server step and replace it with one
      // click carrying clickCount=2, instead of recording three separate actions.
      if (kind === "click") {
        if (Number(action.clickCount || 1) >= 2
            && lastClickEmission
            && lastClickEmission.logicalElementId === target.logicalElementId
            && uiState.actions.some(existing => existing.id === lastClickEmission.item.id)) {
          deleteAction(lastClickEmission.item);
        }
        lastClickEmission = null;
      } else {
        lastClickEmission = null;
      }
      const readiness = readinessFor(kind, target);
      if (readiness) setReadiness(readiness.state, readiness.warning);
      const mergeKey = kind === "input" ? "input:" + target.logicalElementId : "";
      const description = describe(kind, target, action);
      const item = announce(description, mergeKey, {
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
        action.stepDescription = description;
      }
      if (kind === "click" && item) {
        lastClickEmission = {item, logicalElementId: target.logicalElementId};
      }
      if (kind === "input" && item) {
        lastInputEmissions.set(target.logicalElementId, {
          value: String(action.value || ""),
          clientActionId: action.clientActionId,
          stepDescription: description
        });
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
    {kind: "ELEMENT_PRESENT", label: "Element exists", needsValue: false, needsAttribute: false, booleanCheck: true},
    {kind: "ELEMENT_VISIBLE", label: "Element is visible", needsValue: false, needsAttribute: false, booleanCheck: true},
    {kind: "ELEMENT_ENABLED", label: "Element is enabled", needsValue: false, needsAttribute: false, booleanCheck: true},
    {kind: "ELEMENT_SELECTED", label: "Element is selected", needsValue: false, needsAttribute: false, booleanCheck: true},
    {kind: "TEXT_EQUALS", label: "Text equals", needsValue: true, needsAttribute: false},
    {kind: "TEXT_CONTAINS", label: "Text contains", needsValue: true, needsAttribute: false},
    {kind: "ATTRIBUTE_EQUALS", label: "Attribute equals", needsValue: true, needsAttribute: true},
    {kind: "ELEMENT_IMAGE_MATCHES", label: "Image matches reference", needsValue: false, needsAttribute: false},
    {kind: "ARIA_SNAPSHOT_MATCHES", label: "Aria snapshot matches", needsValue: true, needsAttribute: false},
    {kind: "SCREENSHOT_MATCHES", label: "Screenshot matches baseline", needsValue: false, needsAttribute: false}
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
    // The aria-snapshot baseline file name is unrelated to the element's current text/attribute
    // content, so leave the field blank instead of prefilling it with a misleading default.
    if (kind === "ARIA_SNAPSHOT_MATCHES") return "";
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
    const item = announce(summaryText, "", target ? {
      kind: "verification",
      target,
      locator: bestLocatorSummary(target),
      readiness: "READY",
      warning: ""
    } : {kind: "verification", readiness: "READY", warning: ""});
    if (item) {
      // Carrying the client action id and description lets the persisted verification event
      // surface in the server-side step list, so saved assertions rehydrate across navigations
      // exactly like recorded interactions.
      payload.clientActionId = clientActionId(item);
      payload.stepDescription = text(summaryText);
    }
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
    let expectedBooleanInput = null;
    if (catalogEntry.booleanCheck) {
      const label = document.createElement("label");
      const caption = document.createElement("span");
      caption.textContent = "Expected";
      expectedBooleanInput = document.createElement("select");
      expectedBooleanInput.name = "expectedBoolean";
      [{value: "true", label: "true"}, {value: "false", label: "false"}].forEach(option => {
        const item = document.createElement("option");
        item.value = option.value;
        item.textContent = option.label;
        expectedBooleanInput.appendChild(item);
      });
      label.append(caption, expectedBooleanInput);
      list.appendChild(label);
    }
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
      const negated = Boolean(expectedBooleanInput && expectedBooleanInput.value === "false");
      const payload = {verification: catalogEntry.kind, negated};
      if (catalogEntry.needsAttribute) {
        payload.attributeName = text(fields[0].value);
        if (!payload.attributeName) return;
      }
      if (catalogEntry.needsValue) {
        payload.expected = valueText(valueInput.value);
      }
      const name = target ? targetName(target) : "page";
      const summary = negated
        ? "Assert not " + catalogEntry.label.toLowerCase() + " on " + name
        : "Assert " + catalogEntry.label.toLowerCase() + " on " + name;
      finishAssertion(target, payload, summary);
    });
    const actions = cancelAssertionRow(closeAssertionPanel);
    actions.appendChild(confirm);
    renderAssertionPanel(catalogEntry.label, [list, actions]);
    const first = list.querySelector("select,input");
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
  // Unmissable "now click the element" affordance for assertion mode (issue #3426 B3): a
  // top-center banner plus a crosshair cursor. The tiny status chip alone was routinely missed,
  // leaving users unaware the recorder was waiting for a target click.
  const assertionBannerId = "shaft-capture-assertion-banner";
  const beginAssertionTargetHints = () => {
    document.documentElement.setAttribute("data-shaft-assertion-mode", "true");
    if (document.getElementById(assertionBannerId) || !document.body) return;
    const banner = document.createElement("div");
    banner.id = assertionBannerId;
    banner.setAttribute("data-shaft-capture-control", "true");
    banner.setAttribute("role", "status");
    banner.textContent = "Assertion: click the element you want to verify. This click is not recorded as a step. Press Esc to cancel.";
    document.body.appendChild(banner);
  };
  const endAssertionTargetHints = () => {
    document.documentElement.removeAttribute("data-shaft-assertion-mode");
    const banner = document.getElementById(assertionBannerId);
    if (banner) banner.remove();
  };
  const beginElementAssertion = () => {
    uiState.assertionMode = true;
    uiState.locatorMode = false;
    clearLocatorHighlight();
    renderLocatorPanel(null);
    beginAssertionTargetHints();
    persist();
    // The "click the target element" guidance is mode UI, not a recorded action, so it is
    // surfaced through the banner and status chip (see setStatus) rather than the action list.
    // Announcing it as an action left a permanent placeholder row that nothing ever removed.
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
    endAssertionTargetHints();
    clearLocatorHighlight();
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
        --shaft-on-primary: #ffffff;
        --shaft-shadow: 0 18px 45px rgba(24, 31, 42, .22);
        position: fixed;
        right: 16px;
        bottom: 16px;
        width: min(380px, calc(100vw - 32px));
        /* Fixed height (not max-height): the panel is bottom-anchored, so a shrink-to-fit
           height lets every appended action row push the header/toolbar buttons upward on
           screen, which can make a click land on stale coordinates right as a row appears. */
        height: min(520px, calc(100vh - 32px));
        z-index: 2147483647;
        display: flex;
        flex-direction: column;
        overflow: hidden;
        overflow-x: hidden;
        border: 1px solid rgba(var(--shaft-primary-rgb), .36);
        border-radius: 8px;
        background: var(--shaft-surface);
        color: var(--shaft-text);
        box-shadow: var(--shaft-shadow);
        font: 13px/1.35 system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      }
      /* Dark palette kept in lockstep with ReportHtmlTheme (soft consistency per #3501). */
      @media (prefers-color-scheme: dark) {
        #shaft-capture-ui {
          --shaft-primary: #4cc2ff;
          --shaft-primary-rgb: 76, 194, 255;
          --shaft-deep: #07111f;
          --shaft-deep-alt: #102a31;
          --shaft-muted: #dff5f4;
          --shaft-on-dark: #f5fdff;
          --shaft-on-primary: #07111f;
          --shaft-bg: #07111f;
          --shaft-surface: #102a31;
          --shaft-text: #f5fdff;
          --shaft-text-muted: #c8d6e7;
          --shaft-border: rgba(223, 245, 244, .24);
          --shaft-shadow: 0 18px 45px rgba(0, 0, 0, .34);
        }
      }
      #shaft-capture-ui * { box-sizing: border-box; font: inherit; letter-spacing: 0; }
      /* Minimized: only the header toolbar stays, so the page underneath is usable while
         recording continues; the title carries the live step count. */
      #shaft-capture-ui[data-minimized="true"] { height: auto; }
      #shaft-capture-ui[data-minimized="true"] .status-chip,
      #shaft-capture-ui[data-minimized="true"] #shaft-capture-warning,
      #shaft-capture-ui[data-minimized="true"] #shaft-capture-stop-confirm,
      #shaft-capture-ui[data-minimized="true"] #shaft-capture-locator-panel,
      #shaft-capture-ui[data-minimized="true"] #shaft-capture-actions,
      #shaft-capture-ui[data-minimized="true"] #shaft-capture-dialog,
      #shaft-capture-ui[data-minimized="true"] #shaft-capture-assertion-panel { display: none; }
      #shaft-capture-empty-hint {
        margin: 2px 0 0;
        color: var(--shaft-text-muted);
        font-size: 12px;
      }
      #shaft-capture-assertion-banner {
        position: fixed;
        top: 16px;
        left: 50%;
        transform: translateX(-50%);
        z-index: 2147483647;
        max-width: calc(100vw - 32px);
        padding: 10px 18px;
        border-radius: 8px;
        border: 2px solid #b7791f;
        background: #fff8e6;
        color: #17202a;
        font: 600 14px/1.4 system-ui, sans-serif;
        box-shadow: 0 6px 24px rgba(0, 0, 0, .25);
      }
      html[data-shaft-assertion-mode] body,
      html[data-shaft-assertion-mode] body *:not(#shaft-capture-ui):not(#shaft-capture-ui *) {
        cursor: crosshair !important;
      }
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
      #shaft-capture-status { gap: 6px; flex-wrap: wrap; }
      #shaft-capture-ui .status-pill {
        display: inline-flex;
        align-items: center;
        padding: 1px 8px;
        border: 1px solid var(--shaft-border);
        border-radius: 999px;
        background: var(--shaft-surface);
        font-size: 11px;
        font-weight: 700;
        white-space: nowrap;
      }
      #shaft-capture-readiness-pill[data-readiness="READY"] { border-color: var(--shaft-pass); color: var(--shaft-pass); }
      #shaft-capture-readiness-pill[data-readiness="RISKY"] { border-color: var(--shaft-warn); color: var(--shaft-warn); }
      #shaft-capture-readiness-pill[data-readiness="BLOCKED"] { border-color: var(--shaft-fail); color: var(--shaft-fail); }
      #shaft-capture-mode-pill { border-color: var(--shaft-primary); color: var(--shaft-primary); }
      #shaft-capture-mode-pill[data-mode="Stopped"],
      #shaft-capture-mode-pill[data-mode="Paused"] { border-color: var(--shaft-text-muted); color: var(--shaft-text-muted); }
      #shaft-capture-step-count { font-weight: 700; }
      #shaft-capture-warning {
        margin: 6px 10px 0;
        padding: 5px 9px;
        border: 1px solid var(--shaft-warn);
        border-radius: 6px;
        background: rgba(183, 121, 31, .12);
        color: var(--shaft-text);
        font-size: 12px;
        overflow-wrap: anywhere;
      }
      #shaft-capture-ignored-note {
        margin: 0 0 7px;
        padding: 4px 8px;
        border: 1px dashed var(--shaft-border);
        border-radius: 6px;
        color: var(--shaft-text-muted);
        font-size: 12px;
        overflow-wrap: anywhere;
      }
      #shaft-capture-sync-note {
        margin: 0 0 7px;
        padding: 4px 8px;
        border: 1px dashed var(--shaft-pass);
        border-radius: 6px;
        color: var(--shaft-text-muted);
        font-size: 12px;
        overflow-wrap: anywhere;
      }
      #shaft-capture-sync-note[hidden] { display: none; }
      #shaft-capture-coach {
        margin: 6px 10px 0;
        padding: 8px 10px;
        border: 1px solid var(--shaft-pass);
        border-radius: 6px;
        background: rgba(20, 128, 74, .10);
        color: var(--shaft-text);
        font-size: 12px;
      }
      #shaft-capture-coach[hidden] { display: none; }
      #shaft-capture-coach ul { margin: 6px 0 8px; padding-left: 18px; }
      #shaft-capture-coach li { margin: 3px 0; overflow-wrap: anywhere; }
      #shaft-capture-coach-dismiss {
        min-width: 0;
        height: auto;
        padding: 3px 12px;
        line-height: 1.2;
        font-weight: 700;
      }
      #shaft-capture-undo-note {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 8px;
        margin: 0 0 7px;
        padding: 4px 8px;
        border: 1px solid var(--shaft-warn);
        border-radius: 6px;
        background: rgba(183, 121, 31, .12);
        font-size: 12px;
      }
      #shaft-capture-undo-note[hidden] { display: none; }
      #shaft-capture-undo {
        min-width: 0;
        height: auto;
        padding: 3px 10px;
        line-height: 1.2;
        font-weight: 700;
      }
      #shaft-capture-ui .step-badge {
        display: inline-block;
        margin-right: 6px;
        padding: 0 6px;
        border: 1px solid var(--shaft-primary);
        border-radius: 999px;
        color: var(--shaft-primary);
        font-size: 10.5px;
        font-weight: 700;
        letter-spacing: .3px;
        text-transform: uppercase;
        vertical-align: 1px;
      }
      #shaft-capture-ui .step-badge.warn {
        border-color: var(--shaft-warn);
        color: var(--shaft-warn);
      }
      #shaft-capture-help {
        margin: 8px 10px 0;
        padding: 8px 10px;
        border: 1px solid var(--shaft-border);
        border-radius: 8px;
        background: var(--shaft-surface);
        font-size: 12px;
      }
      #shaft-capture-help ul { margin: 6px 0 0; padding-left: 18px; }
      #shaft-capture-help li { margin: 3px 0; overflow-wrap: anywhere; }
      #shaft-capture-help kbd {
        padding: 0 4px;
        border: 1px solid var(--shaft-border);
        border-radius: 4px;
        font-size: 11px;
      }
      #shaft-capture-ui[data-minimized="true"] #shaft-capture-help { display: none; }
      #shaft-capture-ui header { cursor: grab; touch-action: none; }
      #shaft-capture-ui header:active { cursor: grabbing; }
      #shaft-capture-ui header button { cursor: pointer; }
      #shaft-capture-help-toggle { font-weight: 700; }
      #shaft-capture-stop-confirm {
        margin: 8px 10px 0;
        padding: 9px 10px;
        border: 1px solid var(--shaft-primary);
        border-radius: 8px;
        background: rgba(var(--shaft-primary-rgb), .08);
        font-size: 12px;
      }
      #shaft-capture-stop-confirm p { margin: 6px 0; overflow-wrap: anywhere; }
      #shaft-capture-stop-confirm .stop-confirm-buttons { display: flex; gap: 8px; margin-top: 8px; }
      #shaft-capture-stop-confirm button {
        min-width: 0;
        height: auto;
        padding: 5px 10px;
        line-height: 1.2;
        font-weight: 700;
      }
      #shaft-capture-stop-confirm #shaft-capture-stop-confirm-yes {
        background: var(--shaft-primary);
        color: var(--shaft-on-primary);
        border-color: var(--shaft-primary);
      }
      #shaft-capture-actions {
        flex: 1 1 auto;
        min-height: 0;
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
        /* B5 (#3536): assert/edit/delete moved into the row overflow menu, so only up/down and
           the overflow toggle stay inline; each gets a larger 32px touch target. */
        grid-template-columns: 1fr repeat(3, 32px);
        gap: 6px;
        align-items: start;
      }
      #shaft-capture-actions .action-row button,
      #shaft-capture-actions .row-overflow-menu button {
        min-width: 32px;
        min-height: 32px;
      }
      #shaft-capture-actions li { position: relative; }
      #shaft-capture-actions .row-overflow-menu {
        position: absolute;
        right: 0;
        top: 100%;
        z-index: 5;
        display: grid;
        gap: 4px;
        margin-top: 4px;
        padding: 6px;
        border: 1px solid var(--shaft-border);
        border-radius: 8px;
        background: var(--shaft-surface);
        box-shadow: var(--shaft-shadow);
      }
      #shaft-capture-actions .row-overflow-menu[hidden] { display: none; }
      #shaft-capture-actions .row-overflow-menu button {
        width: 100%;
        justify-content: flex-start;
        gap: 6px;
        padding: 0 8px;
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
      #shaft-capture-assertion-panel input,
      #shaft-capture-assertion-panel select {
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
      /* C1 (#3536): opt-in persistent log of suppressed (transparently-ignored) navigations. */
      #shaft-capture-suppressed-toggle {
        display: block;
        width: auto;
        height: auto;
        margin: 6px 0 0;
        padding: 2px 0;
        border: none;
        background: none;
        color: var(--shaft-primary);
        font-size: 11px;
        font-weight: 700;
        text-align: left;
      }
      #shaft-capture-suppressed-toggle[hidden] { display: none; }
      #shaft-capture-suppressed-log {
        margin: 4px 0 0;
        padding: 6px 8px;
        max-height: 120px;
        overflow-y: auto;
        border: 1px dashed var(--shaft-border);
        border-radius: 6px;
        color: var(--shaft-text-muted);
        font-size: 11px;
      }
      #shaft-capture-suppressed-log[hidden] { display: none; }
      #shaft-capture-suppressed-log div { padding: 2px 0; overflow-wrap: anywhere; }
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
    // Assertion and locator flows render inside the panel body; a minimized panel would hide
    // the very UI those modes are waiting on, so entering either mode expands the panel.
    if (uiState.minimized
        && (uiState.assertionMode || uiState.locatorMode
          || document.getElementById("shaft-capture-assertion-panel"))) {
      uiState.minimized = false;
      persist();
    }
    if ((uiState.stopped || uiState.paused) && uiState.locatorMode) {
      uiState.locatorMode = false;
      clearLocatorHighlight();
      renderLocatorPanel(null);
      persist();
    }
    if ((uiState.stopped || uiState.paused) && uiState.assertionMode) {
      uiState.assertionMode = false;
      endAssertionTargetHints();
      closeAssertionPanel();
      persist();
    }
    if (!uiState.assertionMode) {
      endAssertionTargetHints();
    }
    const assertionPanelOpen = Boolean(document.getElementById("shaft-capture-assertion-panel"));
    const latestWarning = uiState.readinessWarnings[uiState.readinessWarnings.length - 1] || "";
    // Shared authoring glossary (#3496/#3501): readiness reads Ready/Risky/Blocked, the mode pill
    // reads Recording/Paused/Asserting/Picking locator/Stopped, and recorded units are "steps".
    const readinessLabel = {READY: "Ready", RISKY: "Risky", BLOCKED: "Blocked"}[uiState.readinessState] || "Ready";
    const mode = uiState.stopped
      ? {label: "Stopped", hint: "Session saved. SHAFT is closing the browser."}
      : uiState.paused
        ? {label: "Paused", hint: "Browser actions are not being captured."}
        : uiState.locatorMode
          ? {label: "Picking locator", hint: "Click an element to rank its locators."}
          : uiState.assertionMode
            ? {label: "Asserting", hint: "Click an element to capture the assertion target."}
            : assertionPanelOpen
              ? {label: "Asserting", hint: "Choose an assertion."}
              : {label: "Recording", hint: "Browser actions are captured as steps."};
    status.dataset.readiness = uiState.readinessState;
    status.title = mode.hint;
    const readinessPill = document.getElementById("shaft-capture-readiness-pill");
    const modePill = document.getElementById("shaft-capture-mode-pill");
    const stepCount = document.getElementById("shaft-capture-step-count");
    if (readinessPill && modePill && stepCount) {
      readinessPill.textContent = readinessLabel;
      readinessPill.dataset.readiness = uiState.readinessState;
      modePill.textContent = mode.label;
      modePill.dataset.mode = mode.label;
      stepCount.textContent = uiState.actions.length + (uiState.actions.length === 1 ? " step" : " steps");
    } else {
      status.textContent = readinessLabel + " | " + mode.label + " | "
        + uiState.actions.length + (uiState.actions.length === 1 ? " step" : " steps");
    }
    // Warnings coexist with the mode pill on their own line instead of replacing the mode text.
    const warningLine = document.getElementById("shaft-capture-warning");
    if (warningLine) {
      warningLine.textContent = latestWarning;
      warningLine.hidden = !latestWarning;
    }
    pause.innerHTML = icon(uiState.paused ? "play" : "pause");
    pause.title = uiState.paused ? "Resume recording" : "Pause recording";
    pause.setAttribute("aria-label", pause.title);
    assert.setAttribute("aria-pressed", String(uiState.assertionMode || assertionPanelOpen));
    assert.disabled = uiState.stopped || uiState.paused || uiState.locatorMode;
    pick.setAttribute("aria-pressed", String(uiState.locatorMode));
    pick.disabled = uiState.stopped || uiState.paused;
    stop.disabled = uiState.stopped;
    const panel = document.getElementById("shaft-capture-ui");
    if (panel) panel.dataset.minimized = String(uiState.minimized);
    const minimize = document.getElementById("shaft-capture-minimize");
    if (minimize) {
      minimize.innerHTML = icon(uiState.minimized ? "up" : "down");
      minimize.title = uiState.minimized ? "Expand the recorder panel" : "Minimize the recorder panel";
      minimize.setAttribute("aria-label", minimize.title);
      minimize.setAttribute("aria-expanded", String(!uiState.minimized));
    }
    const title = document.getElementById("shaft-capture-title");
    if (title) {
      // The step count stays visible while minimized so recording progress is never hidden.
      title.textContent = uiState.minimized
        ? "SHAFT Capture · " + uiState.actions.length
          + (uiState.actions.length === 1 ? " step" : " steps")
        : "SHAFT Capture";
    }
    const emptyHint = document.getElementById("shaft-capture-empty-hint");
    if (emptyHint) emptyHint.hidden = uiState.actions.length > 0;
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
    const summaryText = "Assert element visible on " + targetName(target);
    const created = announce(summaryText, "", {
      kind: "verification",
      target,
      locator: bestLocatorSummary(target),
      readiness: "READY",
      warning: ""
    });
    const payload = {verification: "ELEMENT_VISIBLE"};
    if (created) {
      payload.clientActionId = clientActionId(created);
      payload.stepDescription = text(summaryText);
    }
    sendVerification(target, payload);
  };
  // Soft delete with an undo grace window (#3496 B3): the row disappears immediately but the
  // step_delete signal is only sent once the window elapses, so Undo needs no server-side
  // reverse operation — the server never learned about the delete.
  const PENDING_DELETE_GRACE_MS = 5000;
  let pendingDelete = null;
  const undoNote = show => {
    const note = document.getElementById("shaft-capture-undo-note");
    if (note) note.hidden = !show;
  };
  const commitPendingDelete = () => {
    if (!pendingDelete) return;
    const committed = pendingDelete;
    clearTimeout(committed.timer);
    pendingDelete = null;
    undoNote(false);
    send({
      kind: "step_delete",
      page: page(),
      data: {
        clientActionId: clientActionId(committed.item)
      }
    });
  };
  // Expose any soft-delete still inside its undo grace window so a server-driven stop can finalize
  // it before the browser is torn down (#3560). The IDE's Stop button reaches the capture_stop tool
  // directly and never runs the in-overlay confirmStop that would otherwise commit the pending
  // delete, so without this a step deleted in the last few seconds of recording survives in the
  // session store and resurfaces in the generated JSON/code. Read-only: it never sends or mutates,
  // so the overlay's own undo-grace behaviour is untouched.
  globalThis.__shaftCapturePendingDeletes = () =>
    pendingDelete ? [clientActionId(pendingDelete.item)] : [];
  const undoDelete = () => {
    if (!pendingDelete) return;
    const restored = pendingDelete;
    clearTimeout(restored.timer);
    pendingDelete = null;
    const copy = uiState.actions.slice();
    copy.splice(Math.min(Math.max(restored.index, 0), copy.length), 0, restored.item);
    uiState.actions = copy;
    persist();
    undoNote(false);
    renderActions();
  };
  const deleteAction = item => {
    commitPendingDelete();
    const index = uiState.actions.findIndex(action => action.id === item.id);
    uiState.actions = uiState.actions.filter(action => action.id !== item.id);
    if (item.mergeKey && uiState.currentInputActionKey === item.mergeKey) {
      uiState.currentInputActionKey = "";
    }
    persist();
    pendingDelete = {item, index, timer: setTimeout(commitPendingDelete, PENDING_DELETE_GRACE_MS)};
    undoNote(true);
    renderActions();
  };
  // User-facing kind badge derived from the announced description (#3496 C2).
  const stepKind = item => {
    const value = String(item.text || "");
    if (/^(Click|Double-click) /.test(value)) return "Click";
    if (/^Type into /.test(value)) return "Type";
    if (/^Select /.test(value)) return "Select";
    if (/^Toggle /.test(value)) return "Toggle";
    if (/^Upload /.test(value)) return "Upload";
    if (/^Press /.test(value)) return "Keys";
    if (/^(Navigate to|Open) /.test(value)) return "Navigate";
    if (/^(Assert|Verify) /.test(value)) return "Assert";
    if (/^Pin /.test(value)) return "Pin";
    if (/^Stop recording/.test(value)) return "Stop";
    return "Step";
  };
  // B5 (#3536): only one row's overflow menu is open at a time; a single document-level listener
  // (installed once here, not per row) closes it on outside click or Esc so listeners never leak
  // across re-renders.
  let openRowOverflowMenu = null;
  const closeRowOverflowMenu = () => {
    if (!openRowOverflowMenu) return;
    openRowOverflowMenu.menu.hidden = true;
    openRowOverflowMenu.button.setAttribute("aria-expanded", "false");
    openRowOverflowMenu = null;
  };
  const toggleRowOverflowMenu = (button, menu) => {
    const opening = menu.hidden;
    closeRowOverflowMenu();
    if (opening) {
      menu.hidden = false;
      button.setAttribute("aria-expanded", "true");
      openRowOverflowMenu = {button, menu};
    }
  };
  document.addEventListener("click", () => closeRowOverflowMenu());
  document.addEventListener("keydown", event => {
    if (String(event.key || "") === "Escape") closeRowOverflowMenu();
  });
  function renderActions() {
    const list = document.getElementById("shaft-capture-action-list");
    if (!list) return;
    list.textContent = "";
    uiState.actions.forEach((item, index) => {
      const row = document.createElement("li");
      const body = document.createElement("div");
      body.className = "action-row";
      const textWrap = document.createElement("span");
      const details = item.details || {};
      const badge = document.createElement("span");
      badge.className = "step-badge" + (details.warning ? " warn" : "");
      badge.textContent = stepKind(item);
      textWrap.appendChild(badge);
      const labelNode = document.createElement("span");
      labelNode.textContent = item.text;
      textWrap.appendChild(labelNode);
      const detail = document.createElement("small");
      detail.textContent = [details.locator, details.warning].filter(Boolean).join(" | ");
      if (detail.textContent) textWrap.appendChild(detail);
      const assert = document.createElement("button");
      assert.type = "button";
      assert.innerHTML = icon("assert");
      assert.title = "Add visible assertion for this target";
      assert.setAttribute("aria-label", "Add visible assertion for this target");
      assert.setAttribute("role", "menuitem");
      assert.disabled = !details.target;
      assert.addEventListener("click", () => {
        closeRowOverflowMenu();
        quickAssertion(item);
      });
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
      edit.setAttribute("role", "menuitem");
      edit.addEventListener("click", () => {
        closeRowOverflowMenu();
        editAction(item);
      });
      const remove = document.createElement("button");
      remove.type = "button";
      remove.innerHTML = icon("delete");
      remove.title = "Delete captured action";
      remove.setAttribute("aria-label", "Delete captured action");
      remove.setAttribute("role", "menuitem");
      remove.addEventListener("click", () => {
        closeRowOverflowMenu();
        deleteAction(item);
      });
      // B5 (#3536): secondary actions (assert/edit/delete) live in the overflow menu; up/down
      // stay inline so reordering never needs an extra click.
      const overflow = document.createElement("button");
      overflow.type = "button";
      overflow.className = "row-overflow-toggle";
      overflow.textContent = "⋯";
      overflow.title = "More actions";
      overflow.setAttribute("aria-label", "More actions");
      overflow.setAttribute("aria-haspopup", "menu");
      overflow.setAttribute("aria-expanded", "false");
      const menu = document.createElement("div");
      menu.className = "row-overflow-menu";
      menu.setAttribute("role", "menu");
      menu.hidden = true;
      menu.append(assert, edit, remove);
      overflow.addEventListener("click", event => {
        event.stopPropagation();
        toggleRowOverflowMenu(overflow, menu);
      });
      body.append(textWrap, up, down, overflow);
      row.append(body, menu);
      list.appendChild(row);
    });
    setStatus();
    maybeShowCoach();
    renderSuppressedLog();
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
  // The stop confirmation renders BEFORE the STOP control is sent: once STOP reaches the server
  // the browser closes almost immediately, so any "what happens next" guidance must be read
  // pre-stop. Programmatic stops (MCP capture_stop) intentionally keep closing right away.
  const sessionEndpointUrl = () =>
    stepsEndpoint.url ? String(stepsEndpoint.url).replace(/\/steps($|\?)/, "/session$1") : "";
  const closeStopConfirm = () => {
    const confirmPanel = document.getElementById("shaft-capture-stop-confirm");
    if (confirmPanel) {
      confirmPanel.hidden = true;
      confirmPanel.innerHTML = "";
    }
    // Restore the first-run coach once the dialog closes (#3536).
    maybeShowCoach();
  };
  const confirmStop = () => {
    commitPendingDelete();
    closeStopConfirm();
    uiState.stopped = true;
    lastClickEmission = null;
    persist();
    announce("Stop recording");
    sendControl("STOP");
    setStatus();
  };
  const renderStopConfirm = () => {
    const confirmPanel = document.getElementById("shaft-capture-stop-confirm");
    if (!confirmPanel) return;
    if (uiState.minimized) {
      uiState.minimized = false;
      persist();
      setStatus();
    }
    confirmPanel.hidden = false;
    confirmPanel.innerHTML = `
      <strong>Stop recording?</strong>
      <p id="shaft-capture-stop-confirm-path">Your recorded steps are saved as a session.</p>
      <p>Next: open the SHAFT Assistant in your IDE and use <strong>Review code</strong> to turn this session into a test.</p>
      <div class="stop-confirm-buttons">
        <button id="shaft-capture-stop-confirm-yes" type="button">Save &amp; close</button>
        <button id="shaft-capture-stop-confirm-no" type="button">Keep recording</button>
      </div>`;
    document.getElementById("shaft-capture-stop-confirm-yes").addEventListener("click", confirmStop);
    document.getElementById("shaft-capture-stop-confirm-no").addEventListener("click", closeStopConfirm);
    // Free vertical space for the dialog's buttons on a first session (#3536): the coach is
    // orientation noise while a stop decision is on screen, and the fixed-height panel cannot
    // stack the labeled header, the coach, and the dialog at once without clipping.
    maybeShowCoach();
    const sessionUrl = sessionEndpointUrl();
    if (sessionUrl && stepsEndpoint.token && typeof fetch === "function") {
      fetch(sessionUrl + "?token=" + encodeURIComponent(stepsEndpoint.token), {method: "GET"})
        .then(response => (response.ok ? response.json() : null))
        .then(info => {
          const pathLine = document.getElementById("shaft-capture-stop-confirm-path");
          if (pathLine && info && text(info.outputPath)) {
            // Incomplete-session recovery copy (#3510 C4): if the server marked the session
            // INCOMPLETE (it ended before a clean stop), reassure the user that everything captured
            // so far was still saved and is safe to review, rather than implying a clean save.
            const incomplete = text(info.state).toUpperCase() === "INCOMPLETE";
            pathLine.textContent = incomplete
              ? "This recording was interrupted, but every step captured so far was saved to: "
                + text(info.outputPath) + " — review it or start a new recording; nothing was lost."
              : "Session saved to: " + text(info.outputPath);
          }
        })
        .catch(() => {
          // Loopback fetches from the recorded page are best-effort (see the steps-sync design);
          // the generic saved-session copy already covers the endpoint being unavailable.
        });
    }
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
        <strong id="shaft-capture-title">SHAFT Capture</strong>
        <button id="shaft-capture-pause" type="button"></button>
        <button id="shaft-capture-assert" type="button" title="Add assertion" aria-label="Add assertion" aria-pressed="false">${icon("assert")}</button>
        <button id="shaft-capture-pick" type="button" title="Toggle locator picker" aria-label="Toggle locator picker" aria-pressed="false">${icon("locator")}</button>
        <button id="shaft-capture-stop" type="button" title="Stop recording" aria-label="Stop recording">${icon("stop")}</button>
        <button id="shaft-capture-help-toggle" type="button" title="Show recorder help" aria-label="Show recorder help" aria-expanded="false">?</button>
        <button id="shaft-capture-minimize" type="button" aria-expanded="true"></button>
      </header>
      <div id="shaft-capture-status" class="status-chip shaft-capture-readiness">
        <span id="shaft-capture-readiness-pill" class="status-pill"></span>
        <span id="shaft-capture-mode-pill" class="status-pill"></span>
        <span id="shaft-capture-step-count"></span>
      </div>
      <div id="shaft-capture-warning" hidden></div>
      <div id="shaft-capture-coach" hidden>
        <strong>New here? 3 quick tips</strong>
        <ul>
          <li>Every click, type, and navigation becomes an editable <strong>step</strong> below.</li>
          <li>Use <strong>Add assertion</strong> to verify an element, and <strong>Pick locator</strong> to rank locators &mdash; neither records a step.</li>
          <li><strong>Drag this header</strong> to move the panel; <strong>Stop</strong> saves the session and shows where.</li>
        </ul>
        <button id="shaft-capture-coach-dismiss" type="button">Got it</button>
      </div>
      <div id="shaft-capture-help" hidden>
        <strong>Recorder controls</strong>
        <ul>
          <li><strong>Pause / Resume</strong> &mdash; stop capturing browser actions temporarily.</li>
          <li><strong>Add assertion</strong> &mdash; verify an element or the page; the picking click is not recorded. <kbd>Esc</kbd> cancels.</li>
          <li><strong>Pick locator</strong> &mdash; rank locators for any element without recording a step. <kbd>Esc</kbd> cancels.</li>
          <li><strong>Stop</strong> &mdash; asks for confirmation, shows where the session is saved, then closes the browser.</li>
          <li><strong>Move the panel</strong> &mdash; drag this header; the position is remembered for this session.</li>
          <li><strong>Deleted a step by mistake?</strong> &mdash; use Undo within 5 seconds.</li>
        </ul>
      </div>
      <div id="shaft-capture-stop-confirm" hidden></div>
      <div id="shaft-capture-locator-panel" hidden></div>
      <div id="shaft-capture-actions"><p id="shaft-capture-empty-hint" hidden>No steps yet. Click, type, and navigate in this page &mdash; every action is recorded here as an editable step.</p><div id="shaft-capture-ignored-note" hidden></div><div id="shaft-capture-sync-note" hidden></div><div id="shaft-capture-undo-note" hidden><span>Step deleted.</span><button id="shaft-capture-undo" type="button">Undo</button></div><ol id="shaft-capture-action-list"></ol><button id="shaft-capture-suppressed-toggle" type="button" hidden aria-expanded="false"></button><div id="shaft-capture-suppressed-log" role="log" hidden></div></div>
    `;
    document.body.appendChild(panel);
    document.getElementById("shaft-capture-pause").addEventListener("click", () => {
      if (uiState.stopped) return;
      uiState.paused = !uiState.paused;
      lastClickEmission = null;
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
      if (document.getElementById("shaft-capture-stop-confirm-yes")) {
        closeStopConfirm();
        return;
      }
      renderStopConfirm();
    });
    document.getElementById("shaft-capture-minimize").addEventListener("click", () => {
      uiState.minimized = !uiState.minimized;
      persist();
      setStatus();
    });
    document.getElementById("shaft-capture-help-toggle").addEventListener("click", () => {
      const help = document.getElementById("shaft-capture-help");
      const toggle = document.getElementById("shaft-capture-help-toggle");
      if (!help || !toggle) return;
      help.hidden = !help.hidden;
      toggle.setAttribute("aria-expanded", String(!help.hidden));
    });
    document.getElementById("shaft-capture-undo").addEventListener("click", undoDelete);
    const coachDismiss = document.getElementById("shaft-capture-coach-dismiss");
    if (coachDismiss) {
      coachDismiss.addEventListener("click", () => {
        uiState.coachDismissed = true;
        persist();
        const coach = document.getElementById("shaft-capture-coach");
        if (coach) coach.hidden = true;
      });
    }
    // C1 (#3536): toggling the suppressed-events log is opt-in and stays persisted for the session.
    const suppressedToggle = document.getElementById("shaft-capture-suppressed-toggle");
    if (suppressedToggle) {
      suppressedToggle.addEventListener("click", () => {
        uiState.suppressedLogVisible = !uiState.suppressedLogVisible;
        persist();
        renderSuppressedLog();
      });
    }
    installPanelDrag(panel);
    if (uiState.panelPosition) {
      applyPanelPosition(panel, uiState.panelPosition.left, uiState.panelPosition.top);
    }
    // Keep the panel on-screen after a viewport resize (#3510): a remembered position that fit a
    // large window can strand the panel off-screen once the window shrinks, so re-clamp it.
    addEventListener("resize", () => {
      if (!uiState.panelPosition) return;
      uiState.panelPosition = applyPanelPosition(panel, uiState.panelPosition.left, uiState.panelPosition.top);
      persist();
    });
    maybeShowCoach();
    renderSuppressedLog();
    renderActions();
  };
  // #3536: the panel is a fixed-height flex column (overflow:hidden), so first-run orientation
  // chrome and a modal sub-panel compete for the same vertical space. When the stop confirmation
  // is open, suppress the coach so its buttons are never pushed out of the interactable area.
  const modalSubPanelOpen = () => {
    const stop = document.getElementById("shaft-capture-stop-confirm");
    return Boolean(stop && !stop.hidden);
  };
  // First-run coach marks (#3510 A3): a dismissible, session-scoped set of at-most-three tips shown
  // until the user dismisses them, so a first-time user is oriented without nagging returning users
  // (the "seen" flag rides the same top-frame sessionStorage as the rest of the UI state). The count
  // is not gated on an empty step list because a fresh recording already carries the initial
  // "Open ..." breadcrumb step.
  function maybeShowCoach() {
    const coach = document.getElementById("shaft-capture-coach");
    if (!coach) return;
    coach.hidden = !(topLevel && !uiState.coachDismissed && !uiState.stopped && !modalSubPanelOpen());
  }
  // Draggable panel (#3496 B1): the header is the drag handle; the remembered position is
  // page-session-scoped via the same top-frame-only persisted UI state as everything else.
  const applyPanelPosition = (panel, left, top) => {
    const width = panel.offsetWidth || 380;
    const height = panel.offsetHeight || 520;
    const clampedLeft = Math.min(Math.max(left, 0), Math.max(0, innerWidth - width));
    const clampedTop = Math.min(Math.max(top, 0), Math.max(0, innerHeight - height));
    panel.style.left = clampedLeft + "px";
    panel.style.top = clampedTop + "px";
    panel.style.right = "auto";
    panel.style.bottom = "auto";
    return {left: clampedLeft, top: clampedTop};
  };
  const installPanelDrag = panel => {
    const header = panel.querySelector("header");
    if (!header) return;
    let dragStart = null;
    header.addEventListener("pointerdown", event => {
      // Toolbar buttons keep their click behavior; only the header surface drags.
      if (event.target.closest("button")) return;
      const rect = panel.getBoundingClientRect();
      dragStart = {offsetX: event.clientX - rect.left, offsetY: event.clientY - rect.top};
      header.setPointerCapture(event.pointerId);
    });
    header.addEventListener("pointermove", event => {
      if (!dragStart) return;
      applyPanelPosition(panel, event.clientX - dragStart.offsetX, event.clientY - dragStart.offsetY);
    });
    const endDrag = event => {
      if (!dragStart) return;
      dragStart = null;
      try {
        header.releasePointerCapture(event.pointerId);
      } catch (ignored) {
        // The capture may already be gone (e.g. pointercancel).
      }
      const rect = panel.getBoundingClientRect();
      uiState.panelPosition = applyPanelPosition(panel, rect.left, rect.top);
      persist();
    };
    header.addEventListener("pointerup", endDrag);
    header.addEventListener("pointercancel", endDrag);
  };
  const schedulePanel = () => {
    renderPanel();
    if (!document.body) {
      setTimeout(schedulePanel, 50);
    }
  };
  // Only the top-level frame polls for navigations: a subframe URL change is never a user
  // navigation step, and announcing it produced phantom "Navigate to" rows (#3432).
  // Navigations observed within this window after a recorded interaction are consequences of
  // that interaction (link click, form submit, server redirect, history rewrite) — announcing
  // them created "Navigate to" rows for navigations the user never performed, e.g. a search
  // redirect landing on the results page counted as two extra navigation actions.
  const INTERACTION_NAVIGATION_WINDOW_MS = 10000;
  // Announces a user-performed navigation and reports it to the server with this row's client
  // action id, so the persisted NavigationEvent carries the row's identity: the row then
  // survives server step syncs and stays editable/deletable like every other step. Sources:
  // "user_reported" (URL poll), "user_traversal" (back/forward, which the interaction window
  // must never swallow), and "user_annotation" (the initial "Open" breadcrumb, which only tags
  // the already-recorded initial OPEN event and must never append a second one).
  // Suppression transparency (#3496): when a navigation is treated as a consequence of the last
  // recorded step (and therefore NOT recorded as its own step), say so briefly instead of
  // silently dropping it, so users trust that "missing" rows are deliberate.
  let ignoredNoteTimer = null;
  // C1 (#3536): renders the toggle button (always, so its "(N)" count stays live even while the
  // log itself is collapsed) and the log body (only while uiState.suppressedLogVisible).
  function renderSuppressedLog() {
    const toggle = document.getElementById("shaft-capture-suppressed-toggle");
    const log = document.getElementById("shaft-capture-suppressed-log");
    if (!toggle || !log) return;
    const count = uiState.suppressedEvents.length;
    toggle.hidden = !(topLevel && count > 0);
    toggle.textContent = (uiState.suppressedLogVisible ? "Hide" : "Show") + " suppressed events (" + count + ")";
    toggle.setAttribute("aria-expanded", String(uiState.suppressedLogVisible));
    log.hidden = !uiState.suppressedLogVisible;
    if (!uiState.suppressedLogVisible) return;
    log.textContent = "";
    uiState.suppressedEvents.slice().reverse().forEach(event => {
      const line = document.createElement("div");
      // textContent, never innerHTML: reason strings come from page-observed navigation state.
      line.textContent = new Date(Number(event.timestamp) || 0).toLocaleTimeString([], {
        hour: "2-digit",
        minute: "2-digit"
      }) + " — " + String(event.reason || "");
      log.appendChild(line);
    });
  }
  const noteIgnored = reason => {
    const note = document.getElementById("shaft-capture-ignored-note");
    if (!note) return;
    note.textContent = "Ignored: " + reason;
    note.hidden = false;
    if (ignoredNoteTimer) clearTimeout(ignoredNoteTimer);
    ignoredNoteTimer = setTimeout(() => {
      note.hidden = true;
    }, 6000);
    // Persistent opt-in log (#3536 C1): the transient note above still auto-hides, but the toggle
    // count is refreshed unconditionally (not only "if visible") so it never goes stale between
    // renderActions()/renderPanel() calls.
    uiState.suppressedEvents.push({reason: String(reason), timestamp: Date.now()});
    uiState.suppressedEvents = uiState.suppressedEvents.slice(-50);
    persist();
    renderSuppressedLog();
  };
  // Server-sync transparency (#3510 C3): when a background re-sync from the durable session changes
  // the visible step list (a cross-origin navigation rehydrated rows this page had not seen), say
  // so briefly instead of letting the list silently rewrite itself.
  let syncNoteTimer = null;
  function noteRefreshed(message) {
    const note = document.getElementById("shaft-capture-sync-note");
    if (!note) return;
    note.textContent = message;
    note.hidden = false;
    if (syncNoteTimer) clearTimeout(syncNoteTimer);
    syncNoteTimer = setTimeout(() => {
      note.hidden = true;
    }, 6000);
  }
  const reportNavigation = (source, breadcrumb) => {
    if (uiState.stopped || uiState.paused) return;
    uiState.lastUrl = String(location.href || "");
    const description = (breadcrumb ? "Open " : "Navigate to ") + visibleLocation();
    const lastAction = uiState.actions[uiState.actions.length - 1];
    const data = {action: "OPEN", navigationSource: source};
    // Redirect hops with the query stripped read identically; one row is the truth.
    if (!(lastAction && lastAction.text === description)) {
      if (!breadcrumb) {
        setReadiness("RISKY", "Step " + uiState.nextId + " needs a follow-up assertion after navigation.");
      }
      const item = announce(description);
      if (item) {
        data.clientActionId = clientActionId(item);
        data.stepDescription = item.text;
      }
      lastClickEmission = null;
    }
    send({kind: "navigation", page: page(), data});
    persist();
  };
  if (topLevel) {
    setInterval(() => {
      const current = String(location.href || "");
      if (current === uiState.lastUrl) return;
      uiState.lastUrl = current;
      if (!uiState.stopped && !uiState.paused) {
        const sinceInteraction = Date.now() - uiState.lastInteractionAt;
        if (sinceInteraction > INTERACTION_NAVIGATION_WINDOW_MS) {
          reportNavigation("user_reported", false);
        } else {
          noteIgnored("navigation caused by your last step, kept as part of that step.");
        }
        lastClickEmission = null;
      }
      syncStepsFromServer();
      persist();
    }, 500);
    // Back/forward is a navigation the user performed, even right after an interaction, so it
    // bypasses the interaction window. Same-document traversals fire popstate. Cross-document
    // traversals are detected on pageshow — the one event that fires for both a back/forward
    // cache restore (persisted=true) and a fresh document, and late enough that the navigation
    // timing entry (absent at preload time) reliably reports "back_forward".
    addEventListener("popstate", () => reportNavigation("user_traversal", false), true);
    addEventListener("pageshow", event => {
      const freshTraversal = (() => {
        try {
          const entry = performance.getEntriesByType
            && performance.getEntriesByType("navigation")[0];
          return Boolean(entry && entry.type === "back_forward");
        } catch (ignored) {
          return false;
        }
      })();
      if (!event.persisted && !freshTraversal) return;
      if (event.persisted) {
        // A bfcache restore resumes this page with its pre-navigation in-memory state; newer
        // rows recorded on the page the user came back from live in shared storage.
        const stored = persisted();
        if (stored && typeof stored === "object" && Array.isArray(stored.actions)) {
          Object.assign(uiState, stored);
          uiState.actions = stored.actions.slice(-80);
          renderActions();
        }
      }
      if (uiState.actions.length > 0) {
        // A first-ever page skips this — its "Open" breadcrumb already covers the arrival.
        reportNavigation("user_traversal", false);
      }
    }, true);
  }

  addEventListener("mousemove", event => {
    // Assertion mode shares the locator hover highlight so the element about to be picked is
    // visibly outlined before the click (issue #3426 B3).
    if ((!uiState.locatorMode && !uiState.assertionMode) || uiState.stopped || uiState.paused) return;
    updateLocatorHighlight(eventElement(event));
  }, true);
  addEventListener("click", event => {
    if (captureLocatorPick(event)) return;
    if (captureAssertion(event)) return;
    const element = event.composedPath ? event.composedPath()[0] : event.target;
    const type = String(element && element.type || "").toLowerCase();
    const tag = String(element && element.localName || "").toLowerCase();
    if (["checkbox", "radio", "file"].includes(type) || tag === "select" || tag === "option") return;
    // A user cannot click an element that has no rendered box: such clicks are browser-synthesized
    // (pressing Enter in a form "clicks" its hidden default submit button) and recording them adds
    // phantom steps the user never performed (issue #3426 B2).
    if (element && element.getClientRects && element.getClientRects().length === 0) return;
    // Bare-root clicks (body/html) are focus/dismiss noise, never a meaningful test step, and
    // their accessible name would swallow the entire page text (issue #3426 B3).
    if (tag === "body" || tag === "html") return;
    emit("click", event, {
      button: Number(event.button || 0),
      clickCount: Number(event.detail || 1)
    });
  }, true);
  addEventListener("dblclick", event => {
    // A native dblclick is always preceded by a click with detail=2, which the click listener
    // above already emits with clickCount=2 (and coalesces with the preceding single click via
    // lastClickEmission), so emitting again here would duplicate the action. Only the locator-pick
    // guard still applies.
    captureLocatorPick(event);
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
    if (uiState.assertionMode && String(event.key || "") === "Escape") {
      uiState.assertionMode = false;
      endAssertionTargetHints();
      clearLocatorHighlight();
      persist();
      setStatus();
      event.preventDefault();
      event.stopImmediatePropagation();
      return;
    }
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

  if (topLevel) {
    schedulePanel();
    if (uiState.actions.length === 0 && stepsEndpoint.url && stepsEndpoint.token) {
      // A fresh page (first load, or after a cross-origin navigation that reset page-scoped
      // storage) waits for the authoritative server step list before showing a local-only
      // "Open ..." breadcrumb, so a mid-session page does not briefly flash an empty state.
      syncStepsFromServer();
      setTimeout(() => {
        if (uiState.actions.length === 0) {
          reportNavigation("user_annotation", true);
        }
      }, 400);
    } else {
      syncStepsFromServer();
      if (uiState.actions.length === 0) {
        reportNavigation("user_annotation", true);
      }
    }
  }
}
