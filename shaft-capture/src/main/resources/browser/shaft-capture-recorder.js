(channel) => {
  if (globalThis.__shaftCaptureInstalled) {
    return;
  }
  globalThis.__shaftCaptureInstalled = true;
  globalThis.__shaftCaptureQueue = globalThis.__shaftCaptureQueue || [];

  const testIdAttributes = ["data-testid", "data-test", "data-qa"];
  const STORAGE_KEY = "shaft.capture.recorder.ui";
  const text = value => String(value || "").replace(/\s+/g, " ").trim().slice(0, 500);
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
    actions: [],
    nextId: 1,
    lastUrl: String(location.href || ""),
    ...persisted()
  };
  uiState.actions = Array.isArray(uiState.actions) ? uiState.actions.slice(-80) : [];
  uiState.nextId = Number(uiState.nextId || uiState.actions.length + 1);
  globalThis.__shaftCaptureUiState = uiState;
  const persist = () => {
    try {
      sessionStorage.setItem(STORAGE_KEY, JSON.stringify({
        paused: uiState.paused,
        stopped: uiState.stopped,
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
  const snapshot = event => {
    const path = event.composedPath ? event.composedPath() : [];
    const element = path.find(item => item && item.nodeType === Node.ELEMENT_NODE) || event.target;
    if (!element || element.nodeType !== Node.ELEMENT_NODE) return null;
    if (element.closest && element.closest("[data-shaft-capture-control]")) return null;
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
    return {
      logicalElementId: logical,
      tagName: String(element.localName || "").toLowerCase(),
      role: inferredRole(element),
      accessibleName: accessibleName(element),
      label: label(element),
      attributes,
      locators: locators(element),
      visible: Boolean(element.getClientRects && element.getClientRects().length),
      enabled: !Boolean(element.disabled),
      selected: Boolean(element.checked || element.selected)
    };
  };
  const emit = (kind, event, data) => {
    if (uiState.paused || uiState.stopped) return;
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

  const styles = () => {
    if (document.getElementById("shaft-capture-ui-style")) return;
    const style = document.createElement("style");
    style.id = "shaft-capture-ui-style";
    style.textContent = `
      #shaft-capture-ui {
        position: fixed;
        right: 16px;
        bottom: 16px;
        width: min(380px, calc(100vw - 32px));
        max-height: min(520px, calc(100vh - 32px));
        z-index: 2147483647;
        display: flex;
        flex-direction: column;
        overflow: hidden;
        border: 1px solid #0f172a;
        border-radius: 8px;
        background: #ffffff;
        color: #111827;
        box-shadow: 0 12px 32px rgba(15, 23, 42, .28);
        font: 13px/1.35 system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      }
      #shaft-capture-ui * { box-sizing: border-box; font: inherit; letter-spacing: 0; }
      #shaft-capture-ui header {
        display: flex;
        align-items: center;
        gap: 8px;
        padding: 8px 10px;
        background: #0f172a;
        color: #ffffff;
      }
      #shaft-capture-ui strong { flex: 1; font-weight: 700; }
      #shaft-capture-ui button {
        min-width: 28px;
        height: 28px;
        border: 1px solid #cbd5e1;
        border-radius: 6px;
        background: #f8fafc;
        color: #0f172a;
        cursor: pointer;
        font-weight: 700;
      }
      #shaft-capture-ui button:hover { background: #e0f2fe; }
      #shaft-capture-ui button:disabled { cursor: default; opacity: .5; }
      #shaft-capture-status {
        padding: 6px 10px;
        border-bottom: 1px solid #e5e7eb;
        color: #334155;
        background: #f8fafc;
      }
      #shaft-capture-actions {
        overflow: auto;
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
    `;
    (document.head || document.documentElement).appendChild(style);
  };
  const setStatus = () => {
    const status = document.getElementById("shaft-capture-status");
    const pause = document.getElementById("shaft-capture-pause");
    const stop = document.getElementById("shaft-capture-stop");
    if (!status || !pause || !stop) return;
    status.textContent = uiState.stopped
      ? "Stopped. Waiting for SHAFT to close the browser."
      : uiState.paused
        ? "Paused. Browser actions are not being captured."
        : "Recording browser actions.";
    pause.textContent = uiState.paused ? ">" : "||";
    pause.title = uiState.paused ? "Resume recording" : "Pause recording";
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
      edit.textContent = "edit";
      edit.title = "Edit captured action";
      edit.addEventListener("click", () => editAction(item));
      body.append(labelNode, edit);
      row.append(body);
      list.appendChild(row);
    });
    setStatus();
  }
  const renderPanel = () => {
    if (!document.body || document.getElementById("shaft-capture-ui")) return;
    styles();
    const panel = document.createElement("section");
    panel.id = "shaft-capture-ui";
    panel.setAttribute("data-shaft-capture-control", "true");
    panel.innerHTML = `
      <header>
        <strong>SHAFT Capture</strong>
        <button id="shaft-capture-pause" type="button"></button>
        <button id="shaft-capture-checkpoint" type="button" title="Add checkpoint">+</button>
        <button id="shaft-capture-stop" type="button" title="Stop recording">x</button>
      </header>
      <div id="shaft-capture-status"></div>
      <div id="shaft-capture-actions"><ol id="shaft-capture-action-list"></ol></div>
    `;
    document.body.appendChild(panel);
    document.getElementById("shaft-capture-pause").addEventListener("click", () => {
      if (uiState.stopped) return;
      uiState.paused = !uiState.paused;
      persist();
      sendControl(uiState.paused ? "PAUSE" : "RESUME");
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

  addEventListener("click", event => {
    const element = event.composedPath ? event.composedPath()[0] : event.target;
    const type = String(element && element.type || "").toLowerCase();
    const tag = String(element && element.localName || "").toLowerCase();
    if (["checkbox", "radio", "file"].includes(type) || tag === "select" || tag === "option") return;
    emit("click", event, {
      button: Number(event.button || 0),
      clickCount: Number(event.detail || 1)
    });
  }, true);
  addEventListener("dblclick", event => emit("click", event, {
    button: Number(event.button || 0),
    clickCount: 2
  }), true);
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
