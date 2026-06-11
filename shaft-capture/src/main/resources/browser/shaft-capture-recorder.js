(channel) => {
  if (globalThis.__shaftCaptureInstalled) {
    return;
  }
  globalThis.__shaftCaptureInstalled = true;
  globalThis.__shaftCaptureQueue = globalThis.__shaftCaptureQueue || [];

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
    ["data-testid", "data-test", "data-qa"].forEach(attribute => {
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
    const target = snapshot(event);
    if (target) send({kind, page: page(), target, data: data || {}});
  };
  const isTextInput = element => {
    const tag = String(element && element.localName || "").toLowerCase();
    const type = String(element && element.type || "text").toLowerCase();
    return tag === "textarea" || (tag === "input" &&
      !["button", "submit", "reset", "checkbox", "radio", "file", "hidden"].includes(type));
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
}
