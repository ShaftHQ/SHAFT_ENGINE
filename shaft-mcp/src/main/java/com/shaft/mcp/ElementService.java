package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

import static com.shaft.mcp.EngineService.getDriver;
import static com.shaft.mcp.EngineService.getLocator;

/**
 * MCP-safe element actions. Sensitive values are returned only to direct callers and are never logged.
 */
@Service
public class ElementService {
    private static final Logger logger = LoggerFactory.getLogger(ElementService.class);
    private final PlaywrightService playwrightService;
    private final MobileService mobileService;

    /**
     * Creates the default element service, wiring default Playwright/mobile services for direct
     * Java callers and tests that construct this class without Spring.
     */
    public ElementService() {
        this(new PlaywrightService(), new MobileService(new EngineService()));
    }

    @Autowired
    ElementService(PlaywrightService playwrightService, MobileService mobileService) {
        this.playwrightService = playwrightService;
        this.mobileService = mobileService;
    }

    /**
     * Hovers over an element, dispatching to whichever engine is currently active. Mobile engines
     * (MOBILE_NATIVE/MOBILE_WEB) share the same WebDriver-backed session as WEB and use the same
     * code path; only Playwright uses a distinct driver instance.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @return the active engine and, when a Playwright recording is active, the recorded step's code block
     */
    @Tool(name = "element_hover", description = "hovers over an element; dispatches to the active engine and "
            + "records the step when a Playwright recording is active")
    public ElementActionResult hover(locatorStrategy locatorStrategy, String locatorValue) {
        ActiveEngine engine = EngineService.activeEngine();
        if (engine == ActiveEngine.PLAYWRIGHT) {
            return fromDispatchResult(engine, playwrightService.hover(locatorStrategy, locatorValue));
        }
        withLocator(locatorStrategy, locatorValue, "hover", locator -> getDriver().element().hover(locator));
        return new ElementActionResult(engine.name(), false, null, List.of());
    }

    /**
     * Clicks an element, dispatching to whichever engine is currently active (web/mobile WebDriver
     * or Playwright) and routing through that engine's recording wrapper when a mobile or Playwright
     * recording is active.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @param mode click gesture; blank/omitted defaults to {@link ClickMode#SINGLE}. {@link ClickMode#DOUBLE}
     *             and {@link ClickMode#LONG} absorb the former element_double_click/element_click_and_hold/
     *             mobile_double_tap/mobile_long_tap tools; {@link ClickMode#LONG} is not supported on the
     *             Playwright engine.
     * @return the active engine and, when a recording is active, the recorded step's code block
     */
    @Tool(name = "element_click", description = "clicks an element; optional mode selects single (default) | "
            + "double | long; dispatches to the active engine and records the step when a mobile or "
            + "Playwright recording is active")
    public ElementActionResult click(
            locatorStrategy locatorStrategy,
            String locatorValue,
            @ToolParam(required = false) ClickMode mode) {
        ClickMode resolvedMode = mode == null ? ClickMode.SINGLE : mode;
        ActiveEngine engine = EngineService.activeEngine();
        return switch (engine) {
            case PLAYWRIGHT -> fromDispatchResult(engine,
                    playwrightService.dispatchClick(locatorStrategy, locatorValue, resolvedMode));
            case MOBILE_NATIVE, MOBILE_WEB -> fromDispatchResult(engine,
                    mobileService.dispatchClick(locatorStrategy, locatorValue, resolvedMode));
            case WEB, NONE -> {
                webClick(locatorStrategy, locatorValue, resolvedMode);
                yield new ElementActionResult(engine.name(), false, null, List.of());
            }
        };
    }

    /**
     * Java-caller convenience overload defaulting {@code mode} to {@link ClickMode#SINGLE}; not an
     * MCP tool.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @return the active engine and, when a recording is active, the recorded step's code block
     */
    public ElementActionResult click(locatorStrategy locatorStrategy, String locatorValue) {
        return click(locatorStrategy, locatorValue, null);
    }

    private static void webClick(locatorStrategy locatorStrategy, String locatorValue, ClickMode mode) {
        withLocator(locatorStrategy, locatorValue, "click", locator -> {
            switch (mode) {
                case DOUBLE -> getDriver().element().doubleClick(locator);
                case LONG -> getDriver().element().clickAndHold(locator);
                case SINGLE -> getDriver().element().click(locator);
            }
        });
    }

    private static ElementActionResult fromDispatchResult(ActiveEngine engine, McpMobileActionResult result) {
        return new ElementActionResult(engine.name(), result.recorded(), result.codeBlock(), result.warnings());
    }

    /**
     * Clicks an element using SHAFT semantic locator behavior for direct Java callers.
     * Not exposed as an MCP tool because semantic names may contain sensitive application text.
     *
     * @param elementName semantic element name
     */
    public void clickSemantic(String elementName) {
        try {
            getDriver().element().click(elementName);
            logger.info("Semantic click completed (name length: {})", safeLength(elementName));
        } catch (Exception exception) {
            logger.error("Semantic click failed (name redacted)", exception);
            throw exception;
        }
    }

    /**
     * Compatibility alias for older clients; not exposed as an MCP tool.
     *
     * @param elementName semantic element name
     */
    @Deprecated(forRemoval = false)
    public void clickUsingAI(String elementName) {
        clickSemantic(elementName);
    }

    /**
     * Clicks an element using JavaScript.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     */
    @Tool(name = "element_click_js", description = "clicks an element using JavaScript")
    public void clickUsingJavaScript(locatorStrategy locatorStrategy, String locatorValue) {
        withLocator(locatorStrategy, locatorValue, "javascript-click",
                locator -> getDriver().element().clickUsingJavascript(locator));
    }

    /**
     * Double-clicks an element.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     */
    @Tool(name = "element_double_click", description = "double clicks an element")
    public void doubleClick(locatorStrategy locatorStrategy, String locatorValue) {
        withLocator(locatorStrategy, locatorValue, "double-click",
                locator -> getDriver().element().doubleClick(locator));
    }

    /**
     * Clicks and holds an element.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     */
    @Tool(name = "element_click_and_hold", description = "clicks and holds an element")
    public void clickAndHold(locatorStrategy locatorStrategy, String locatorValue) {
        withLocator(locatorStrategy, locatorValue, "click-and-hold",
                locator -> getDriver().element().clickAndHold(locator));
    }

    /**
     * Types text into an element, dispatching to whichever engine is currently active and routing
     * through that engine's recording wrapper when a mobile or Playwright recording is active.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @param text text to type
     * @param append when true, appends instead of clearing first; blank/omitted defaults to false
     * @param clear when false, types without clearing first (equivalent to {@code append=true});
     *              blank/omitted defaults to true
     * @return the active engine and, when a recording is active, the recorded step's code block
     */
    @Tool(name = "element_type", description = "types text into an element; optional append (default false) "
            + "appends instead of clearing first, optional clear (default true; false behaves like append=true) "
            + "controls whether the field is cleared before typing; dispatches to the active engine and records "
            + "the step when a mobile or Playwright recording is active")
    public ElementActionResult type(
            locatorStrategy locatorStrategy,
            String locatorValue,
            String text,
            @ToolParam(required = false) Boolean append,
            @ToolParam(required = false) Boolean clear) {
        boolean resolvedAppend = Boolean.TRUE.equals(append) || Boolean.FALSE.equals(clear);
        ActiveEngine engine = EngineService.activeEngine();
        return switch (engine) {
            case PLAYWRIGHT -> fromDispatchResult(engine, resolvedAppend
                    ? playwrightService.appendText(locatorStrategy, locatorValue, text)
                    : playwrightService.type(locatorStrategy, locatorValue, text));
            case MOBILE_NATIVE, MOBILE_WEB -> fromDispatchResult(engine,
                    mobileService.dispatchType(locatorStrategy, locatorValue, text, resolvedAppend));
            case WEB, NONE -> {
                webType(locatorStrategy, locatorValue, text, resolvedAppend);
                yield new ElementActionResult(engine.name(), false, null, List.of());
            }
        };
    }

    /**
     * Java-caller convenience overload defaulting {@code append}/{@code clear}; not an MCP tool.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @param text text to type
     * @return the active engine and, when a recording is active, the recorded step's code block
     */
    public ElementActionResult type(locatorStrategy locatorStrategy, String locatorValue, String text) {
        return type(locatorStrategy, locatorValue, text, null, null);
    }

    private static void webType(locatorStrategy locatorStrategy, String locatorValue, String text, boolean append) {
        withLocator(locatorStrategy, locatorValue, append ? "type-append" : "type", locator -> {
            if (append) {
                getDriver().element().typeAppend(locator, text);
            } else {
                getDriver().element().type(locator, text);
            }
            logger.info("Typed into element (append: {}, length: {})", append, safeLength(text));
        });
    }

    /**
     * Appends text to an element.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @param textValue text values
     */
    @Tool(name = "element_append_text", description = "appends text to an element")
    public void appendText(locatorStrategy locatorStrategy, String locatorValue, CharSequence... textValue) {
        withLocator(locatorStrategy, locatorValue, "append-text", locator -> {
            getDriver().element().typeAppend(locator, textValue);
            logger.info("Appended text to element (value count: {}, total length: {})",
                    textValue == null ? 0 : textValue.length, totalLength(textValue));
        });
    }

    /**
     * Types text using SHAFT semantic locator behavior for direct Java callers.
     * Not exposed as an MCP tool because semantic names and typed values may contain sensitive application text.
     *
     * @param elementName semantic element name
     * @param textValue text values
     */
    public void typeSemantic(String elementName, CharSequence... textValue) {
        try {
            getDriver().element().type(elementName, textValue);
            logger.info("Semantic type completed (name length: {}, value count: {}, total length: {})",
                    safeLength(elementName), textValue == null ? 0 : textValue.length, totalLength(textValue));
        } catch (Exception exception) {
            logger.error("Semantic type failed (values redacted)", exception);
            throw exception;
        }
    }

    /**
     * Compatibility alias for older clients; not exposed as an MCP tool.
     *
     * @param elementName semantic element name
     * @param textValue text values
     */
    @Deprecated(forRemoval = false)
    public void typeUsingAI(String elementName, CharSequence... textValue) {
        typeSemantic(elementName, textValue);
    }

    /**
     * Sets an element value using JavaScript.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @param textValue value
     */
    @Tool(name = "element_set_value_js", description = "sets value to an element using JavaScript")
    public void setValueUsingJavaScript(locatorStrategy locatorStrategy, String locatorValue, String textValue) {
        withLocator(locatorStrategy, locatorValue, "javascript-set-value", locator -> {
            getDriver().element().setValueUsingJavaScript(locator, textValue);
            logger.info("Set element value with JavaScript (value length: {})", safeLength(textValue));
        });
    }

    /**
     * Clears an element, dispatching to whichever engine is currently active and routing through
     * that engine's recording wrapper when a mobile or Playwright recording is active.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @return the active engine and, when a recording is active, the recorded step's code block
     */
    @Tool(name = "element_clear", description = "clears text from an element; dispatches to the active engine "
            + "and records the step when a mobile or Playwright recording is active")
    public ElementActionResult clear(locatorStrategy locatorStrategy, String locatorValue) {
        ActiveEngine engine = EngineService.activeEngine();
        return switch (engine) {
            case PLAYWRIGHT -> fromDispatchResult(engine, playwrightService.clear(locatorStrategy, locatorValue));
            case MOBILE_NATIVE, MOBILE_WEB -> fromDispatchResult(engine, mobileService.clear(locatorStrategy, locatorValue));
            case WEB, NONE -> {
                withLocator(locatorStrategy, locatorValue, "clear", locator -> getDriver().element().clear(locator));
                yield new ElementActionResult(engine.name(), false, null, List.of());
            }
        };
    }

    /**
     * Drops a file onto an upload element, dispatching to whichever engine is currently active.
     * Mobile engines (MOBILE_NATIVE/MOBILE_WEB) share the same WebDriver-backed session as WEB and
     * use the same code path; only Playwright uses a distinct driver instance.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @param filePath local file path
     * @return the active engine and, when a Playwright recording is active, the recorded step's code block
     */
    @Tool(name = "element_drop_file_to_upload", description = "drops file to an element to upload; dispatches to "
            + "the active engine and records the step when a Playwright recording is active")
    public ElementActionResult dropFileToUpload(locatorStrategy locatorStrategy, String locatorValue, String filePath) {
        ActiveEngine engine = EngineService.activeEngine();
        if (engine == ActiveEngine.PLAYWRIGHT) {
            return fromDispatchResult(engine, playwrightService.uploadFile(locatorStrategy, locatorValue, filePath));
        }
        withLocator(locatorStrategy, locatorValue, "drop-file", locator -> {
            getDriver().element().dropFileToUpload(locator, filePath);
            logger.info("Dropped file to upload element (path length: {})", safeLength(filePath));
        });
        return new ElementActionResult(engine.name(), false, null, List.of());
    }

    /**
     * Drags one element to another, or drags a source element by an offset when no target locator is
     * given, dispatching to whichever engine is currently active. Mobile engines (MOBILE_NATIVE/
     * MOBILE_WEB) share the same WebDriver-backed session as WEB and use the same code path; only
     * Playwright uses a distinct driver instance.
     *
     * @param sourceLocatorStrategy source locator strategy
     * @param sourceLocatorValue source locator value
     * @param targetLocatorStrategy target locator strategy; omit together with targetLocatorValue to
     *                              use offsetX/offsetY instead
     * @param targetLocatorValue target locator value; omit to use offsetX/offsetY instead
     * @param offsetX horizontal offset from the source element; used only when no target locator is given
     * @param offsetY vertical offset from the source element; used only when no target locator is given
     * @return the active engine and, when a recording is active, the recorded step's code block
     */
    @Tool(name = "element_drag_and_drop", description = "drags and drops an element from source to a target "
            + "locator, or by offsetX/offsetY when no target locator is given; dispatches to the active engine "
            + "and records the step when a Playwright recording is active")
    public ElementActionResult dragAndDrop(
            locatorStrategy sourceLocatorStrategy,
            String sourceLocatorValue,
            @ToolParam(required = false) locatorStrategy targetLocatorStrategy,
            @ToolParam(required = false) String targetLocatorValue,
            @ToolParam(required = false) Integer offsetX,
            @ToolParam(required = false) Integer offsetY) {
        boolean hasTarget = targetLocatorStrategy != null && targetLocatorValue != null && !targetLocatorValue.isBlank();
        boolean hasOffset = offsetX != null && offsetY != null;
        if (!hasTarget && !hasOffset) {
            throw new IllegalArgumentException(
                    "element_drag_and_drop requires either a target locator or offsetX/offsetY.");
        }
        ActiveEngine engine = EngineService.activeEngine();
        if (engine == ActiveEngine.PLAYWRIGHT) {
            return fromDispatchResult(engine, hasTarget
                    ? playwrightService.dragAndDrop(sourceLocatorStrategy, sourceLocatorValue,
                            targetLocatorStrategy, targetLocatorValue)
                    : playwrightService.dragAndDropByOffset(sourceLocatorStrategy, sourceLocatorValue, offsetX, offsetY));
        }
        if (hasTarget) {
            webDragAndDrop(sourceLocatorStrategy, sourceLocatorValue, targetLocatorStrategy, targetLocatorValue);
        } else {
            webDragAndDropByOffset(sourceLocatorStrategy, sourceLocatorValue, offsetX, offsetY);
        }
        return new ElementActionResult(engine.name(), false, null, List.of());
    }

    /**
     * Java-caller convenience overload for the element-to-element drag shape; not an MCP tool.
     *
     * @param sourceLocatorStrategy source locator strategy
     * @param sourceLocatorValue source locator value
     * @param targetLocatorStrategy target locator strategy
     * @param targetLocatorValue target locator value
     * @return the active engine and, when a recording is active, the recorded step's code block
     */
    public ElementActionResult dragAndDrop(
            locatorStrategy sourceLocatorStrategy,
            String sourceLocatorValue,
            locatorStrategy targetLocatorStrategy,
            String targetLocatorValue) {
        return dragAndDrop(sourceLocatorStrategy, sourceLocatorValue, targetLocatorStrategy, targetLocatorValue, null, null);
    }

    private static void webDragAndDrop(
            locatorStrategy sourceLocatorStrategy,
            String sourceLocatorValue,
            locatorStrategy targetLocatorStrategy,
            String targetLocatorValue) {
        try {
            By sourceLocator = getLocator(sourceLocatorStrategy, sourceLocatorValue);
            By targetLocator = getLocator(targetLocatorStrategy, targetLocatorValue);
            getDriver().element().dragAndDrop(sourceLocator, targetLocator);
            logger.info("Drag-and-drop completed (strategies: {} -> {})",
                    sourceLocatorStrategy, targetLocatorStrategy);
        } catch (Exception exception) {
            logger.error("Drag-and-drop failed (locators redacted)", exception);
            throw exception;
        }
    }

    private static void webDragAndDropByOffset(
            locatorStrategy locatorStrategy, String locatorValue, int xOffset, int yOffset) {
        withLocator(locatorStrategy, locatorValue, "drag-and-drop-by-offset", locator -> {
            getDriver().element().dragAndDropByOffset(locator, xOffset, yOffset);
            logger.info("Offset drag completed (x: {}, y: {})", xOffset, yOffset);
        });
    }

    /**
     * Drags an element by an offset.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @param xOffset horizontal offset
     * @param yOffset vertical offset
     */
    @Tool(name = "element_drag_and_drop_by_offset", description = "drags and drops an element by offset")
    public void dragAndDropByOffset(locatorStrategy locatorStrategy, String locatorValue, int xOffset, int yOffset) {
        withLocator(locatorStrategy, locatorValue, "drag-and-drop-by-offset", locator -> {
            getDriver().element().dragAndDropByOffset(locator, xOffset, yOffset);
            logger.info("Offset drag completed (x: {}, y: {})", xOffset, yOffset);
        });
    }

    /**
     * Gets text for direct Java callers. Not exposed as an MCP tool because text may be sensitive.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @return element text
     */
    public String getText(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            String text = getDriver().element().get().text(getLocator(locatorStrategy, locatorValue));
            logger.info("Retrieved element text (length: {})", safeLength(text));
            return text;
        } catch (Exception exception) {
            logger.error("Failed to retrieve element text (locator redacted)", exception);
            throw exception;
        }
    }

    /**
     * Gets a DOM attribute for direct Java callers. Not exposed as an MCP tool.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @param domAttributeName attribute name
     * @return attribute value
     */
    public String getDomAttribute(locatorStrategy locatorStrategy, String locatorValue, String domAttributeName) {
        try {
            String value = getDriver().element().get().domAttribute(
                    getLocator(locatorStrategy, locatorValue), domAttributeName);
            logger.info("Retrieved DOM attribute (name length: {}, value length: {})",
                    safeLength(domAttributeName), safeLength(value));
            return value;
        } catch (Exception exception) {
            logger.error("Failed to retrieve DOM attribute (values redacted)", exception);
            throw exception;
        }
    }

    /**
     * Gets a DOM property for direct Java callers. Not exposed as an MCP tool.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @param domPropertyName property name
     * @return property value
     */
    public String getDomProperty(locatorStrategy locatorStrategy, String locatorValue, String domPropertyName) {
        try {
            String value = getDriver().element().get().domProperty(
                    getLocator(locatorStrategy, locatorValue), domPropertyName);
            logger.info("Retrieved DOM property (name length: {}, value length: {})",
                    safeLength(domPropertyName), safeLength(value));
            return value;
        } catch (Exception exception) {
            logger.error("Failed to retrieve DOM property (values redacted)", exception);
            throw exception;
        }
    }

    /**
     * Gets a CSS value for direct Java callers. Not exposed as an MCP tool.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @param cssPropertyName CSS property name
     * @return CSS value
     */
    public String getCssValue(locatorStrategy locatorStrategy, String locatorValue, String cssPropertyName) {
        try {
            String value = getDriver().element().get().cssValue(
                    getLocator(locatorStrategy, locatorValue), cssPropertyName);
            logger.info("Retrieved CSS value (name length: {}, value length: {})",
                    safeLength(cssPropertyName), safeLength(value));
            return value;
        } catch (Exception exception) {
            logger.error("Failed to retrieve CSS value (values redacted)", exception);
            throw exception;
        }
    }

    /**
     * Checks whether an element is displayed, dispatching to whichever engine is currently active.
     * Mobile engines (MOBILE_NATIVE/MOBILE_WEB) share the same WebDriver-backed session as WEB and use
     * the same code path; only Playwright uses a distinct driver instance.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @return the active engine and true when displayed
     */
    @Tool(name = "element_is_displayed", description = "checks if an element is displayed; dispatches to the "
            + "active engine")
    public ElementQueryResult isDisplayed(locatorStrategy locatorStrategy, String locatorValue) {
        ActiveEngine engine = EngineService.activeEngine();
        try {
            boolean displayed = engine == ActiveEngine.PLAYWRIGHT
                    ? playwrightService.isDisplayed(locatorStrategy, locatorValue)
                    : getDriver().element().get().isDisplayed(getLocator(locatorStrategy, locatorValue));
            logger.info("Element displayed state retrieved: {}", displayed);
            return new ElementQueryResult(engine.name(), displayed);
        } catch (Exception exception) {
            logger.error("Failed to check displayed state (locator redacted)", exception);
            throw exception;
        }
    }

    /**
     * Checks whether an element is enabled, dispatching to whichever engine is currently active.
     * Mobile engines (MOBILE_NATIVE/MOBILE_WEB) share the same WebDriver-backed session as WEB and use
     * the same code path; only Playwright uses a distinct driver instance.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @return the active engine and true when enabled
     */
    @Tool(name = "element_is_enabled", description = "checks if an element is enabled; dispatches to the active "
            + "engine")
    public ElementQueryResult isEnabled(locatorStrategy locatorStrategy, String locatorValue) {
        ActiveEngine engine = EngineService.activeEngine();
        try {
            boolean enabled = engine == ActiveEngine.PLAYWRIGHT
                    ? playwrightService.isEnabled(locatorStrategy, locatorValue)
                    : getDriver().element().get().isEnabled(getLocator(locatorStrategy, locatorValue));
            logger.info("Element enabled state retrieved: {}", enabled);
            return new ElementQueryResult(engine.name(), enabled);
        } catch (Exception exception) {
            logger.error("Failed to check enabled state (locator redacted)", exception);
            throw exception;
        }
    }

    /**
     * Checks whether an element (for example a checkbox or radio button) is selected, dispatching to
     * whichever engine is currently active. Mobile engines (MOBILE_NATIVE/MOBILE_WEB) share the same
     * WebDriver-backed session as WEB and use the same code path; Playwright uses a small dedicated
     * implementation via the raw {@code Locator.isChecked()} (design doc amendment A8), since no
     * SHAFT Playwright {@code ElementActions} equivalent existed before this commit.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @return the active engine and true when selected
     */
    @Tool(name = "element_is_selected", description = "checks if an element is selected; dispatches to the "
            + "active engine")
    public ElementQueryResult isSelected(locatorStrategy locatorStrategy, String locatorValue) {
        ActiveEngine engine = EngineService.activeEngine();
        try {
            boolean selected = engine == ActiveEngine.PLAYWRIGHT
                    ? playwrightService.isSelected(locatorStrategy, locatorValue)
                    : getDriver().element().get().isSelected(getLocator(locatorStrategy, locatorValue));
            logger.info("Element selected state retrieved: {}", selected);
            return new ElementQueryResult(engine.name(), selected);
        } catch (Exception exception) {
            logger.error("Failed to check selected state (locator redacted)", exception);
            throw exception;
        }
    }

    private static void withLocator(
            locatorStrategy locatorStrategy,
            String locatorValue,
            String operation,
            LocatorOperation locatorOperation) {
        try {
            locatorOperation.execute(getLocator(locatorStrategy, locatorValue));
            logger.info("Element operation completed (operation: {}, strategy: {}, locator length: {})",
                    operation, locatorStrategy, safeLength(locatorValue));
        } catch (Exception exception) {
            logger.error("Element operation failed (operation: {}, locator redacted)", operation, exception);
            throw exception;
        }
    }

    private static int safeLength(String value) {
        return value == null ? 0 : value.length();
    }

    private static int totalLength(CharSequence... values) {
        if (values == null) {
            return 0;
        }
        int total = 0;
        for (CharSequence value : values) {
            total += value == null ? 0 : value.length();
        }
        return total;
    }

    @FunctionalInterface
    private interface LocatorOperation {
        void execute(By locator);
    }
}
