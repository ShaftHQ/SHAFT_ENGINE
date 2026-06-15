package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import static com.shaft.mcp.EngineService.getDriver;
import static com.shaft.mcp.EngineService.getLocator;

/**
 * MCP-safe element actions. Sensitive values are returned only to direct callers and are never logged.
 */
@Service
public class ElementService {
    private static final Logger logger = LoggerFactory.getLogger(ElementService.class);

    /**
     * Hovers over an element.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     */
    @Tool(name = "element_hover", description = "hovers over an element")
    public void hover(locatorStrategy locatorStrategy, String locatorValue) {
        withLocator(locatorStrategy, locatorValue, "hover", locator -> getDriver().element().hover(locator));
    }

    /**
     * Clicks an element.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     */
    @Tool(name = "element_click", description = "clicks an element")
    public void click(locatorStrategy locatorStrategy, String locatorValue) {
        withLocator(locatorStrategy, locatorValue, "click", locator -> getDriver().element().click(locator));
    }

    /**
     * Clicks an element using SHAFT semantic locator behavior.
     *
     * @param elementName semantic element name
     */
    @Tool(name = "element_click_semantic",
            description = "clicks an element using SHAFT semantic locator behavior; no AI provider is called")
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
     * Types text into an element.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @param textValue text values
     */
    @Tool(name = "element_type", description = "types value to an element")
    public void type(locatorStrategy locatorStrategy, String locatorValue, CharSequence... textValue) {
        withLocator(locatorStrategy, locatorValue, "type", locator -> {
            getDriver().element().type(locator, textValue);
            logger.info("Typed into element (value count: {}, total length: {})",
                    textValue == null ? 0 : textValue.length, totalLength(textValue));
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
     * Types text using SHAFT semantic locator behavior.
     *
     * @param elementName semantic element name
     * @param textValue text values
     */
    @Tool(name = "element_type_semantic",
            description = "types value using SHAFT semantic locator behavior; no AI provider is called")
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
     * Clears an element.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     */
    @Tool(name = "element_clear", description = "clears text from an element")
    public void clear(locatorStrategy locatorStrategy, String locatorValue) {
        withLocator(locatorStrategy, locatorValue, "clear", locator -> getDriver().element().clear(locator));
    }

    /**
     * Drops a file onto an upload element.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @param filePath local file path
     */
    @Tool(name = "element_drop_file_to_upload", description = "drops file to an element to upload")
    public void dropFileToUpload(locatorStrategy locatorStrategy, String locatorValue, String filePath) {
        withLocator(locatorStrategy, locatorValue, "drop-file", locator -> {
            getDriver().element().dropFileToUpload(locator, filePath);
            logger.info("Dropped file to upload element (path length: {})", safeLength(filePath));
        });
    }

    /**
     * Drags one element to another.
     *
     * @param sourceLocatorStrategy source locator strategy
     * @param sourceLocatorValue source locator value
     * @param targetLocatorStrategy target locator strategy
     * @param targetLocatorValue target locator value
     */
    @Tool(name = "element_drag_and_drop", description = "drags and drops an element from source to target")
    public void dragAndDrop(
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
     * Checks whether an element is displayed.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @return true when displayed
     */
    @Tool(name = "element_is_displayed", description = "checks if an element is displayed")
    public boolean isDisplayed(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            boolean displayed = getDriver().element().get().isDisplayed(getLocator(locatorStrategy, locatorValue));
            logger.info("Element displayed state retrieved: {}", displayed);
            return displayed;
        } catch (Exception exception) {
            logger.error("Failed to check displayed state (locator redacted)", exception);
            throw exception;
        }
    }

    /**
     * Checks whether an element is enabled.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @return true when enabled
     */
    @Tool(name = "element_is_enabled", description = "checks if an element is enabled")
    public boolean isEnabled(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            boolean enabled = getDriver().element().get().isEnabled(getLocator(locatorStrategy, locatorValue));
            logger.info("Element enabled state retrieved: {}", enabled);
            return enabled;
        } catch (Exception exception) {
            logger.error("Failed to check enabled state (locator redacted)", exception);
            throw exception;
        }
    }

    /**
     * Checks whether an element is selected.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @return true when selected
     */
    @Tool(name = "element_is_selected", description = "checks if an element is selected")
    public boolean isSelected(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            boolean selected = getDriver().element().get().isSelected(getLocator(locatorStrategy, locatorValue));
            logger.info("Element selected state retrieved: {}", selected);
            return selected;
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
