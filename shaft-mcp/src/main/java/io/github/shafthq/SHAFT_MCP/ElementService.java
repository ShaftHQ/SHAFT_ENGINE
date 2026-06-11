package io.github.shafthq.SHAFT_MCP;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import static io.github.shafthq.SHAFT_MCP.EngineService.getDriver;
import static io.github.shafthq.SHAFT_MCP.EngineService.getLocator;

@Service
public class ElementService {
    private static final Logger logger = LoggerFactory.getLogger(ElementService.class);

    /**
     * Hovers over an element identified by the specified locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     */
    @Tool(name = "element_hover", description = "hovers over an element")
    public void hover(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            driver.element().hover(locator);
            logger.info("Hovered over element with locator: {} - {}", locatorStrategy, locatorValue);
        } catch (Exception e) {
            logger.error("Failed to hover over element with locator: {} - {}", locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Clicks on an element identified by the specified locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     */
    @Tool(name = "element_click", description = "clicks an element")
    public void click(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            driver.element().click(locator);
            logger.info("Clicked element with locator: {} - {}", locatorStrategy, locatorValue);
        } catch (Exception e) {
            logger.error("Failed to click element with locator: {} - {}", locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Clicks on an element identified by the specified name using Artificial Intelligence.
     *
     * @param elementName The name of the element to click, this is the visible text of the element. Some elements have placeholder, other element have title text written in a label above or next to them.
     *                    For example, to click on a button with text "Submit", you would use "Submit" as the element name.
     */
    @Tool(name = "element_click_ai", description = "clicks an element using AI")
    public void clickUsingAI(String elementName) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.element().click(elementName);
            logger.info("Clicked element using AI with name: {}", elementName);
        } catch (Exception e) {
            logger.error("Failed to click element using AI with name: {}", elementName, e);
            throw e;
        }
    }

    /**
     * Click using JavaScript on an element identified by the specified locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     */
    @Tool(name = "element_click_js", description = "clicks an element using JavaScript")
    public void clickUsingJavaScript(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            driver.element().clickUsingJavascript(locator);
            logger.info("Clicked element using JavaScript with locator: {} - {}", locatorStrategy, locatorValue);
        } catch (Exception e) {
            logger.error("Failed to click element using JavaScript with locator: {} - {}", locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Double-clicks on an element identified by the specified locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     */
    @Tool(name = "element_double_click", description = "double clicks an element")
    public void doubleClick(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            driver.element().doubleClick(locator);
            logger.info("Double-clicked element with locator: {} - {}", locatorStrategy, locatorValue);
        } catch (Exception e) {
            logger.error("Failed to double-click element with locator: {} - {}", locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Click and hold on an element identified by the specified locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     */
    @Tool(name = "element_click_and_hold", description = "clicks and holds an element")
    public void clickAndHold(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            driver.element().clickAndHold(locator);
            logger.info("Clicked and held element with locator: {} - {}", locatorStrategy, locatorValue);
        } catch (Exception e) {
            logger.error("Failed to click and hold element with locator: {} - {}", locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Types the specified text into an element identified by the given locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     * @param textValue       The text to type into the element.
     */
    @Tool(name = "element_type", description = "types value to an element")
    public void type(locatorStrategy locatorStrategy, String locatorValue, CharSequence... textValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            driver.element().type(locator, textValue);
            logger.info("Typed text '{}' into element with locator: {} - {}", String.join(", ", textValue), locatorStrategy, locatorValue);
        } catch (Exception e) {
            logger.error("Failed to type text '{}' into element with locator: {} - {}", String.join(", ", textValue), locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Appends the specified text to an element identified by the given locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     * @param textValue       The text to append to the element.
     */
    @Tool(name = "element_append_text", description = "appends text to an element")
    public void appendText(locatorStrategy locatorStrategy, String locatorValue, CharSequence... textValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            driver.element().typeAppend(locator, textValue);
            logger.info("Appended text '{}' to element with locator: {} - {}", String.join(", ", textValue), locatorStrategy, locatorValue);
        } catch (Exception e) {
            logger.error("Failed to append text '{}' to element with locator: {} - {}", String.join(", ", textValue), locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Types the specified text into an element identified by the given name using Artificial Intelligence.
     *
     * @param elementName The name of the element to click, this is the visible text of the element. Some elements have placeholder, other element have title text written in a label above or next to them.
     *                    For example, to type into a text field with label or placeholder text, or next to or below a div with this text "Username", you would use "Username" as the element name.
     * @param textValue   The text to type into the element.
     */
    @Tool(name = "element_type_ai", description = "types value to an element using AI")
    public void typeUsingAI(String elementName, CharSequence... textValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.element().type(elementName, textValue);
            logger.info("Typed text '{}' into element using AI with name: {}", String.join(", ", textValue), elementName);
        } catch (Exception e) {
            logger.error("Failed to type text '{}' into element using AI with name: {}", String.join(", ", textValue), elementName, e);
            throw e;
        }
    }

    /**
     * Set value using JavaScript on an element identified by the specified locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     * @param textValue       The text to set into the element.
     */
    @Tool(name = "element_set_value_js", description = "sets value to an element using JavaScript")
    public void setValueUsingJavaScript(locatorStrategy locatorStrategy, String locatorValue, String textValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            driver.element().setValueUsingJavaScript(locator, textValue);
            logger.info("Set value '{}' to element using JavaScript with locator: {} - {}", textValue, locatorStrategy, locatorValue);
        } catch (Exception e) {
            logger.error("Failed to set value '{}' to element using JavaScript with locator: {} - {}", textValue, locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Clears the text from an input element identified by the specified locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     */
    @Tool(name = "element_clear", description = "clears text from an element")
    public void clear(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            driver.element().clear(locator);
            logger.info("Cleared text from element with locator: {} - {}", locatorStrategy, locatorValue);
        } catch (Exception e) {
            logger.error("Failed to clear text from element with locator: {} - {}", locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Drop file to upload to an element identified by the specified locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     * @param filePath        The path of the file to upload.
     */
    @Tool(name = "element_drop_file_to_upload", description = "drops file to an element to upload")
    public void dropFileToUpload(locatorStrategy locatorStrategy, String locatorValue, String filePath) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            driver.element().dropFileToUpload(locator, filePath);
            logger.info("Dropped file '{}' to element with locator: {} - {}", filePath, locatorStrategy, locatorValue);
        } catch (Exception e) {
            logger.error("Failed to drop file '{}' to element with locator: {} - {}", filePath, locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Drag and drops an element from a source locator to a target locator.
     *
     * @param sourceLocatorStrategy The strategy to locate the source element (e.g., ID, XPATH).
     * @param sourceLocatorValue    The value used with the source locator strategy to find the source element.
     * @param targetLocatorStrategy The strategy to locate the target element (e.g., ID, XPATH).
     * @param targetLocatorValue    The value used with the target locator strategy to find the target element.
     */
    @Tool(name = "element_drag_and_drop", description = "drags and drops an element from source to target")
    public void dragAndDrop(locatorStrategy sourceLocatorStrategy, String sourceLocatorValue, locatorStrategy
            targetLocatorStrategy, String targetLocatorValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By sourceLocator = getLocator(sourceLocatorStrategy, sourceLocatorValue);
            By targetLocator = getLocator(targetLocatorStrategy, targetLocatorValue);
            driver.element().dragAndDrop(sourceLocator, targetLocator);
            logger.info("Dragged and dropped element from locator: {} - {} to locator: {} - {}",
                    sourceLocatorStrategy, sourceLocatorValue, targetLocatorStrategy, targetLocatorValue);
        } catch (Exception e) {
            logger.error("Failed to drag and drop element from locator: {} - {} to locator: {} - {}",
                    sourceLocatorStrategy, sourceLocatorValue, targetLocatorStrategy, targetLocatorValue, e);
            throw e;
        }
    }

    /**
     * Drag and drop by offset an element identified by the specified locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     * @param xOffset         The horizontal offset to drag the element.
     * @param yOffset         The vertical offset to drag the element.
     */
    @Tool(name = "element_drag_and_drop_by_offset", description = "drags and drops an element by offset")
    public void dragAndDropByOffset(locatorStrategy locatorStrategy, String locatorValue, int xOffset, int yOffset) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            driver.element().dragAndDropByOffset(locator, xOffset, yOffset);
            logger.info("Dragged and dropped element with locator: {} - {} by offset: ({}, {})",
                    locatorStrategy, locatorValue, xOffset, yOffset);
        } catch (Exception e) {
            logger.error("Failed to drag and drop element with locator: {} - {} by offset: ({}, {})",
                    locatorStrategy, locatorValue, xOffset, yOffset, e);
            throw e;
        }
    }

    /**
     * Retrieves the text content of an element identified by the specified locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     * @return The text content of the element.
     */
    @Tool(name = "element_get_text", description = "gets text of an element")
    public String getText(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            String text = driver.element().get().text(locator);
            logger.info("Retrieved text '{}' from element with locator: {} - {}", text, locatorStrategy, locatorValue);
            return text;
        } catch (Exception e) {
            logger.error("Failed to retrieve text from element with locator: {} - {}", locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Retrieves the value of a specified DOM attribute from an element identified by the given locator strategy and value.
     *
     * @param locatorStrategy  The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue     The value used with the locator strategy to find the element.
     * @param domAttributeName The name of the DOM attribute whose value is to be retrieved.
     * @return The value of the specified DOM attribute, or null if the attribute does not exist.
     */
    @Tool(name = "element_get_dom_attribute", description = "gets a DOM attribute value of an element")
    public String getDomAttribute(locatorStrategy locatorStrategy, String locatorValue, String domAttributeName) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            String attributeValue = driver.element().get().domAttribute(locator, domAttributeName);
            logger.info("Retrieved DOM attribute '{}' with value '{}' from element with locator: {} - {}",
                    domAttributeName, attributeValue, locatorStrategy, locatorValue);
            return attributeValue;
        } catch (Exception e) {
            logger.error("Failed to retrieve DOM attribute '{}' from element with locator: {} - {}",
                    domAttributeName, locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Retrieves the value of a specified DOM property from an element identified by the given locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     * @param domPropertyName The name of the DOM property whose value is to be retrieved.
     * @return The value of the specified DOM property, or null if the property does not exist.
     */
    @Tool(name = "element_get_dom_property", description = "gets a DOM property value of an element")
    public String getDomProperty(locatorStrategy locatorStrategy, String locatorValue, String domPropertyName) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            String propertyValue = driver.element().get().domProperty(locator, domPropertyName);
            logger.info("Retrieved DOM property '{}' with value '{}' from element with locator: {} - {}",
                    domPropertyName, propertyValue, locatorStrategy, locatorValue);
            return propertyValue;
        } catch (Exception e) {
            logger.error("Failed to retrieve DOM property '{}' from element with locator: {} - {}",
                    domPropertyName, locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Retrieves the value of a specified CSS property from an element identified by the given locator strategy and value.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     * @param cssPropertyName The name of the CSS property whose value is to be retrieved.
     * @return The value of the specified CSS property, or null if the property does not exist.
     */
    @Tool(name = "element_get_css_value", description = "gets a CSS property value of an element")
    public String getCssValue(locatorStrategy locatorStrategy, String locatorValue, String cssPropertyName) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            String cssValue = driver.element().get().cssValue(locator, cssPropertyName);
            logger.info("Retrieved CSS property '{}' with value '{}' from element with locator: {} - {}",
                    cssPropertyName, cssValue, locatorStrategy, locatorValue);
            return cssValue;
        } catch (Exception e) {
            logger.error("Failed to retrieve CSS property '{}' from element with locator: {} - {}",
                    cssPropertyName, locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Checks if an element identified by the specified locator strategy and value is displayed on the page.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     * @return True if the element is visible, false otherwise.
     */
    @Tool(name = "element_is_displayed", description = "checks if an element is displayed")
    public boolean isDisplayed(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            boolean isDisplayed = driver.element().get().isDisplayed(locator);
            logger.info("Element with locator: {} - {} is displayed: {}", locatorStrategy, locatorValue, isDisplayed);
            return isDisplayed;
        } catch (Exception e) {
            logger.error("Failed to check visibility of element with locator: {} - {}", locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Checks if an element identified by the specified locator strategy and value is enabled.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     * @return True if the element is enabled, false otherwise.
     */
    @Tool(name = "element_is_enabled", description = "checks if an element is enabled")
    public boolean isEnabled(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            boolean isEnabled = driver.element().get().isEnabled(locator);
            logger.info("Element with locator: {} - {} is enabled: {}", locatorStrategy, locatorValue, isEnabled);
            return isEnabled;
        } catch (Exception e) {
            logger.error("Failed to check if element with locator: {} - {} is enabled", locatorStrategy, locatorValue, e);
            throw e;
        }
    }

    /**
     * Checks if an element identified by the specified locator strategy and value is selected.
     *
     * @param locatorStrategy The strategy to locate the element (e.g., ID, XPATH).
     * @param locatorValue    The value used with the locator strategy to find the element.
     * @return True if the element is selected, false otherwise.
     */
    @Tool(name = "element_is_selected", description = "checks if an element is selected")
    public boolean isSelected(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            By locator = getLocator(locatorStrategy, locatorValue);
            boolean isSelected = driver.element().get().isSelected(locator);
            logger.info("Element with locator: {} - {} is selected: {}", locatorStrategy, locatorValue, isSelected);
            return isSelected;
        } catch (Exception e) {
            logger.error("Failed to check if element with locator: {} - {} is selected", locatorStrategy, locatorValue, e);
            throw e;
        }
    }
}