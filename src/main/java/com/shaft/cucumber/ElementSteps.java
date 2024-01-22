package com.shaft.cucumber;

import com.shaft.driver.SHAFT;
import com.shaft.enums.internal.ClipboardAction;
import io.cucumber.java.en.When;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;

import java.util.Objects;

public class ElementSteps {
    private final ThreadLocal<SHAFT.GUI.WebDriver> driver;

    public ElementSteps(ThreadLocal<SHAFT.GUI.WebDriver> driver) {
        this.driver = Objects.requireNonNullElseGet(driver, ThreadLocal::new);
    }

    protected static By getLocatorFromTypeAndValue(String locatorType, String locatorValue) {
        switch (locatorType.toLowerCase()) {
            case "id" -> {
                return By.id(locatorValue);
            }
            case "tagname", "tag_name", "tag name" -> {
                return By.tagName(locatorValue);
            }
            case "classname", "class_name", "class name" -> {
                return By.className(locatorValue);
            }
            case "name" -> {
                return By.name(locatorValue);
            }
            case "linktext", "link_text", "link text" -> {
                return By.linkText(locatorValue);
            }
            case "partiallinktext", "partial_link_text", "partial link text" -> {
                return By.partialLinkText(locatorValue);
            }
            case "cssselector", "css", "selector", "css_selector", "css selector" -> {
                return By.cssSelector(locatorValue);
            }
            default -> {
                return By.xpath(locatorValue);
            }
        }
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element.
     *
     * @param text         the target text that needs to be typed into the target
     *                     webElement
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @SuppressWarnings("SpellCheckingInspection")
    @When("I Type {string} into the element found by {string}: {string}")
//    @عندما("اقوم بكتابة {string} بداخل مربع الكتابة المحدد بإستخدام {string} بقيمة {string}")
    public void type(String text, String locatorType, String locatorValue) {
        driver.get().element().type(getLocatorFromTypeAndValue(locatorType, locatorValue), text);
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required string into the target element. Obfuscates the written text in the output report. This action should be used for writing passwords and secure text.
     *
     * @param text         the target text that needs to be typed into the target
     *                     webElement
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I Type {string} securely into the element found by {string}: {string}")
    public void typeSecure(String text, String locatorType, String locatorValue) {
        driver.get().element().typeSecure(getLocatorFromTypeAndValue(locatorType, locatorValue), text);
    }

    /**
     * Appends the required string into the target element, regardless of the
     * current text value.
     *
     * @param text         the target text that needs to be appended into the target webElement
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I Append the text {string} to the element found by {string}: {string}")
    public void typeAppend(String text, String locatorType, String locatorValue) {
        driver.get().element().typeAppend(getLocatorFromTypeAndValue(locatorType, locatorValue), text);
    }

    /**
     * ValidationEnums the required file path into an input[type='file'] button, to
     * successfully upload the target file.
     *
     * @param absoluteFilePath the full path to the file that needs to be uploaded
     * @param locatorType      can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue     the value/expression of the desired element locator
     */
    @When("I Upload the file {string} to the element found by {string}: {string}")
    public void typeFileLocationForUpload(String absoluteFilePath, String locatorType, String locatorValue) {
        driver.get().element().typeFileLocationForUpload(getLocatorFromTypeAndValue(locatorType, locatorValue), absoluteFilePath);
    }

    /**
     * Sends a keypress to the target element. Supported keys are: ENTER, RETURN, TAB.
     *
     * @param key          the key that should be pressed
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @SuppressWarnings("SpellCheckingInspection")
    @When("I Press the {string} key into the element found by {string}: {string}")
//    @عندما("اقوم بالضغط على زر {string} بداخل عنصر الويب المحدد بإستخدام {string} بقيمة {string}")
    public void keyPress(String key, String locatorType, String locatorValue) {
        driver.get().element().keyPress(getLocatorFromTypeAndValue(locatorType, locatorValue), Keys.valueOf(key.toUpperCase()));
    }

    /**
     * Clicks on a certain element using Selenium WebDriver, or JavaScript.
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I Click the element found by {string}: {string}")
    public void click(String locatorType, String locatorValue) {
        driver.get().element().click(getLocatorFromTypeAndValue(locatorType, locatorValue));
    }

    /**
     * Waits for the element to be clickable, and then clicks and holds it.
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I Click and hold the element found by {string}: {string}")
    public void clickAndHold(String locatorType, String locatorValue) {
        driver.get().element().clickAndHold(getLocatorFromTypeAndValue(locatorType, locatorValue));
    }

    /**
     * Attempts to perform a native clipboard action on the text from a certain web
     * element, like copy/cut/paste
     *
     * @param action       supports the following actions "copy", "paste", "cut",
     *                     "select all", "unselect"
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I use the clipboard to perform {string} on the element found by {string}: {string}")
    public void clipboardActions(String action, String locatorType, String locatorValue) {
        driver.get().element().clipboardActions(getLocatorFromTypeAndValue(locatorType, locatorValue), ClipboardAction.valueOf(action));
    }

    /**
     * Double-clicks on an element using Selenium WebDriver's Actions Library
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I Double-click the element found by {string}: {string}")
    public void doubleClick(String locatorType, String locatorValue) {
        driver.get().element().doubleClick(getLocatorFromTypeAndValue(locatorType, locatorValue));
    }

    /**
     * Drags the source element and drops it onto the destination element
     *
     * @param sourceLocatorType       can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param sourceLocatorValue      the value/expression of the source element locator that's draggable
     * @param destinationLocatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param destinationLocatorValue the value/expression of the target element locator that's droppable
     */
    @When("I Drag the element found by {string}: {string} and drop it on the element found by {string}: {string}")
    public void dragAndDrop(String sourceLocatorType, String sourceLocatorValue, String destinationLocatorType, String destinationLocatorValue) {
        driver.get().element().dragAndDrop(getLocatorFromTypeAndValue(sourceLocatorType, sourceLocatorValue), getLocatorFromTypeAndValue(destinationLocatorType, destinationLocatorValue));
    }

    /**
     * Drags the source element and drops it onto the determined offset
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     * @param xOffset      the horizontal offset by which the element should
     *                     be moved
     * @param yOffset      the vertical offset by which the element should
     *                     be moved
     */
    @When("I Drag the element found by {string}: {string} and drop it by offset x={int} and y={int}")
    public void dragAndDropByOffset(String locatorType, String locatorValue, int xOffset, int yOffset) {
        driver.get().element().dragAndDropByOffset(getLocatorFromTypeAndValue(locatorType, locatorValue), xOffset, yOffset);
    }

    /**
     * Hovers over target element.
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I Hover over the element found by {string}: {string}")
    public void hover(String locatorType, String locatorValue) {
        driver.get().element().hover(getLocatorFromTypeAndValue(locatorType, locatorValue));
    }

    /**
     * Selects an element from a dropdown list using its displayed text
     *
     * @param text         the text of the choice that you need to select from the target dropDown menu
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I Select {string} from the drop-down list element found by {string}: {string}")
    public void select(String text, String locatorType, String locatorValue) {
        driver.get().element().select(getLocatorFromTypeAndValue(locatorType, locatorValue), text);
    }

    /**
     * Used to SetProperty value for an element (hidden or visible) using javascript
     *
     * @param value        the desired value that should be SetProperty for the target element
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I Set the value {string} into the element found by {string}: {string}")
    public void setValueUsingJavaScript(String value, String locatorType, String locatorValue) {
        driver.get().element().setValueUsingJavaScript(getLocatorFromTypeAndValue(locatorType, locatorValue), value);
    }

    /**
     * Used to submit a form using javascript
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I Submit the form found by {string}: {string}")
    public void submitFormUsingJavaScript(String locatorType, String locatorValue) {
        driver.get().element().submitFormUsingJavaScript(getLocatorFromTypeAndValue(locatorType, locatorValue));
    }

    /**
     * Waits dynamically for a specific element to achieve the desired
     * stateOfPresence on the current page. Waits for a specific number of retries
     * multiplied by the default element identification timeout (in the POM.xml
     * file)
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I Wait for the element found by {string}: {string} to be present")
    public void waitForElementToBePresent(String locatorType, String locatorValue) {
        driver.get().element().waitToBeReady(getLocatorFromTypeAndValue(locatorType, locatorValue));
    }

    /**
     * Waits dynamically for a specific element to achieve the desired
     * stateOfPresence on the current page. Waits for a specific number of retries
     * multiplied by the default element identification timeout (in the POM.xml
     * file)
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I Wait for the element found by {string}: {string} to be not present")
    public void waitForElementToBeNotPresent(String locatorType, String locatorValue) {
        driver.get().element().waitToBeInvisible(getLocatorFromTypeAndValue(locatorType, locatorValue));
    }

    /**
     * Waits dynamically for a specific element's text to change from the initial
     * value to a new unknown value. Waits for a specific number of retries
     * multiplied by the default element identification timeout (in the POM.xml
     * file)
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     * @param initialValue the initial text value of the target webElement
     */
    @When("I Wait for the text inside the element found by {string}: {string} to change from the initial value {string}")
    public void waitForTextToChange(String locatorType, String locatorValue, String initialValue) {
        driver.get().element().waitForTextToChange(getLocatorFromTypeAndValue(locatorType, locatorValue), initialValue);

    }

    @SuppressWarnings("unused")
    protected enum LocatorType {
        ID("id"), TAG_NAME("tagname"), CLASS_NAME("classname"), NAME("name"), LINK_TEXT("linktext"),
        PARTIAL_LINK_TEXT("partiallinktext"), CSS_SELECTOR("cssselector"), XPATH("xpath");

        private final String value;

        LocatorType(String type) {
            this.value = type;
        }

        private String getValue() {
            return value;
        }
    }
}