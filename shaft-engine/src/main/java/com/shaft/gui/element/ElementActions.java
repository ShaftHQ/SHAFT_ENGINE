package com.shaft.gui.element;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.driver.internal.WizardHelpers;
import com.shaft.gui.element.internal.Actions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.internal.WebDriverElementValidationsBuilder;
import org.openqa.selenium.*;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.time.Duration;
import java.util.*;

/**
 * Provides high-level actions for interacting with web and mobile elements
 * such as clicking, typing, selecting, dragging, and retrieving element state.
 *
 * <p>This class extends {@link FluentWebDriverAction} and supports method
 * chaining via the {@link #and()} connector. Prefer using
 * {@link com.shaft.gui.element.internal.Actions} (returned by
 * {@link com.shaft.driver.SHAFT.GUI.WebDriver#element()}) for new tests.
 *
 * <p><b>Usage example:</b>
 * <pre>{@code
 * driver.element().click(By.id("submit"))
 *       .and().element().type(By.id("name"), "John")
 *       .and().assertThat(By.id("result")).text().contains("Success");
 * }</pre>
 *
 * @see com.shaft.gui.element.internal.Actions
 * @see <a href="https://shaftengine.netlify.app/">SHAFT User Guide &ndash; Element Actions</a>
 */
@SuppressWarnings({"unused", "UnusedReturnValue"})
public class ElementActions extends FluentWebDriverAction implements com.shaft.gui.driver.ElementActions {
    /**
     * Creates an element actions instance using a lazily initialized default driver helper.
     */
    public ElementActions() {
        initialize();
    }

    /**
     * Creates an element actions instance bound to an existing WebDriver.
     *
     * @param driver the active WebDriver session to use
     */
    public ElementActions(WebDriver driver) {
        initialize(driver);
    }

    /**
     * Creates an element actions instance bound to an existing WebDriver with optional silent logging.
     *
     * @param driver the active WebDriver session to use
     * @param isSilent {@code true} to suppress non-critical action logs; {@code false} otherwise
     */
    public ElementActions(WebDriver driver, boolean isSilent) {
        initialize(driver, isSilent);
    }

    /**
     * Creates an element actions instance bound to an existing driver helper.
     *
     * @param helper the driver factory helper that manages the underlying WebDriver
     */
    public ElementActions(DriverFactoryHelper helper) {
        initialize(helper);
    }

    @Override
    public Actions and() {
        return new Actions(driverFactoryHelper);
    }

    /**
     * Starts a hard-assertion chain on the target element.
     * The test fails immediately when the assertion condition is not met.
     *
     * @param elementLocator the locator of the element under test (e.g. {@code By.id("result")})
     * @return a {@link WebDriverElementValidationsBuilder} for chaining element assertions
     * @see <a href="https://shaftengine.netlify.app/">SHAFT User Guide &ndash; Assertions</a>
     */
    @Override
    public WebDriverElementValidationsBuilder assertThat(By elementLocator) {
        return new WizardHelpers.WebDriverAssertions(driverFactoryHelper).element(elementLocator);
    }

    /**
     * Starts a soft-assertion (verification) chain on the target element.
     * Failures are collected and reported at the end of the test rather than stopping execution immediately.
     *
     * @param elementLocator the locator of the element under test (e.g. {@code By.id("result")})
     * @return a {@link WebDriverElementValidationsBuilder} for chaining element verifications
     * @see <a href="https://shaftengine.netlify.app/">SHAFT User Guide &ndash; Assertions</a>
     */
    @Override
    public WebDriverElementValidationsBuilder verifyThat(By elementLocator) {
        return new WizardHelpers.WebDriverVerifications(driverFactoryHelper).element(elementLocator);
    }

    /**
     * Returns the number of elements on the current page that match the given locator.
     *
     * @param elementLocator the locator used to find elements (e.g. {@code By.cssSelector(".item")})
     * @return the count of matching elements, or {@code 0} if none are found
     * @see <a href="https://shaftengine.netlify.app/">SHAFT User Guide &ndash; Element Actions</a>
     */
    @Override
    public int getElementsCount(By elementLocator) {
        return elementActionsHelper.getElementsCount(driverFactoryHelper.getDriver(), elementLocator);
    }

    /**
     * This is a generic method to enable the execution of the native mobile
     * commands found herein: <a href="http://appium.io/docs/en/commands/mobile-command/">appium.io</a>
     * <p>
     * Note: This method does no validation on the output of the executed JavaScript
     *
     * @param command    the desired mobile command to be executed. e.g., "mobile:
     *                   scroll"
     * @param parameters a map of the key, value parameters for this command. e.g.,
     *                   ImmutableMap.of("direction", "down")
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions executeNativeMobileCommand(String command, Map<String, String> parameters) {
        return new Actions(driverFactoryHelper).executeNativeMobileCommand(command, parameters);
    }

    /**
     * Clicks on a certain element using Selenium WebDriver, or JavaScript
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions click(By elementLocator) {
        return new Actions(driverFactoryHelper).click(elementLocator);
    }

    /**
     * Clicks on certain element using javaScript only
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions clickUsingJavascript(By elementLocator) {
        return new Actions(driverFactoryHelper).clickUsingJavascript(elementLocator);
    }

    /**
     * If the element is outside the viewport, scrolls the bottom of the element to the bottom of the viewport.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions scrollToElement(By elementLocator) {
        return new Actions(driverFactoryHelper).scrollToElement(elementLocator);
    }

    /**
     * Waits for the element to be clickable, and then clicks and holds it.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions clickAndHold(By elementLocator) {
        return new Actions(driverFactoryHelper).clickAndHold(elementLocator);
    }

    /**
     * Double-clicks on an element using Selenium WebDriver's Actions Library
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions doubleClick(By elementLocator) {
        return new Actions(driverFactoryHelper).doubleClick(elementLocator);
    }

    /**
     * Drags the source element and drops it onto the destination element
     *
     * @param sourceElementLocator      the locator of the source webElement that
     *                                  should be dragged under test (By xpath, id,
     *                                  selector, name ...etc.)
     * @param destinationElementLocator the locator of the target webElement that
     *                                  should receive the dropped source element
     *                                  under test (By xpath, id, selector, name
     *                                  ...etc.)
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions dragAndDrop(By sourceElementLocator, By destinationElementLocator) {
        return new Actions(driverFactoryHelper).dragAndDrop(sourceElementLocator, destinationElementLocator);
    }

    /**
     * Drags the source element and drops it onto the determined offset
     *
     * @param sourceElementLocator the locator of the source webElement that should
     *                             be dragged under test (By xpath, id, selector,
     *                             name ...etc.)
     * @param xOffset              the horizontal offset by which the element should
     *                             be moved
     * @param yOffset              the vertical offset by which the element should
     *                             be moved
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions dragAndDropByOffset(By sourceElementLocator, int xOffset, int yOffset) {
        return new Actions(driverFactoryHelper).dragAndDropByOffset(sourceElementLocator, xOffset, yOffset);
    }

    /**
     * Hovers over target element. If you want to hover on a webElement to expose
     * another webElement and click on it, use hoverAndClick instead for a more
     * reliable result.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions hover(By elementLocator) {
        return new Actions(driverFactoryHelper).hover(elementLocator);
    }

    /**
     * Hovers over the hoverElements in sequence then clicks the clickableElement
     *
     * @param hoverElementLocators    the list of locators of the webElements under
     *                                test upon which the hover action will be
     *                                performed in sequence (By xpath, id, selector,
     *                                name ...etc.)
     * @param clickableElementLocator the locator of the webElement under test upon
     *                                which the click action will be performed (By
     *                                xpath, id, selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions hoverAndClick(List<By> hoverElementLocators, By clickableElementLocator) {
        hoverElementLocators.forEach(this::hover);
        return click(clickableElementLocator);
    }

    /**
     * Selects an element from a dropdown list using its displayed text or attribute Value
     *
     * @param elementLocator     the locator of the webElement under test (By xpath, id,
     *                           selector, name ...etc.)
     * @param valueOrVisibleText the text of the choice that you need to select from the
     *                           target dropDown menu or the string value of attribute "value"
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions select(By elementLocator, String valueOrVisibleText) {
        return new Actions(driverFactoryHelper).select(elementLocator, valueOrVisibleText);
    }

    /**
     * Used to SetProperty value for an element (hidden or visible) using javascript
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @param value          the desired value that should be SetProperty for the target
     *                       element
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions setValueUsingJavaScript(By elementLocator, String value) {
        return new Actions(driverFactoryHelper).setValueUsingJavaScript(elementLocator, value);
    }

    /**
     * Used to submit a form using javascript
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions submitFormUsingJavaScript(By elementLocator) {
        return new Actions(driverFactoryHelper).submitFormUsingJavaScript(elementLocator);
    }

    /**
     * Switches focus to a certain iFrame, is mainly used in coordination with
     * {@link #switchToDefaultContent()} to navigate inside any iFrame
     * layer and go back to the main page
     *
     * @param elementLocator the locator of the iFrame webElement under test (By
     *                       xpath, id, selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions switchToIframe(By elementLocator) {
        return new Actions(driverFactoryHelper).switchToIframe(elementLocator);
    }

    /**
     * Switches focus to default content, is mainly used in coordination with
     * {@link #switchToIframe(By)} to exit any iFrame layer and go back
     * to the main page
     *
     * @return a self-reference to be used to chain actions
     */
    @SuppressWarnings("UnusedReturnValue")
    @Override
    public Actions switchToDefaultContent() {
        return new Actions(driverFactoryHelper).switchToDefaultContent();
    }

    /**
     * gets the current frame
     *
     * @return currentFrame the current frame name
     */
    @Override
    public String getCurrentFrame() {
        String currentFrame = (String) ((JavascriptExecutor) driverFactoryHelper.getDriver()).executeScript("return self.name");
        ReportManager.logDiscrete("Current frame name: \"" + currentFrame + "\"");
        return currentFrame;
    }

    /**
     * Types the provided text into the target element, replacing any existing value.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name, etc.)
     * @param text           one or more {@link CharSequence} values (including {@link org.openqa.selenium.Keys})
     *                       to be typed into the target webElement
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions type(By elementLocator, CharSequence... text) {
        return new Actions(driverFactoryHelper).type(elementLocator, text);
    }

    /**
     * Clears the value of the target element.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name, etc.)
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions clear(By elementLocator) {
        return new Actions(driverFactoryHelper).clear(elementLocator);
    }

    /**
     * Appends the required string into the target element, regardless of the
     * current text value.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @param text           the target text that needs to be appended into the
     *                       target webElement
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions typeAppend(By elementLocator, CharSequence... text) {
        return new Actions(driverFactoryHelper).typeAppend(elementLocator, text);
    }

    /**
     * ValidationEnums the required file path into an input[type='file'] button, to
     * successfully upload the target file.
     *
     * @param elementLocator the locator of the webElement under test (By xpath,
     *                       id, selector, name ...etc.)
     * @param filePath       the full path to the file that needs to be uploaded, it can be absolute or relative
     *                       path, Engine will detect that.
     * @return a self-reference to be used to chain actions.
     */
    @Override
    public Actions typeFileLocationForUpload(By elementLocator, String filePath) {
        return new Actions(driverFactoryHelper).typeFileLocationForUpload(elementLocator, filePath);
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element. Obfuscates the written text in the output
     * report. This action should be used for writing passwords and secure text.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     * @return a self-reference to be used to chain actions
     */
    @Override
    public Actions typeSecure(By elementLocator, CharSequence... text) {
        return new Actions(driverFactoryHelper).typeSecure(elementLocator, text);
    }

    /**
     * Get any simple table rows' data that has
     * thead which include all the column labels and tbody which includes all table data
     *
     * @param tableLocator the locator of the table which should be a table tag
     * @return List of Map format and each Map Object follows the following format (Key:column label, value: cell data)
     */
    @Override
    public List<Map<String, String>> getTableRowsData(By tableLocator) {
        List<Map<String, String>> tableData = new ArrayList<>();
        // Wait for the table to be present and visible
        WebDriverWait wait = new WebDriverWait(driverFactoryHelper.getDriver(), Duration.ofSeconds(10));

        try {
            wait.until(ExpectedConditions.visibilityOfElementLocated(tableLocator));
        } catch (Exception throwable) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), tableLocator, throwable);
            return null;
        }
        WebElement table = driverFactoryHelper.getDriver().findElement(tableLocator);
        try {
            //Wait until any row is loaded because some websites use lazy loading,
            //and you need to wait for rows to be loaded
            wait.until(ExpectedConditions.visibilityOfElementLocated(By.cssSelector("tbody tr")));
        } catch (Exception e) {
            ReportManager.logDiscrete("Table\"" + tableLocator + "\" is empty");
            //Will return empty list to be used in case you want to assert if the table is empty
            return new ArrayList<>();
        }
        List<WebElement> rows = table.findElement(By.tagName("tbody")).findElements(By.tagName("tr"));
        List<WebElement> headerCells = table.findElement(By.tagName("thead")).findElements(By.tagName("th"));

        //extract the data into a List of Maps
        for (WebElement row : rows) {
            WebElement currentRow = wait.until(ExpectedConditions.visibilityOf(row));
            List<WebElement> cells = row.findElements(By.tagName("td"));
            Map<String, String> rowData = new HashMap<>();
            for (int cellIndex = 0; cellIndex < cells.size(); cellIndex++) {
                String columnName = headerCells.get(cellIndex).getText();
                String cellValue = cells.get(cellIndex).getText();
                rowData.put(columnName, cellValue);
            }
            tableData.add(rowData);
        }
        return tableData;
    }

    /**
     * Captures a screenshot of the specified element and attaches it to the test report.
     *
     * @param elementLocator the locator of the element to screenshot
     * @return an {@link Actions} instance for chaining further actions
     * @see <a href="https://shaftengine.netlify.app/">SHAFT User Guide &ndash; Element Actions</a>
     */
    @Override
    public Actions captureScreenshot(By elementLocator) {
        return new Actions(driverFactoryHelper).captureScreenshot(elementLocator);
    }

}
