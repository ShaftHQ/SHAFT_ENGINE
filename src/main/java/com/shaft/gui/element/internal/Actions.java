package com.shaft.gui.element.internal;

import com.google.common.annotations.Beta;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.internal.exceptions.MultipleElementsFoundException;
import com.shaft.gui.internal.image.AnimatedGifManager;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.ScreenshotHelper;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.gui.internal.locator.SmartLocators;
import com.shaft.properties.internal.PropertiesHelper;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.internal.support.JavaScriptHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.appium.java_client.AppiumDriver;
import io.qameta.allure.Allure;
import io.qameta.allure.Step;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StatusDetails;
import lombok.NonNull;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.*;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.interactions.Locatable;
import org.openqa.selenium.interactions.Pause;
import org.openqa.selenium.interactions.PointerInput;
import org.openqa.selenium.interactions.Sequence;
import org.openqa.selenium.support.locators.RelativeLocator;
import org.openqa.selenium.support.pagefactory.ByAll;
import org.openqa.selenium.support.ui.Select;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicReferenceArray;
import java.util.function.Function;

/**
 * Provides a fluent API for performing low-level UI interactions on web and mobile elements.
 *
 * <p>This class extends {@link ElementActions} and adds concrete implementations for common
 * gestures such as clicking, typing, dragging, file uploading, and clipboard operations.
 * It integrates with SHAFT's reporting pipeline to attach screenshots and structured step
 * information to Allure reports automatically.
 *
 * <p>Actions instances are created internally by {@code SHAFT.GUI.WebDriver} and are not
 * intended to be constructed directly by test authors. Use the driver-level factory methods
 * instead:
 *
 * <pre>{@code
 * SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();
 * driver.element().click(By.id("submit"));
 * }</pre>
 *
 * <p><b>Thread safety:</b> Each {@code Actions} instance is tied to a single {@link WebDriver}
 * session. For parallel test execution, wrap driver instances in a {@link ThreadLocal}.
 */
public class Actions extends ElementActions {
    private static final Duration defaultPauseDuration = Duration.ofMillis(500);

    /**
     * Creates a new {@code Actions} instance using the driver managed by the current thread's
     * active {@link com.shaft.driver.internal.DriverFactory.DriverFactoryHelper}.
     */
    public Actions() {
        super();
    }

    /**
     * Creates a new {@code Actions} instance wrapping the given {@link WebDriver}.
     *
     * @param driver the WebDriver session to use for all element interactions
     */
    public Actions(WebDriver driver) {
        super(driver);
    }

    /**
     * Creates a new {@code Actions} instance wrapping the given {@link WebDriver},
     * with optional suppression of standard reporting output.
     *
     * @param driver   the WebDriver session to use for all element interactions
     * @param isSilent {@code true} to suppress log and report entries for each action;
     *                 {@code false} for normal reporting
     */
    public Actions(WebDriver driver, boolean isSilent) {
        super(driver, isSilent);
    }

    /**
     * Creates a new {@code Actions} instance backed by the given
     * {@link DriverFactoryHelper}, which provides access to driver configuration,
     * synchronization settings, and screenshot utilities.
     *
     * @param helper the helper that owns the underlying WebDriver session
     */
    public Actions(DriverFactoryHelper helper) {
        super(helper);
    }

    @Override
    public Actions and() {
        return this;
    }

    @Step("Hover")
    @Override
    public Actions hover(@NonNull By locator) {
        performAction(ActionType.HOVER, locator, null);
        return this;
    }

    @Step("Click")
    @Override
    public Actions click(@NonNull By locator) {
        performAction(ActionType.CLICK, locator, null);
        return this;
    }

    @Step("Click and hold")
    @Override
    public Actions clickAndHold(@NonNull By locator) {
        performAction(ActionType.CLICK_AND_HOLD, locator, null);
        return this;
    }

    @Step("Double click")
    @Override
    public Actions doubleClick(@NonNull By locator) {
        performAction(ActionType.DOUBLE_CLICK, locator, null);
        return this;
    }

    @Step("Click using JavaScript")
    @Override
    public Actions clickUsingJavascript(@NonNull By locator) {
        performAction(ActionType.JAVASCRIPT_CLICK, locator, null);
        return this;
    }

    @Step("Set value using JavaScript")
    @Override
    public Actions setValueUsingJavaScript(@NonNull By locator, @NonNull String value) {
        performAction(ActionType.JAVASCRIPT_SET_VALUE, locator, value);
        return this;
    }

    /**
     * Clicks a clickable element resolved by its visible label or accessible name.
     *
     * <p>This is a {@link Beta} smart-locator overload: SHAFT resolves the element
     * automatically using {@link SmartLocators#clickableField(String)}, so no explicit
     * {@link By} locator is required.
     *
     * <pre>{@code
     * driver.element().click("Submit");
     * }</pre>
     *
     * @param elementName the visible text, label, or accessible name of the target element
     * @return this {@code Actions} instance for fluent chaining
     */
    @Beta
    @Step("Click")
    public Actions click(@NonNull String elementName) {
        performAction(ActionType.CLICK, SmartLocators.clickableField(elementName), null);
        return this;
    }

    @Step("Type")
    @Override
    public Actions type(@NonNull By locator, @NonNull CharSequence... text) {
        performAction(ActionType.TYPE, locator, text);
        return this;
    }

    /**
     * Types the given text into an input field resolved by its visible label or placeholder.
     *
     * <p>This is a {@link Beta} smart-locator overload: SHAFT resolves the element
     * automatically using {@link SmartLocators#inputField(String)}, so no explicit
     * {@link By} locator is required. The field is cleared (according to the configured
     * clear-before-typing mode) before the text is entered.
     *
     * <pre>{@code
     * driver.element().type("Search", "SHAFT Engine");
     * }</pre>
     *
     * @param elementName the visible label or placeholder of the target input field
     * @param text        the character sequence(s) to type; supports {@link Keys} chords
     * @return this {@code Actions} instance for fluent chaining
     */
    @Beta
    @Step("Type")
    public Actions type(@NonNull String elementName, @NonNull CharSequence... text) {
        performAction(ActionType.TYPE, SmartLocators.inputField(elementName), text);
        return this;
    }

    @Step("Type securely")
    @Override
    public Actions typeSecure(@NonNull By locator, @NonNull CharSequence... text) {
        performAction(ActionType.TYPE_SECURELY, locator, text);
        return this;
    }

    @Step("Append")
    @Override
    public Actions typeAppend(@NonNull By locator, @NonNull CharSequence... text) {
        performAction(ActionType.TYPE_APPEND, locator, text);
        return this;
    }

    @Step("Clear")
    @Override
    public Actions clear(@NonNull By locator) {
        performAction(ActionType.CLEAR, locator, null);
        return this;
    }

    /**
     * Uploads a file by simulating a drag-and-drop gesture onto the target drop zone.
     *
     * <p>An invisible {@code <input type="file">} element is injected via JavaScript into the
     * drop zone. The file path is then sent to that injected input, which triggers the browser's
     * native file-upload mechanism without requiring the OS file-picker dialog.
     *
     * <pre>{@code
     * driver.element().dropFileToUpload(By.id("dropZone"), "src/test/resources/files/document.pdf");
     * }</pre>
     *
     * @param locator  the locator for the drop-zone element that accepts file drops
     * @param filePath the path to the local file to upload (relative or absolute)
     * @return this {@code Actions} instance for fluent chaining
     */
    @Step("Drop file to upload")
    public Actions dropFileToUpload(@NonNull By locator, @NonNull String filePath) {
        performAction(ActionType.DROP_FILE_TO_UPLOAD, locator, filePath);
        return this;
    }

    @Step("Drag and drop")
    @Override
    public Actions dragAndDrop(@NonNull By sourceElementLocator, @NonNull By destinationElementLocator) {
        performAction(ActionType.DRAG_AND_DROP, sourceElementLocator, destinationElementLocator);
        return this;
    }

    @Step("Drag and drop by offset")
    @Override
    public Actions dragAndDropByOffset(@NonNull By sourceElementLocator, int xOffset, int yOffset) {
        performAction(ActionType.DRAG_AND_DROP_BY_OFFSET, sourceElementLocator, new ArrayList<>(List.of(xOffset, yOffset)));
        return this;
    }

    /**
     * Returns a {@link GetElementInformation} builder that exposes read-only element
     * property accessors such as text, attributes, CSS values, and visibility state.
     *
     * <pre>{@code
     * String value = driver.element().get().text(By.id("username"));
     * }</pre>
     *
     * @return a new {@link GetElementInformation} instance scoped to this driver session
     */
    public GetElementInformation get() {
        return new GetElementInformation();
    }

    /**
     * Returns a {@link ClipboardAction} builder that exposes clipboard operations
     * (copy, cut, paste, delete) for a target element.
     *
     * <pre>{@code
     * driver.element().clipboard().copyAll(By.id("textField"));
     * }</pre>
     *
     * @return a new {@link ClipboardAction} instance scoped to this driver session
     */
    public ClipboardAction clipboard() {
        return new ClipboardAction(this);
    }

    /**
     * Waits until the given condition returns {@code true}, using the framework's default
     * element-identification timeout multiplied by ten seconds.
     *
     * <p>Delegates to {@link #waitUntil(Function, Duration)} with a computed timeout derived
     * from {@code SHAFT.Properties.timeouts.defaultElementIdentificationTimeout()}.
     *
     * <pre>{@code
     * driver.element().waitUntil(ExpectedConditions.titleContains("Dashboard"));
     * }</pre>
     *
     * @param isTrue a WebDriver condition lambda; must return a truthy value when satisfied
     * @return this {@code Actions} instance for fluent chaining
     */
    public Actions waitUntil(@NonNull Function<? super WebDriver, ?> isTrue) {
        return waitUntil(isTrue, Duration.ofSeconds((long) (SHAFT.Properties.timeouts.defaultElementIdentificationTimeout()) * 10));
    }

    /**
     * Waits until the given condition returns {@code true} within the specified timeout.
     *
     * <p>Uses a fluent wait backed by the current driver session. If the condition is not
     * satisfied before {@code timeout} elapses, a {@link TimeoutException} is caught and
     * the step is marked as broken in the Allure report.
     *
     * <pre>{@code
     * driver.element().waitUntil(
     *     ExpectedConditions.visibilityOfElementLocated(By.id("modal")),
     *     Duration.ofSeconds(30)
     * );
     * }</pre>
     *
     * @param isTrue  a WebDriver condition lambda; must return a truthy value when satisfied
     * @param timeout the maximum time to wait before failing
     * @return this {@code Actions} instance for fluent chaining
     */
    @Step("Wait until")
    public Actions waitUntil(@NonNull Function<? super WebDriver, ?> isTrue, @NonNull Duration timeout) {
        String output = "";
        try {
            output = String.valueOf(new SynchronizationManager(driverFactoryHelper.getDriver()).fluentWait().withTimeout(timeout).until(isTrue));
            if (!"true".equalsIgnoreCase(output))
                throw new TimeoutException("Condition was not met within the timeout period.");
        } catch (WebDriverException exception) {
            if (output.isEmpty())
                output = "custom lambda expression";
            reportBroken("waitUntil", output, takeFailureScreenshot(null), exception);
        }
        //report pass
        var description = String.valueOf(isTrue);
        output = description.contains(" ") ? description : "custom lambda expression";

        reportPass("waitUntil", output, null);
        return this;
    }

    protected String performAction(ActionType action, By locator, Object data) {
        AtomicReference<String> output = new AtomicReference<>("");
        AtomicReference<String> accessibleName = new AtomicReference<>(JavaHelper.formatLocatorToString(locator));
        AtomicReferenceArray<byte[]> screenshot = new AtomicReferenceArray<>(1);
        AtomicReference<List<WebElement>> foundElements = new AtomicReference<>();

        try {
            new SynchronizationManager(driverFactoryHelper.getDriver()).fluentWait(true).until(d -> {
                // find all elements matching the target locator
                foundElements.set(findAllElements(locator));

                // fail fast if no elements were found
                if (foundElements.get().isEmpty())
                    throw new NoSuchElementException("Cannot locate an element using " + JavaHelper.formatLocatorToString(locator));

                // ensure element locator is unique if applicable
                if (foundElements.get().size() > 1 && SHAFT.Properties.flags.forceCheckElementLocatorIsUnique() && !(locator instanceof RelativeLocator.RelativeBy) && !(locator instanceof ByAll))
                    throw new MultipleElementsFoundException();

                // identify run type
                boolean isMobileNativeExecution = DriverFactoryHelper.isMobileNativeExecution();

                // get accessible name if needed
                if (SHAFT.Properties.reporting.captureElementName()) {
                    String fetchedName;
                    if (!isMobileNativeExecution) {
                        try {
                            fetchedName = foundElements.get().getFirst().getAccessibleName();
                        } catch (UnsupportedCommandException | StaleElementReferenceException throwable) {
                            //happens on some elements that show unhandled inspector error
                            //this exception is thrown on some older selenium grid instances, I saw it with firefox running over selenoid
                            //ignore
                            //saw it again with mobile web tests
                            // the stale was thrown in an iframe
                            fetchedName = foundElements.get().getFirst().getDomProperty("text");
                        }
                    } else {
                        fetchedName = foundElements.get().getFirst().getAttribute("name");
                    }
                    if (fetchedName != null && !fetchedName.isEmpty())
                        accessibleName.set(fetchedName.trim());
                }

                if (!isMobileNativeExecution) {
                // scroll to element (avoid relocating the element if already found)
                switch (SHAFT.Properties.flags.scrollingMode().toLowerCase()){
                    case "w3c" -> {
                        try {
                            // w3c compliant scroll
                            new org.openqa.selenium.interactions.Actions(d).scrollToElement(foundElements.get().getFirst()).perform();
                        } catch (Throwable throwable1) {
                            // old school selenium scroll
                            if (d instanceof Locatable locatable) {
                                locatable.getCoordinates().inViewPort();
                            } else if (d instanceof JavascriptExecutor) {
                                // native Javascript scroll to center (smooth / auto)
                                ((JavascriptExecutor) d).executeScript("""
                                    arguments[0].scrollIntoView({behavior: "auto", block: "center", inline: "center"});""", foundElements.get().getFirst());
                            }
                        }
                    }
                    case "javascript" -> {
                        try {
                            // native Javascript scroll to center (smooth / auto)
                            ((JavascriptExecutor) d).executeScript("""
                                arguments[0].scrollIntoView({behavior: "auto", block: "center", inline: "center"});""", foundElements.get().getFirst());
                        } catch (Throwable throwable) {
                            try {
                                // w3c compliant scroll
                                new org.openqa.selenium.interactions.Actions(d).scrollToElement(foundElements.get().getFirst()).perform();
                            } catch (Throwable throwable1) {
                                // old school selenium scroll
                                if (d instanceof Locatable locatable)
                                    locatable.getCoordinates().inViewPort();
                            }
                        }
                    }
                    case "legacy" -> {
                        if (d instanceof Locatable locatable) {
                            locatable.getCoordinates().inViewPort();
                        } else if (d instanceof JavascriptExecutor) {
                            // native Javascript scroll to center (smooth / auto)
                            ((JavascriptExecutor) d).executeScript("""
                                    arguments[0].scrollIntoView({behavior: "auto", block: "center", inline: "center"});""", foundElements.get().getFirst());
                        }
                    }
                }
                }

                //wait for lazy loading
                JavaScriptWaitManager.waitForLazyLoading(d);
                // perform action
                switch (action) {
                    case HOVER ->
                            (new org.openqa.selenium.interactions.Actions(d)).pause(defaultPauseDuration).moveToElement(foundElements.get().getFirst()).perform();
                    case CLICK -> {
                        try {
                            screenshot.set(0, takeActionScreenshot(foundElements.get().getFirst()));
                            foundElements.get().getFirst().click();
                        } catch (InvalidElementStateException exception) {
                            if (SHAFT.Properties.flags.clickUsingJavascriptWhenWebDriverClickFails()) {
                                ((JavascriptExecutor) d).executeScript("arguments[0].click();", foundElements.get().getFirst());
                                ReportManager.logDiscrete("Performed Click using JavaScript; If the report is showing that the click passed but you observe that no action was taken, we recommend trying a different element locator.");
                            } else {
                                throw exception;
                            }
                        }
                    }
                    case JAVASCRIPT_CLICK -> {
                        screenshot.set(0, takeActionScreenshot(foundElements.get().getFirst()));
                        ((JavascriptExecutor) d).executeScript("arguments[0].click();", foundElements.get().getFirst());
                    }
                    case CLICK_AND_HOLD -> {
                        screenshot.set(0, takeActionScreenshot(foundElements.get().getFirst()));
                        new org.openqa.selenium.interactions.Actions(d).pause(defaultPauseDuration).clickAndHold(foundElements.get().getFirst()).perform();
                    }
                    case DOUBLE_CLICK -> {
                        var targetElement = foundElements.get().getFirst();
                        screenshot.set(0, takeActionScreenshot(targetElement));

                        if (d instanceof AppiumDriver appiumDriver) {
                            PointerInput.Origin VIEW = PointerInput.Origin.viewport();
                            Rectangle targetElementRectangle = targetElement.getRect();
                            PointerInput finger = new PointerInput(PointerInput.Kind.TOUCH, "finger");
                            Sequence doubleTap = new Sequence(finger, 0);

                            // move to element
                            doubleTap.addAction(finger.createPointerMove(defaultPauseDuration, VIEW,
                                    targetElementRectangle.getWidth() / 2, targetElementRectangle.getHeight() / 2));

                            // first tap
                            doubleTap.addAction(finger.createPointerDown(PointerInput.MouseButton.LEFT.asArg()));
                            doubleTap.addAction(finger.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));

                            // pause
                            doubleTap.addAction(new Pause(finger, defaultPauseDuration));

                            // second tap
                            doubleTap.addAction(finger.createPointerDown(PointerInput.MouseButton.LEFT.asArg()));
                            doubleTap.addAction(finger.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));

                            // perform
                            appiumDriver.perform(List.of(doubleTap));
                        } else {
                            new org.openqa.selenium.interactions.Actions(d).pause(defaultPauseDuration)
                                    .doubleClick(targetElement)
                                    .perform();
                        }
                    }
                    case TYPE, TYPE_SECURELY -> {
                        PropertiesHelper.setClearBeforeTypingMode();
                        executeClearBasedOnClearMode(foundElements.get().getFirst(), SHAFT.Properties.flags.clearBeforeTypingMode());
                        foundElements.get().getFirst().sendKeys((CharSequence[]) data);
                    }
                    case TYPE_APPEND -> {
                        foundElements.get().getFirst().sendKeys((CharSequence[]) data);
                    }
                    case JAVASCRIPT_SET_VALUE ->
                            ((JavascriptExecutor) d).executeScript("arguments[0].value = arguments[1];", foundElements.get().getFirst(), data);
                    case CLEAR -> {
                        executeClearBasedOnClearMode(foundElements.get().getFirst(), "native");
                        if (!"".equals(foundElements.get().getFirst().getDomProperty("value")))
                            executeClearBasedOnClearMode(foundElements.get().getFirst(), "backspace");
                    }
                    case DRAG_AND_DROP -> new org.openqa.selenium.interactions.Actions(d).pause(defaultPauseDuration)
                                    .dragAndDrop(foundElements.get().getFirst(),
                                            ElementActionsHelper.safeFindElement(d, (By) data)).perform();
                    case DRAG_AND_DROP_BY_OFFSET ->
                            new org.openqa.selenium.interactions.Actions(d).pause(defaultPauseDuration)
                                    .dragAndDropBy(foundElements.get().getFirst(),
                                            (int) ((ArrayList<?>) data).get(0),
                                            (int) ((ArrayList<?>) data).get(1)).perform();
                    case DROP_FILE_TO_UPLOAD -> {
                        // Prepare target file to be uploaded
                        File file = new File((String) data);
                        if(!file.exists())
                            throw new RuntimeException("File not found: " + data);
                        String absoluteFilePath = file.getAbsoluteFile().toString();

                        // Prepare custom script to inject the upload input field with required parameters
                        double offsetX = 0;
                        double offsetY = 0;

                        // Inject upload input field
                        WebElement input;
                        JavascriptExecutor javascriptExecutor = (JavascriptExecutor) d;
                        input = (WebElement) javascriptExecutor.executeScript(JavaScriptHelper.INJECT_INPUT_TO_UPLOAD_FILE_VIA_DROP_ACTION.getValue(), foundElements.get().getFirst(), offsetX, offsetY);

                        // Ensure that the upload input field was created successfully
                        assert input != null;

                        // Use upload input field to send the file location for upload
                        input.sendKeys(absoluteFilePath);
                    }
                    case GET_ATTRIBUTE -> output.set(foundElements.get().getFirst().getAttribute((String) data));
                    case GET_DOM_ATTRIBUTE -> output.set(foundElements.get().getFirst().getDomAttribute((String) data));
                    case GET_DOM_PROPERTY -> output.set(foundElements.get().getFirst().getDomProperty((String) data));
                    case GET_CSS_VALUE -> output.set(foundElements.get().getFirst().getCssValue((String) data));
                    case GET_NAME -> output.set(foundElements.get().getFirst().getAccessibleName());
                    case GET_IS_DISPLAYED -> output.set(String.valueOf(foundElements.get().getFirst().isDisplayed()));
                    case GET_IS_ENABLED -> output.set(String.valueOf(foundElements.get().getFirst().isEnabled()));
                    case GET_IS_SELECTED -> output.set(String.valueOf(foundElements.get().getFirst().isSelected()));
                    case GET_TEXT -> {
                        output.set(foundElements.get().getFirst().getText());
                        if ((output.get() == null || output.get().isBlank()) && !DriverFactoryHelper.isMobileNativeExecution()) {
                            output.set(foundElements.get().getFirst().getDomProperty(ElementActionsHelper.TextDetectionStrategy.CONTENT.getValue()));
                            if (output.get() == null || output.get().isBlank()) {
                                output.set(foundElements.get().getFirst().getDomProperty(ElementActionsHelper.TextDetectionStrategy.VALUE.getValue()));
                            }
                        }
                        if (output.get() == null) {
                            output.set("");
                        }
                    }
                    case GET_SELECTED_TEXT -> {
                        StringBuilder elementSelectedText = new StringBuilder();
                        new Select(foundElements.get().getFirst()).getAllSelectedOptions().forEach(selectedOption -> elementSelectedText.append(selectedOption.getText()));
                        output.set(elementSelectedText.toString());
                    }
                    case CLIPBOARD_COPY ->
                            foundElements.get().getFirst().sendKeys(Keys.chord(Keys.CONTROL, "a"), Keys.chord(Keys.CONTROL, "c"), Keys.ARROW_RIGHT);
                    case CLIPBOARD_CUT ->
                            foundElements.get().getFirst().sendKeys(Keys.chord(Keys.CONTROL, "a"), Keys.chord(Keys.CONTROL, "x"), Keys.ARROW_RIGHT);
                    case CLIPBOARD_PASTE ->
                            foundElements.get().getFirst().sendKeys(Keys.chord(Keys.CONTROL, "v"), Keys.ARROW_RIGHT);
                    case CLIPBOARD_DELETE ->
                            foundElements.get().getFirst().sendKeys(Keys.chord(Keys.CONTROL, "a"), Keys.BACK_SPACE, Keys.ARROW_RIGHT);
                }

                // take screenshot if not already taken before action
                if (screenshot.get(0) == null)
                    screenshot.set(0, takeActionScreenshot(foundElements.get().getFirst()));
                return true;
            });
        } catch (WebDriverException exception) {
            // take failure screenshot if needed
            if (screenshot.get(0) == null) {
                try {
                    if (foundElements.get() == null || foundElements.get().size() != 1) {
                        screenshot.set(0, takeFailureScreenshot(null));
                    } else {
                        screenshot.set(0, takeFailureScreenshot(foundElements.get().getFirst()));
                    }
                    // report broken
                    reportBroken(action.name(), accessibleName.get(), screenshot.get(0), exception);
                } catch (RuntimeException exception2) {
                    if (exception2.getCause() == null || !exception2.getCause().equals(exception)) {
                        // in case a new exception was thrown while attempting to take a screenshot
                        exception2.addSuppressed(exception);
                        // report broken
                        reportBroken(action.name(), accessibleName.get(), screenshot.get(0), exception2);
                    } else {
                        // in case no new exceptions where thrown, just the one created by SHAFT for the main issue
                        throw exception2;
                    }
                }
            }
        }
        //report pass
        reportPass(action.name(), accessibleName.get(), screenshot.get(0));
        return output.get();
    }

    private void executeClearBasedOnClearMode(WebElement elem, String clearMode) {
        switch (clearMode) {
            case "native":
                elem.click();
                elem.clear();
                break;
            case "backspace":
                String text = parseElementText(elem);
                if (!text.isEmpty())
                    elem.sendKeys(String.valueOf(Keys.BACK_SPACE).repeat(text.length()));
                break;
            case "off":
                break;
        }
    }

    private String parseElementText(WebElement element) {
        String text = element.getText();
        if (!text.isEmpty())
            return text;
        text = element.getDomProperty("value");
        if (text != null && !text.isEmpty())
            return text;
        text = element.getDomProperty("textContent");
        if (text != null && !text.isEmpty())
            return text;
        text = element.getDomProperty("innerHTML");
        if (text != null && !text.isEmpty() && !text.contains("<"))
            return text;
        return "";
    }

    private List<WebElement> findAllElements(By locator) {
        List<WebElement> foundElements;

        By shadowDomLocator = ShadowLocatorBuilder.shadowDomLocator.get();
        By cssSelector = ShadowLocatorBuilder.cssSelector.get();

        if (shadowDomLocator != null && cssSelector == locator) {
            //reset to default content
            driverFactoryHelper.getDriver().switchTo().defaultContent();
            //switch to shadow root and find elements
            foundElements = driverFactoryHelper.getDriver().findElement(shadowDomLocator)
                    .getShadowRoot()
                    .findElements(cssSelector);
        } else if (LocatorBuilder.getIFrameLocator().get() != null) {
            //reset to default content
            driverFactoryHelper.getDriver().switchTo().defaultContent();
            //switch to frame and find elements
            foundElements = driverFactoryHelper.getDriver().switchTo()
                    .frame(driverFactoryHelper.getDriver().findElement(LocatorBuilder.getIFrameLocator().get()))
                    .findElements(locator);
        } else {
            //normal case, just find the elements
            foundElements = ElementActionsHelper.safeFindElements(driverFactoryHelper.getDriver(), locator);
        }
        return foundElements;
    }

    private byte[] takeActionScreenshot(WebElement element) {
        if (SHAFT.Properties.visuals.createAnimatedGif() || "Always".equals(SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot()))
            return captureScreenshot(element, true);
        return null;
    }

    private byte[] takeFailureScreenshot(WebElement element) {
        return captureScreenshot(element, false);
    }

    private byte[] captureScreenshot(WebElement element, boolean isPass) {
        // capture screenshot
        byte[] screenshot;
        if (element != null && SHAFT.Properties.visuals.screenshotParamsHighlightElements()) {
            if ("JavaScript".equals(SHAFT.Properties.visuals.screenshotParamsHighlightMethod())) {
                // take screenshot, apply watermark and append it to gif before removing javascript highlighting
                screenshot = takeJavaScriptHighlightedScreenshot(element, isPass);
            } else {
                screenshot = takeAIHighlightedScreenshot(element, isPass);
                // append screenshot to animated gif
                AnimatedGifManager.startOrAppendToAnimatedGif(screenshot);
            }
        } else {
            screenshot = takeScreenshot(element);
            // append screenshot to animated gif
            AnimatedGifManager.startOrAppendToAnimatedGif(screenshot);
        }

        return screenshot;
    }

    private byte[] appendShaftWatermark(byte[] screenshot) {
        try {
            // add SHAFT_Engine logo overlay
            BufferedImage screenshotImage = ImageIO.read(new ByteArrayInputStream(screenshot));
            ScreenshotHelper.overlayShaftEngineLogo(screenshotImage);
            ByteArrayOutputStream screenshotOutputStream = new ByteArrayOutputStream();
            ImageIO.write(screenshotImage, "png", screenshotOutputStream);
            return screenshotOutputStream.toByteArray();
        } catch (IOException e) {
            ReportManagerHelper.logDiscrete(e);
            return screenshot;
        }
    }

    private byte[] takeScreenshot(WebElement element) {
        byte[] screenshot;
        switch (Screenshots.getType()) {
            case FULL -> {
                try {
                    if (!SHAFT.Properties.visuals.screenshotParamsSkippedElementsFromScreenshot().isEmpty()) {
                        // handle elements that should be skipped from screenshot
                        List<WebElement> skippedElementsList = new ArrayList<>();
                        String[] skippedElementLocators = SHAFT.Properties.visuals.screenshotParamsSkippedElementsFromScreenshot().split(";");
                        for (String locator : skippedElementLocators) {
                            if (elementActionsHelper.getElementsCount(driverFactoryHelper.getDriver(), By.xpath(locator)) == 1) {
                                skippedElementsList.add(((WebElement) elementActionsHelper.identifyUniqueElementIgnoringVisibility(driverFactoryHelper.getDriver(), By.xpath(locator)).get(1)));
                            }
                        }
                        WebElement[] skippedElementsArray = new WebElement[skippedElementsList.size()];
                        skippedElementsArray = skippedElementsList.toArray(skippedElementsArray);
                        screenshot = ScreenshotHelper.makeFullScreenshot(driverFactoryHelper.getDriver(), skippedElementsArray);
                    } else {
                        // make full page screenshot using BiDi, CDP, or manually based on the target browser
                        screenshot = ScreenshotHelper.makeFullScreenshot(driverFactoryHelper.getDriver());
                    }
                } catch (Throwable throwable) {
                    // return regular screenshot in case of failure
                    ReportManagerHelper.logDiscrete(throwable);
                    screenshot = ((TakesScreenshot) driverFactoryHelper.getDriver()).getScreenshotAs(OutputType.BYTES);
                }
            }
            case ELEMENT -> {
                try {
                    //get element screenshot
                    screenshot = element.getScreenshotAs(OutputType.BYTES);
                } catch (Throwable throwable) {
                    // return regular screenshot in case of failure
                    ReportManagerHelper.logDiscrete(throwable);
                    screenshot = ((TakesScreenshot) driverFactoryHelper.getDriver()).getScreenshotAs(OutputType.BYTES);
                }
            }
            default -> screenshot = ((TakesScreenshot) driverFactoryHelper.getDriver()).getScreenshotAs(OutputType.BYTES);
        }
        //append shaft watermark
        screenshot = appendShaftWatermark(screenshot);
        return screenshot;
    }

    private byte[] takeAIHighlightedScreenshot(WebElement element, boolean isPass) {
        // getElementLocation
        Rectangle elementLocation = element.getRect();

        //take screenshot
        byte[] src = takeScreenshot(element);

        //highlightElement using OpenCV
        Color color;
        if (isPass) {
            color = new Color(67, 176, 42); // selenium-green
        } else {
            color = new Color(255, 255, 153); // yellow
        }
        src = ImageProcessingActions.highlightElementInScreenshot(src, elementLocation, color);
        return src;
    }

    private byte[] takeJavaScriptHighlightedScreenshot(WebElement element, boolean isPass) {
        //highlightElement
        JavascriptExecutor js = (JavascriptExecutor) driverFactoryHelper.getDriver();
        String regularElementStyle = highlightElementAndReturnDefaultStyle(driverFactoryHelper.getDriver(), element, js,
                setHighlightedElementStyle(isPass));

        //take screenshot
        byte[] src = takeScreenshot(element);

        //append highlighted element screenshot to GIF
        AnimatedGifManager.startOrAppendToAnimatedGif(src);
        //resetElementStyle
        if (SHAFT.Properties.visuals.screenshotParamsHighlightMethod().equals("JavaScript") && js != null) {
            js.executeScript("arguments[0].setAttribute('style', arguments[1]);", element, regularElementStyle);
        }
        return src;
    }

    private String highlightElementAndReturnDefaultStyle(WebDriver driver, WebElement element, JavascriptExecutor js,
                                                         String highlightedElementStyle) {
        String regularElementStyle = element.getDomProperty("style");
        if (regularElementStyle != null && !regularElementStyle.isEmpty()) {
            js.executeScript("arguments[0].style.cssText = arguments[1];", element,
                    regularElementStyle + highlightedElementStyle);
        } else {
            js.executeScript("arguments[0].setAttribute('style', arguments[1]);", element, highlightedElementStyle);
        }

        JavaScriptWaitManager.waitForLazyLoading(driver);
        return regularElementStyle;
    }

    private String setHighlightedElementStyle(boolean isPass) {
        String background;
        String backgroundColor;

        if (isPass) {
            background = "#46aad2";
            backgroundColor = "#A5D2A5";
        } else {
            background = "#FFFF99";
            backgroundColor = "#FFFF99";
        }
        return "outline-offset:-3px !important; outline:3px solid #808080 !important; background:" + background
                + " !important; background-color:" + backgroundColor
                + " !important; color:#000000 !important; -webkit-transition: none !important; -moz-transition: none !important; -o-transition: none !important; transition: none !important;";

    }

    private void reportPass(String action, String elementName, byte[] screenshot) {
        report(action, elementName, Status.PASSED, screenshot, null);
    }

    private void reportBroken(String action, String elementName, byte[] screenshot, RuntimeException exception) {
        report(action, elementName, Status.BROKEN, screenshot, exception);
    }

    private void report(String action, String elementName, Status status, byte[] screenshot, RuntimeException exception) {
        // update allure step name
        StringBuilder stepName = new StringBuilder();
        stepName.append(JavaHelper.convertToSentenceCase(action)).append(" \"").append(elementName).append("\"");

        if (!status.equals(Status.PASSED))
            stepName.append(" is ").append(status.name().toLowerCase());

        Allure.getLifecycle().updateStep(update -> update.setName(stepName.toString()));

        // handle secure typing
        if (ActionType.TYPE_SECURELY.name().equals(action))
            Allure.getLifecycle().updateStep(update -> {
                var params = update.getParameters();
                params.forEach(parameter -> {
                    if (parameter.getName().equals("text")) {
                        parameter.setValue("********");
                    }
                });
                update.setParameters(params);
            });

        // attach screenshot
        if (screenshot != null)
            Allure.addAttachment(new SimpleDateFormat("yyyyMMdd_HHmmss").format(Calendar.getInstance().getTime()) + "_" + JavaHelper.convertToSentenceCase(action) + "_" + JavaHelper.removeSpecialCharacters(elementName), "image/png", new ByteArrayInputStream(screenshot), ".png");

        // handle reporting based on status
        if (Status.PASSED.equals(status)) {
            // if the step passed
            ReportManager.logDiscrete(stepName.toString());
        } else {
            // if the step failed
            ReportManager.logDiscrete(stepName.toString(), Level.ERROR);

            // update allure step status to broken
            Allure.getLifecycle().updateStep(update -> update.setStatus(status));

            // update test status to failed
            Allure.getLifecycle().updateTestCase(update -> update.setStatus(Status.FAILED));

            if (exception != null) {
                // update allure stacktrace
                Allure.getLifecycle().updateStep(update -> {
                    var trace = update.getStatusDetails() == null ? exception : update.getStatusDetails().getTrace() + System.lineSeparator() + exception;
                    StatusDetails details = update.getStatusDetails() == null ? new StatusDetails() : update.getStatusDetails();
                    details.setTrace(trace.toString().trim());
                    update.setStatusDetails(details);
                });
                throw new RuntimeException(FailureReporter.getRootCause(exception).trim(), exception);
            }
        }
    }

    protected enum ActionType {HOVER, CLICK, JAVASCRIPT_CLICK, TYPE, TYPE_SECURELY, TYPE_APPEND, JAVASCRIPT_SET_VALUE, CLEAR, DRAG_AND_DROP, GET_ATTRIBUTE, GET_DOM_ATTRIBUTE, GET_DOM_PROPERTY, GET_NAME, GET_TEXT, GET_CSS_VALUE, GET_IS_DISPLAYED, GET_IS_ENABLED, DRAG_AND_DROP_BY_OFFSET, GET_SELECTED_TEXT, CLICK_AND_HOLD, DOUBLE_CLICK, GET_IS_SELECTED, CLIPBOARD_DELETE, CLIPBOARD_COPY, CLIPBOARD_CUT, CLIPBOARD_PASTE, DROP_FILE_TO_UPLOAD}

    /**
     * Provides read-only accessors for querying properties, attributes, and state of a
     * located web element.
     *
     * <p>Obtain an instance via {@link Actions#get()}:
     *
     * <pre>{@code
     * String title = driver.element().get().attribute(By.id("header"), "title");
     * boolean visible = driver.element().get().isDisplayed(By.id("banner"));
     * }</pre>
     */
    public class GetElementInformation {
        /**
         * Creates a new {@code GetElementInformation} instance scoped to the enclosing
         * {@link Actions} session.
         */
        protected GetElementInformation() {
        }

        /**
         * Returns the value of the named HTML attribute from the located element.
         *
         * <pre>{@code
         * String href = driver.element().get().attribute(By.cssSelector("a.logo"), "href");
         * }</pre>
         *
         * @param locator       the locator for the target element
         * @param attributeName the name of the HTML attribute to read
         * @return the attribute value, or {@code null} if the attribute is absent
         */
        @Step("Get Attribute")
        public String attribute(@NonNull By locator, @NonNull String attributeName) {
            return performAction(ActionType.GET_ATTRIBUTE, locator, attributeName);
        }

        /**
         * Returns the value of the named DOM attribute from the located element.
         *
         * <p>Unlike {@link #attribute(By, String)}, this method reads the live DOM attribute
         * value rather than the initial HTML attribute value.
         *
         * @param locator       the locator for the target element
         * @param attributeName the name of the DOM attribute to read
         * @return the DOM attribute value, or {@code null} if the attribute is absent
         */
        @Step("Get DOM attribute")
        public String domAttribute(@NonNull By locator, @NonNull String attributeName) {
            return performAction(ActionType.GET_DOM_ATTRIBUTE, locator, attributeName);
        }

        /**
         * Returns the value of the named DOM property from the located element.
         *
         * <p>DOM properties reflect the current JavaScript object state (e.g., {@code value}
         * for an input) rather than the original HTML attribute.
         *
         * @param locator      the locator for the target element
         * @param propertyName the name of the DOM property to read (e.g., {@code "value"})
         * @return the property value as a string, or {@code null} if the property is absent
         */
        @Step("Get DOM property")
        public String domProperty(@NonNull By locator, @NonNull String propertyName) {
            return performAction(ActionType.GET_DOM_PROPERTY, locator, propertyName);
        }

        /**
         * Returns the accessible name of the located element as computed by the browser's
         * accessibility tree.
         *
         * @param locator the locator for the target element
         * @return the accessible name string, or an empty string if none is defined
         */
        @Step("Get name")
        public String name(@NonNull By locator) {
            return performAction(ActionType.GET_NAME, locator, null);
        }

        /**
         * Returns the visible inner text of the located element.
         *
         * <p>Falls back to the {@code textContent} DOM property and then the {@code value}
         * DOM property if the element's visible text is blank (e.g., for inputs).
         *
         * @param locator the locator for the target element
         * @return the trimmed visible text; never {@code null} (returns an empty string if
         *         no text is found)
         */
        @Step("Get text")
        public String text(@NonNull By locator) {
            return performAction(ActionType.GET_TEXT, locator, null);
        }

        /**
         * Returns the combined text of all currently selected {@code <option>} elements
         * inside a {@code <select>} element.
         *
         * @param locator the locator for the {@code <select>} element
         * @return the concatenated text of all selected options
         */
        @Step("Get selected text")
        public String selectedText(@NonNull By locator) {
            return performAction(ActionType.GET_SELECTED_TEXT, locator, null);
        }

        /**
         * Returns the computed CSS value of the named property for the located element.
         *
         * <pre>{@code
         * String color = driver.element().get().cssValue(By.id("title"), "color");
         * }</pre>
         *
         * @param locator      the locator for the target element
         * @param propertyName the CSS property name (e.g., {@code "background-color"})
         * @return the computed CSS value as a string
         */
        @Step("Get CSS value")
        public String cssValue(@NonNull By locator, @NonNull String propertyName) {
            return performAction(ActionType.GET_CSS_VALUE, locator, propertyName);
        }

        /**
         * Returns {@code true} if the located element is currently displayed on the page.
         *
         * @param locator the locator for the target element
         * @return {@code true} if the element is visible; {@code false} otherwise
         */
        @Step("Get is displayed")
        public boolean isDisplayed(@NonNull By locator) {
            return Boolean.parseBoolean(performAction(ActionType.GET_IS_DISPLAYED, locator, null));
        }

        /**
         * Returns {@code true} if the located element is currently enabled (i.e., interactive).
         *
         * @param locator the locator for the target element
         * @return {@code true} if the element is enabled; {@code false} if it is disabled
         */
        @Step("Get is enabled")
        public boolean isEnabled(@NonNull By locator) {
            return Boolean.parseBoolean(performAction(ActionType.GET_IS_ENABLED, locator, null));
        }

        /**
         * Returns {@code true} if the located element (e.g., a checkbox or radio button)
         * is currently selected.
         *
         * @param locator the locator for the target element
         * @return {@code true} if the element is selected; {@code false} otherwise
         */
        @Step("Get is selected")
        public boolean isSelected(@NonNull By locator) {
            return Boolean.parseBoolean(performAction(ActionType.GET_IS_SELECTED, locator, null));
        }
    }

    /**
     * Provides keyboard-shortcut-based clipboard operations for a target element.
     *
     * <p>Each method selects all text in the element first (Ctrl+A) before performing the
     * requested clipboard operation, then moves focus away (Arrow Right). Obtain an instance
     * via {@link Actions#clipboard()}:
     *
     * <pre>{@code
     * driver.element().clipboard().copyAll(By.id("sourceField"))
     *       .and().clipboard().paste(By.id("destinationField"));
     * }</pre>
     */
    public class ClipboardAction {
        private final Actions actions;

        private ClipboardAction() {
            actions = null;
        }

        /**
         * Creates a {@code ClipboardAction} associated with the given {@link Actions} instance
         * so that chained calls can continue on the same {@code Actions} object.
         *
         * @param actions the enclosing {@link Actions} instance to return from each operation
         */
        protected ClipboardAction(Actions actions) {
            this.actions = actions;
        }

        /**
         * Selects all text inside the located element and deletes it (Ctrl+A, Backspace).
         *
         * <pre>{@code
         * driver.element().clipboard().deleteAll(By.id("notes"));
         * }</pre>
         *
         * @param locator the locator for the target element whose text should be deleted
         * @return the parent {@link Actions} instance for fluent chaining
         */
        @Step("Delete text")
        public Actions deleteAll(@NonNull By locator) {
            performAction(ActionType.CLIPBOARD_DELETE, locator, null);
            return this.actions;
        }

        /**
         * Selects all text inside the located element and copies it to the system clipboard
         * (Ctrl+A, Ctrl+C).
         *
         * <pre>{@code
         * driver.element().clipboard().copyAll(By.id("output"));
         * }</pre>
         *
         * @param locator the locator for the target element whose text should be copied
         * @return the parent {@link Actions} instance for fluent chaining
         */
        @Step("Copy text to clipboard")
        public Actions copyAll(@NonNull By locator) {
            performAction(ActionType.CLIPBOARD_COPY, locator, null);
            return this.actions;
        }

        /**
         * Selects all text inside the located element, cuts it to the system clipboard
         * (Ctrl+A, Ctrl+X), and leaves the field empty.
         *
         * @param locator the locator for the target element whose text should be cut
         * @return the parent {@link Actions} instance for fluent chaining
         */
        @Step("Cut text to clipboard")
        public Actions cutAll(@NonNull By locator) {
            performAction(ActionType.CLIPBOARD_CUT, locator, null);
            return this.actions;
        }

        /**
         * Pastes the current system clipboard contents into the located element (Ctrl+V).
         *
         * <pre>{@code
         * driver.element().clipboard().copyAll(By.id("source"))
         *       .and().clipboard().paste(By.id("destination"));
         * }</pre>
         *
         * @param locator the locator for the target element that should receive the paste
         * @return the parent {@link Actions} instance for fluent chaining
         */
        @Step("Paste from clipboard")
        public Actions paste(@NonNull By locator) {
            performAction(ActionType.CLIPBOARD_PASTE, locator, null);
            return this.actions;
        }
    }
}
