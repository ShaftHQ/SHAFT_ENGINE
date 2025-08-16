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
import io.qameta.allure.Allure;
import io.qameta.allure.Step;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StatusDetails;
import lombok.NonNull;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.*;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.interactions.Locatable;
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

public class Actions extends ElementActions {
    public Actions() {
        super();
    }

    public Actions(WebDriver driver) {
        super(driver);
    }

    public Actions(WebDriver driver, boolean isSilent) {
        super(driver, isSilent);
    }

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

    public GetElementInformation get() {
        return new GetElementInformation();
    }

    @Step("Wait until")
    public Actions waitUntil(@NonNull Function<? super WebDriver, ?> isTrue) {
        new SynchronizationManager(driverFactoryHelper.getDriver()).fluentWait().until(isTrue);
        return this;
    }

    @Step("Wait until")
    public Actions waitUntil(@NonNull Function<? super WebDriver, ?> isTrue, @NonNull Duration timeout) {
        new SynchronizationManager(driverFactoryHelper.getDriver()).fluentWait().withTimeout(timeout).until(isTrue);
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

                // scroll to element (avoid relocating the element if already found)
                switch (SHAFT.Properties.flags.scrollingMode().toLowerCase()){
                    case "w3c" -> {
                        try {
                            // w3c compliant scroll
                            new org.openqa.selenium.interactions.Actions(d).scrollToElement(foundElements.get().getFirst()).perform();
                        } catch (Throwable throwable1) {
                            // old school selenium scroll
                            ((Locatable) d).getCoordinates().inViewPort();
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
                                ((Locatable) d).getCoordinates().inViewPort();
                            }
                        }
                    }
                    case "legacy" -> ((Locatable) d).getCoordinates().inViewPort();
                }

                // perform action
                switch (action) {
                    case HOVER ->
                            (new org.openqa.selenium.interactions.Actions(d)).pause(Duration.ofMillis(400)).moveToElement(foundElements.get().getFirst()).perform();
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
                        new org.openqa.selenium.interactions.Actions(d).pause(Duration.ofMillis(400)).clickAndHold(foundElements.get().getFirst()).perform();
                    }
                    case DOUBLE_CLICK -> {
                        screenshot.set(0, takeActionScreenshot(foundElements.get().getFirst()));
                        new org.openqa.selenium.interactions.Actions(d).pause(Duration.ofMillis(400)).doubleClick(foundElements.get().getFirst()).perform();
                    }
                    case TYPE, TYPE_SECURELY -> {
                        PropertiesHelper.setClearBeforeTypingMode();
                        executeClearBasedOnClearMode(foundElements);
                        foundElements.get().getFirst().sendKeys((CharSequence[]) data);
                    }
                    case TYPE_APPEND -> foundElements.get().getFirst().sendKeys((CharSequence[]) data);
                    case JAVASCRIPT_SET_VALUE ->
                            ((JavascriptExecutor) d).executeScript("arguments[0].value = arguments[1];", foundElements.get().getFirst(), data);
                    case CLEAR -> executeClearBasedOnClearMode(foundElements);
                    case DRAG_AND_DROP ->
                            new org.openqa.selenium.interactions.Actions(d).pause(Duration.ofMillis(400))
                                    .dragAndDrop(foundElements.get().getFirst(),
                                            d.findElement((By) data)).perform();
                    case DRAG_AND_DROP_BY_OFFSET ->
                            new org.openqa.selenium.interactions.Actions(d).pause(Duration.ofMillis(400))
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
                    if (!exception2.getCause().equals(exception)){
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

    private void executeClearBasedOnClearMode(AtomicReference<List<WebElement>> foundElements) {
        String clearMode = SHAFT.Properties.flags.clearBeforeTypingMode();
        switch (clearMode) {
            case "native":
                foundElements.get().getFirst().clear();
                break;
            case "backspace":
                String text = parseElementText(foundElements.get().getFirst());
                if (!text.isEmpty())
                    foundElements.get().getFirst().sendKeys(String.valueOf(Keys.BACK_SPACE).repeat(text.length()));
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
            foundElements = driverFactoryHelper.getDriver().findElements(locator);
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

        try {
            JavaScriptWaitManager.waitForLazyLoading(driver);
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
        }
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
            stepName.append(" ").append(JavaHelper.convertToSentenceCase(status.name()));

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

    protected enum ActionType {HOVER, CLICK, JAVASCRIPT_CLICK, TYPE, TYPE_SECURELY, TYPE_APPEND, JAVASCRIPT_SET_VALUE, CLEAR, DRAG_AND_DROP, GET_DOM_ATTRIBUTE, GET_DOM_PROPERTY, GET_NAME, GET_TEXT, GET_CSS_VALUE, GET_IS_DISPLAYED, GET_IS_ENABLED, DRAG_AND_DROP_BY_OFFSET, GET_SELECTED_TEXT, CLICK_AND_HOLD, DOUBLE_CLICK, GET_IS_SELECTED, DROP_FILE_TO_UPLOAD}

    public class GetElementInformation {
        protected GetElementInformation() {
        }

        @Step("Get DOM attribute")
        public String domAttribute(@NonNull By locator, @NonNull String attributeName) {
            return performAction(ActionType.GET_DOM_ATTRIBUTE, locator, attributeName);
        }

        @Step("Get DOM property")
        public String domProperty(@NonNull By locator, @NonNull String propertyName) {
            return performAction(ActionType.GET_DOM_PROPERTY, locator, propertyName);
        }

        @Step("Get name")
        public String name(@NonNull By locator) {
            return performAction(ActionType.GET_NAME, locator, null);
        }

        @Step("Get text")
        public String text(@NonNull By locator) {
            return performAction(ActionType.GET_TEXT, locator, null);
        }

        @Step("Get selected text")
        public String selectedText(@NonNull By locator) {
            return performAction(ActionType.GET_SELECTED_TEXT, locator, null);
        }

        @Step("Get CSS value")
        public String cssValue(@NonNull By locator, @NonNull String propertyName) {
            return performAction(ActionType.GET_CSS_VALUE, locator, propertyName);
        }

        @Step("Get is displayed")
        public boolean isDisplayed(@NonNull By locator) {
            return Boolean.parseBoolean(performAction(ActionType.GET_IS_DISPLAYED, locator, null));
        }

        @Step("Get is enabled")
        public boolean isEnabled(@NonNull By locator) {
            return Boolean.parseBoolean(performAction(ActionType.GET_IS_ENABLED, locator, null));
        }

        @Step("Get is selected")
        public boolean isSelected(@NonNull By locator) {
            return Boolean.parseBoolean(performAction(ActionType.GET_IS_SELECTED, locator, null));
        }
    }
}
