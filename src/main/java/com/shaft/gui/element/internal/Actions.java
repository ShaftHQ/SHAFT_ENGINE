package com.shaft.gui.element.internal;

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
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.qameta.allure.Allure;
import io.qameta.allure.Step;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StatusDetails;
import lombok.NonNull;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.Locatable;
import org.openqa.selenium.support.locators.RelativeLocator;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

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

    @Override public Actions and() {
        return this;
    }

    @Step("Click")
    @Override public Actions click(@NonNull By locator) {
        //performClick
        performAction(ActionType.CLICK, locator, null);
        return this;
    }

    @Step("Type")
    @Override
    public Actions type(@NonNull By locator, @NonNull CharSequence text) {
        performAction(ActionType.TYPE, locator, text);
        return this;
    }

    @Step("Drag and drop")
    @Override
    public Actions dragAndDrop(@NonNull By sourceElementLocator, @NonNull By destinationElementLocator) {
        performAction(ActionType.DRAG_AND_DROP, sourceElementLocator, destinationElementLocator);
        return this;
    }

    private enum ActionType {GET_NAME, CLICK, TYPE, DRAG_AND_DROP}

    private String performAction(ActionType action, By locator, Object data) {
        AtomicReference<String> output = new AtomicReference<>("");
        AtomicReference<String> accessibleName = new AtomicReference<>(JavaHelper.formatLocatorToString(locator));
        final byte[][] screenshot = {null};
        AtomicReference<List<WebElement>> foundElements = new AtomicReference<>();

        try{
            new SynchronizationManager(driver).fluentWait(true).until(d->{
                // find all elements matching the target locator
                foundElements.set(findAllElements(locator));

                // fail fast if no elements were found
                if (foundElements.get().isEmpty())
                    throw new NoSuchElementException("Cannot locate an element using "+JavaHelper.formatLocatorToString(locator));

                // ensure element locator is unique if applicable
                if (foundElements.get().size() > 1 && SHAFT.Properties.flags.forceCheckElementLocatorIsUnique() && !(locator instanceof RelativeLocator.RelativeBy))
                    throw new MultipleElementsFoundException();

                // identify run type
                boolean isNotMobileExecution = DriverFactoryHelper.isNotMobileExecution();

                // get accessible name if needed
                if (SHAFT.Properties.reporting.captureElementName()) {
                    String fetchedName = "";
                    if (isNotMobileExecution) {
                        try {
                            fetchedName = foundElements.get().getFirst().getAccessibleName();
                        } catch (UnsupportedCommandException | StaleElementReferenceException throwable) {
                            //happens on some elements that show unhandled inspector error
                            //this exception is thrown on some older selenium grid instances, I saw it with firefox running over selenoid
                            //ignore
                            //saw it again with mobile web tests
                            // the stale was thrown in an iframe
                        }
                    } else {
                        fetchedName = foundElements.get().getFirst().getAttribute("text");
                    }
                    if (fetchedName != null && !fetchedName.isEmpty())
                        accessibleName.set(fetchedName.trim());
                }

                // scroll to element (avoid relocating the element if already found)
                // if not mobile else just do the w3c compliant scroll
                if (isNotMobileExecution) {
                    try {
                        // native Javascript scroll to center (smooth / auto)
                        ((JavascriptExecutor) driver).executeScript("""
                                arguments[0].scrollIntoView({behavior: "auto", block: "center", inline: "center"});""", foundElements.get().getFirst());
                    } catch (Throwable throwable) {
                        try {
                            // w3c compliant scroll
                            new org.openqa.selenium.interactions.Actions(driver).scrollToElement(foundElements.get().getFirst()).perform();
                        } catch (Throwable throwable1) {
                            // old school selenium scroll
                            ((Locatable) driver).getCoordinates().inViewPort();
                        }
                    }
                }

                // perform action
                switch (action){
                    case CLICK -> {
                        try {
                            screenshot[0] = takeActionScreenshot(foundElements.get().getFirst());
                            foundElements.get().getFirst().click();
                        } catch (ElementClickInterceptedException exception){
                            if (SHAFT.Properties.flags.clickUsingJavascriptWhenWebDriverClickFails()) {
                                ((JavascriptExecutor) driver).executeScript("arguments[0].click();", foundElements.get().getFirst());
                                ReportManager.logDiscrete("Performed Click using JavaScript; If the report is showing that the click passed but you observe that no action was taken, we recommend trying a different element locator.");
                            } else {
                                throw exception;
                            }
                        }
                    }
                    case TYPE -> {
                        if (SHAFT.Properties.flags.attemptClearBeforeTyping())
                            foundElements.get().getFirst().clear();

                        if (SHAFT.Properties.flags.attemptClearBeforeTypingUsingBackspace()){
                            String text = parseElementText(foundElements.get().getFirst());
                            if (!text.isEmpty())
                                foundElements.get().getFirst().sendKeys(String.valueOf(Keys.BACK_SPACE).repeat(text.length()));
                        }

                        foundElements.get().getFirst().sendKeys((CharSequence) data);
                    }
                    case GET_NAME -> output.set(foundElements.get().getFirst().getAccessibleName());
                    case DRAG_AND_DROP -> new org.openqa.selenium.interactions.Actions(driver)
                            .dragAndDrop(foundElements.get().getFirst(),
                                    driver.findElement((By) data))
                            .pause(Duration.ofMillis(300)).build().perform();
                }

                // take screenshot if not already taken before action
                if (screenshot[0] != null)
                    screenshot[0] = takeActionScreenshot(foundElements.get().getFirst());
                return true;
            });
        } catch (WebDriverException exception) {
            // take failure screenshot if needed
            if (screenshot[0] == null) {
                if (foundElements.get() == null || foundElements.get().size() !=1 ) {
                    screenshot[0] = takeFailureScreenshot(null);
                } else {
                    screenshot[0] = takeFailureScreenshot(foundElements.get().getFirst());
                }
            }

            // report broken
            reportBroken(action.name(), accessibleName.get(), screenshot[0], exception);
        }

        //report pass
        reportPass(action.name(),accessibleName.get(), screenshot[0]);
        return output.get();
    }

    private String parseElementText(WebElement element) {
        String text = element.getText();
        if (!text.isEmpty())
            return text;
        text = element.getAttribute("value");
        if (text != null && !text.isEmpty())
            return text;
        text = element.getAttribute("textContent");
        if (text != null && !text.isEmpty())
            return text;
        text = element.getAttribute("innerHTML");
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
            driver.switchTo().defaultContent();
            //switch to shadow root and find elements
            foundElements = driver.findElement(shadowDomLocator)
                    .getShadowRoot()
                    .findElements(cssSelector);
        } else if (LocatorBuilder.getIFrameLocator().get() != null) {
            //reset to default content
            driver.switchTo().defaultContent();
            //switch to frame and find elements
            foundElements = driver.switchTo()
                    .frame(driver.findElement(LocatorBuilder.getIFrameLocator().get()))
                    .findElements(locator);
        } else {
            //normal case, just find the elements
            foundElements = driver.findElements(locator);
        }
        return foundElements;
    }

    private byte[] takeActionScreenshot(WebElement element){
        if (SHAFT.Properties.visuals.createAnimatedGif())
            return captureScreenshot(element, true);
        return null;
    }

    private byte[] takeFailureScreenshot(WebElement element){
        return captureScreenshot(element, false);
    }

    private byte[] captureScreenshot(WebElement element, boolean isPass){
        // capture screenshot
        byte[] screenshot;
        if (element != null && Boolean.TRUE.equals(SHAFT.Properties.visuals.screenshotParamsHighlightElements())) {
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

    private byte[] appendShaftWatermark(byte[] screenshot){
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
                            if (elementActionsHelper.getElementsCount(driver, By.xpath(locator)) == 1) {
                                skippedElementsList.add(((WebElement) elementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, By.xpath(locator)).get(1)));
                            }
                        }
                        WebElement[] skippedElementsArray = new WebElement[skippedElementsList.size()];
                        skippedElementsArray = skippedElementsList.toArray(skippedElementsArray);
                        screenshot= ScreenshotHelper.makeFullScreenshot(driver, skippedElementsArray);
                    } else {
                        // make full page screenshot using BiDi, CDP, or manually based on the target browser
                        screenshot= ScreenshotHelper.makeFullScreenshot(driver);
                    }
                } catch (Throwable throwable) {
                    // return regular screenshot in case of failure
                    ReportManagerHelper.logDiscrete(throwable);
                    screenshot= ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
                }
            }
            case ELEMENT -> {
                try{
                    //get element screenshot
                    screenshot= element.getScreenshotAs(OutputType.BYTES);
                } catch (Throwable throwable) {
                    // return regular screenshot in case of failure
                    ReportManagerHelper.logDiscrete(throwable);
                    screenshot= ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
                }
            }
            default -> screenshot= ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
        }
        //append shaft watermark
        screenshot = appendShaftWatermark(screenshot);
        return screenshot;
    }

    private byte[] takeAIHighlightedScreenshot(WebElement element, boolean isPass) {
        // getElementLocation
        Rectangle elementLocation = element.getRect();

        //take screenshot
        byte [] src = takeScreenshot(element);

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
        JavascriptExecutor js = (JavascriptExecutor) driver;
        String regularElementStyle = highlightElementAndReturnDefaultStyle(driver, element, js,
                setHighlightedElementStyle(isPass));

        //take screenshot
        byte [] src = takeScreenshot(element);

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
        String regularElementStyle = element.getAttribute("style");
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

    private void reportPass(String action, String elementName, byte[] screenshot){
        report(action, elementName, Status.PASSED, screenshot, null);
    }

    private void reportBroken(String action, String elementName, byte[] screenshot, RuntimeException exception){
        report(action, elementName, Status.BROKEN, screenshot, exception);
    }

    private void report(String action, String elementName, Status status, byte[] screenshot, RuntimeException exception){
        // update allure step name
        StringBuilder stepName = new StringBuilder();
        stepName.append(JavaHelper.convertToSentenceCase(action)).append(" \"").append(elementName).append("\"");

        if (!status.equals(Status.PASSED))
            stepName.append(" ").append(JavaHelper.convertToSentenceCase(status.name()));

        Allure.getLifecycle().updateStep(update -> update.setName(stepName.toString()));

        // attach screenshot
        if (screenshot!=null)
            Allure.addAttachment(new SimpleDateFormat("yyyyMMdd_HHmmss").format(Calendar.getInstance().getTime())+"_"+JavaHelper.convertToSentenceCase(action) + "_"+JavaHelper.removeSpecialCharacters(elementName), "image/png", new ByteArrayInputStream(screenshot), ".png");

        // handle reporting based on status
        if (Status.PASSED.equals(status)){
            // if the step passed
            ReportManager.logDiscrete(stepName.toString());
        }else{
            // if the step failed
            ReportManager.logDiscrete(stepName.toString(), Level.ERROR);

            // update allure step status to broken
            Allure.getLifecycle().updateStep(update -> update.setStatus(status));

            // update test status to failed
            Allure.getLifecycle().updateTestCase(update -> update.setStatus(Status.FAILED));

            if (exception!=null){
                // update allure stacktrace
                Allure.getLifecycle().updateStep(update -> {
                    var trace = update.getStatusDetails() == null ? exception : update.getStatusDetails().getTrace() + System.lineSeparator() + exception;
                    StatusDetails details = update.getStatusDetails() == null ? new StatusDetails() : update.getStatusDetails();
                    details.setTrace(trace.toString().trim());
                    update.setStatusDetails(details);
                });
                throw new RuntimeException(FailureReporter.getRootCause(exception).trim(),exception);
            }
        }
    }
}
