package com.shaft.gui.element.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.internal.exceptions.MultipleElementsFoundException;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import io.qameta.allure.Allure;
import io.qameta.allure.Step;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StatusDetails;
import lombok.NonNull;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.Locatable;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

public class Actions extends ElementActions {
    public Actions() {
        initialize();
    }

    public Actions(WebDriver driver) {
        initialize(driver);
    }

    public Actions(WebDriver driver, boolean isSilent) {
        initialize(driver, isSilent);
    }

    public Actions(DriverFactoryHelper helper) {
        initialize(helper);
    }

    @Step("Click")
    @Override public Actions click(@NonNull By locator) {
        //take screenshot (to be added to gif and/or used while reporting failure)
        byte[] screenshotBeforeAction = takeScreenshotBeforeAction(locator);
        //performClick
        performAction(ActionType.CLICK, locator, null, screenshotBeforeAction);
        return this;
    }

    @Step("Type")
    @Override
    public Actions type(@NonNull By locator, @NonNull CharSequence text) {
        performAction(ActionType.TYPE, locator, text, null);
        return this;
    }

    private enum ActionType{GET_NAME,CLICK,TYPE}

    private String performAction(ActionType action, By locator, Object data, byte[] screenshotBeforeAction) {
        AtomicReference<String> output = new AtomicReference<>("");
        AtomicReference<String> accessibleName = new AtomicReference<>(JavaHelper.formatLocatorToString(locator));

        try{
            new SynchronizationManager(driver).fluentWait(true).until(d->{
                // find all elements matching the target locator
                List<WebElement> foundElements = findAllElements(locator);

                // fail fast if no elements were found
                if (foundElements.isEmpty())
                    return false;

                // ensure element locator is unique if applicable
                if (foundElements.size() > 1 && SHAFT.Properties.flags.forceCheckElementLocatorIsUnique())
                    throw new MultipleElementsFoundException();

                // scroll to element (avoid relocating the element if already found)
                try {
                    // native Javascript scroll to center (smooth / auto)
                    ((JavascriptExecutor) driver).executeScript("""
                                                        arguments[0].scrollIntoView({behavior: "auto", block: "center", inline: "center"});""", foundElements.getFirst());
                } catch (Throwable throwable) {
                    try {
                        // w3c compliant scroll
                        new org.openqa.selenium.interactions.Actions(driver).scrollToElement(foundElements.getFirst()).perform();
                    } catch (Throwable throwable1) {
                        // old school selenium scroll
                        ((Locatable) driver).getCoordinates().inViewPort();
                    }
                }

                // get accessible name if needed
                if (SHAFT.Properties.reporting.captureElementName()) {
                    String fetchedName = foundElements.getFirst().getAccessibleName();
                    if (fetchedName != null && !fetchedName.isEmpty())
                        accessibleName.set(fetchedName.trim());
                }

                // perform action
                switch (action){
                    case CLICK -> {
                        try {
                            foundElements.getFirst().click();
                        } catch (ElementClickInterceptedException exception){
                            if (SHAFT.Properties.flags.clickUsingJavascriptWhenWebDriverClickFails()) {
                                ((JavascriptExecutor) driver).executeScript("arguments[0].click();", foundElements.getFirst());
                                ReportManager.logDiscrete("Performed Click using JavaScript; If the report is showing that the click passed but you observe that no action was taken, we recommend trying a different element locator.");
                            } else {
                                throw exception;
                            }
                        }
                    }
                    case TYPE -> foundElements.getFirst().sendKeys((CharSequence) data);
                    case GET_NAME -> output.set(foundElements.getFirst().getAccessibleName());
                }
                return true;
            });
            } catch (WebDriverException | MultipleElementsFoundException exception){
                // report broken
                reportBroken(action.name(),locator, accessibleName.get(),screenshotBeforeAction, exception);
            }
        //report pass
        reportPass(action.name(),locator,accessibleName.get(),screenshotBeforeAction);
        return output.get();
    }

    private List<WebElement> findAllElements(By locator) {
        List<WebElement> foundElements;

        By shadowDomLocator = ShadowLocatorBuilder.shadowDomLocator.get();
        By cssSelector = ShadowLocatorBuilder.cssSelector.get();

        try {
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
        } catch (InvalidSelectorException invalidSelectorException) {
            //break and fail immediately if there's an invalid selector
            throw new RuntimeException(invalidSelectorException);
        }
        return foundElements;
    }

    private byte[] takeScreenshotBeforeAction(By locator){
        if (SHAFT.Properties.visuals.createAnimatedGif())
            return new ScreenshotManager().internalCaptureScreenshot(driver, locator, true);
        return null;
    }

    private byte[] takeScreenshotUponFailure(By locator){
        return new ScreenshotManager().internalCaptureScreenshot(driver, locator, false);
    }

    private byte[] takeScreenshotAfterAction(By locator){
        if (SHAFT.Properties.visuals.createAnimatedGif())
            return new ScreenshotManager().internalCaptureScreenshot(driver, locator, true);
        return null;
    }

    private void reportPass(String action, By locator, String elementName, byte[] screenshot){
        report(action, locator, elementName, Status.PASSED, screenshot, null);
    }

    private void reportBroken(String action, By locator, String elementName, byte[] screenshot, Exception exception){
        report(action, locator, elementName, Status.BROKEN, screenshot, exception);
    }

    private void report(String action, By locator, String elementName, Status status, byte[] screenshot, Exception exception){
        // update allure step name
        StringBuilder stepName = new StringBuilder();
        stepName.append(JavaHelper.convertToSentenceCase(action)).append(" \"").append(elementName).append("\"");

        if (!status.equals(Status.PASSED))
            stepName.append(" ").append(JavaHelper.convertToSentenceCase(status.name()));

        Allure.getLifecycle().updateStep(update -> update.setName(stepName.toString()));

        // add screenshot that was taken before execution if applicable
        if (screenshot!=null)
            Allure.addAttachment(action, "image/png", new ByteArrayInputStream(screenshot), ".png");

        // handle reporting based on status
        if (Status.PASSED.equals(status)){
            // if the step passed
            ReportManager.logDiscrete(stepName.toString());
            //take screenshot highlighting element
            byte[] successScreenshot = takeScreenshotAfterAction(locator);
            if (successScreenshot != null)
                Allure.addAttachment(action, "image/png", new ByteArrayInputStream(successScreenshot), ".png");
        }else{
            // if the step failed
            ReportManager.log(stepName.toString(), Level.ERROR);

            // update allure step status
            Allure.getLifecycle().updateStep(update -> update.setStatus(status));

            if (exception!=null){
                // update exception stacktrace
                ArrayList<Class<? extends Throwable>> elementNotVisibleExceptions = new ArrayList<>();
                elementNotVisibleExceptions.add(NoSuchElementException.class);
                elementNotVisibleExceptions.add(InvalidSelectorException.class);

                //take screenshot highlighting element if possible
                byte[] failureScreenshot;
                if (exception.getCause() !=null && elementNotVisibleExceptions.contains(exception.getCause().getClass())){
                    failureScreenshot = takeScreenshotUponFailure(locator);
                } else {
                    failureScreenshot = takeScreenshotUponFailure(null);
                }
                Allure.addAttachment(action, "image/png", new ByteArrayInputStream(failureScreenshot), ".png");

                // update allure stacktrace
                Allure.getLifecycle().updateStep(update -> {
                    var trace = update.getStatusDetails() == null ? exception : update.getStatusDetails().getTrace() + System.lineSeparator() + exception;
                    StatusDetails details = update.getStatusDetails() == null ? new StatusDetails() : update.getStatusDetails();
                    details.setTrace(trace.toString().trim());
                    update.setStatusDetails(details);
                });
            } else {
                //take screenshot highlighting element
                byte[] failureScreenshot = takeScreenshotUponFailure(locator);
                Allure.addAttachment(action, "image/png", new ByteArrayInputStream(failureScreenshot), ".png");
            }
        }
    }
}
