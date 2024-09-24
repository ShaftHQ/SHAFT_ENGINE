package com.shaft.gui.element.internal;

import com.google.common.base.Throwables;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.enums.internal.ClipboardAction;
import com.shaft.enums.internal.ElementAction;
import com.shaft.gui.browser.internal.BrowserActionsHelper;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.internal.exceptions.MultipleElementsFoundException;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.internal.support.JavaScriptHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportHelper;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.ValidationsHelper;
import io.appium.java_client.AppiumDriver;
import lombok.Getter;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.interactions.Locatable;
import org.openqa.selenium.support.locators.RelativeLocator;
import org.testng.Assert;

import java.awt.*;
import java.time.Duration;
import java.util.List;
import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

@SuppressWarnings({"UnusedReturnValue"})
public class ElementActionsHelper {
    public static final String OBFUSCATED_STRING = "•";
    private static final boolean GET_ELEMENT_HTML = true; //TODO: expose parameter
    private static final int ELEMENT_IDENTIFICATION_POLLING_DELAY = 100; // milliseconds
    private final boolean isSilent;

    public ElementActionsHelper(boolean isSilent) {
        this.isSilent = isSilent;
    }

    public int waitForElementPresenceWithReducedTimeout(WebDriver driver, By elementLocator) {
        AtomicInteger numberOfFoundElements = new AtomicInteger();
        try {
            new SynchronizationManager(driver).fluentWait(true)
                    .withTimeout(Duration.ofMillis(300)) //this is used for faster mobile native scrolling. default for ios is 200 and for android is 250, this covers both
                    .until(f -> {
                        numberOfFoundElements.set(driver.findElements(elementLocator).size());
                        return numberOfFoundElements.get() > 0;
                    });
        } catch (TimeoutException timeoutException) {
            return 0;
        }
        return numberOfFoundElements.get();
    }

    public List<Object> waitForElementPresence(WebDriver driver, By elementLocator) {
        return waitForElementPresence(driver, elementLocator, SHAFT.Properties.flags.forceCheckForElementVisibility());
    }

    public List<Object> waitForElementPresence(WebDriver driver, String elementReferenceScreenshot) {
        long startTime = System.currentTimeMillis();
        long elapsedTime;
        List<Integer> coordinates;
        boolean isFound = false;
        byte[] currentScreenImage;

        List<Object> returnedValue = new LinkedList<>();
        if (FileActions.getInstance(true).doesFileExist(elementReferenceScreenshot)) {
            do {
                try {
                    //noinspection BusyWait
                    Thread.sleep(ELEMENT_IDENTIFICATION_POLLING_DELAY);
                } catch (InterruptedException e) {
                    ReportManagerHelper.logDiscrete(e);
                }
                currentScreenImage = new ScreenshotManager().takeScreenshot(driver, null);
                coordinates = ImageProcessingActions.findImageWithinCurrentPage(elementReferenceScreenshot, currentScreenImage);
                if (!Collections.emptyList().equals(coordinates)) {
                    isFound = true;
                }
                elapsedTime = System.currentTimeMillis() - startTime;
            } while (!isFound && elapsedTime < SHAFT.Properties.timeouts.defaultElementIdentificationTimeout() * 1000L);
            returnedValue.add(currentScreenImage);
            returnedValue.add(FileActions.getInstance(true).readFileAsByteArray(elementReferenceScreenshot));
            returnedValue.add(coordinates);
        } else {
            // reference screenshot doesn't exist
            ReportManager.log("Reference screenshot not found. Kindly confirm the image exists under this path: \"" + elementReferenceScreenshot + "\"");
            currentScreenImage = new ScreenshotManager().takeScreenshot(driver, null);
            returnedValue.add(currentScreenImage);
            returnedValue.add(new byte[0]);
            returnedValue.add(Collections.emptyList());
        }
        return returnedValue;
    }

    public boolean waitForElementInvisibility(WebDriver driver, By elementLocator) {
        new SynchronizationManager(driver).fluentWait(false)
                .until(f -> !driver.findElement(elementLocator).isDisplayed());
        return true;
    }

    private boolean isValidToCheckForVisibility(By elementLocator, boolean checkForVisibility) {
        var locatorString = JavaHelper.formatLocatorToString(elementLocator).toLowerCase();
        return checkForVisibility && !locatorString.contains("type='file'") && !locatorString.contains("type=\"file\"") && !locatorString.contains("frame") && !elementLocator.equals(By.tagName("html"));
    }

    //TODO: keep enhancing this method until we only need to make ONE WebDriver call per element in case of Type and Click (including element name)
    public List<Object> waitForElementPresence(WebDriver driver, By elementLocator, boolean checkForVisibility, Object... action) {
        boolean isValidToCheckForVisibility = isValidToCheckForVisibility(elementLocator, checkForVisibility);
        var isMobileExecution = DriverFactoryHelper.isMobileNativeExecution() || DriverFactoryHelper.isMobileWebExecution();

        try {
            return new SynchronizationManager(driver).fluentWait(isValidToCheckForVisibility)
                    .until(f -> {
                        try (ExecutorService myExecutor = Executors.newVirtualThreadPerTaskExecutor()) {
                            final WebElement[] targetElement = new WebElement[1];
                            ElementInformation elementInformation = new ElementInformation();
                            // BLOCK #1 :: GETTING THE ELEMENT
                            By shadowDomLocator = ShadowLocatorBuilder.shadowDomLocator.get();
                            By cssSelector = ShadowLocatorBuilder.cssSelector.get();
                            if (shadowDomLocator != null && cssSelector == elementLocator) {
                                targetElement[0] = driver.findElement(shadowDomLocator)
                                        .getShadowRoot()
                                        .findElement(cssSelector);
                            } else if (LocatorBuilder.getIFrameLocator().get() != null) {
                                try {
                                    targetElement[0] = driver.switchTo().frame(driver.findElement(LocatorBuilder.getIFrameLocator().get())).findElement(elementLocator);
                                } catch (NoSuchElementException exception) {
                                    targetElement[0] = driver.findElement(elementLocator);
                                }
                            } else {
                                try {
                                    targetElement[0] = driver.findElement(elementLocator);
                                } catch (InvalidSelectorException invalidSelectorException) {
                                    //break and fail immediately if invalid selector
                                    reportActionResult(driver, null, null, null, null, null, false);
                                    FailureReporter.fail(ElementActionsHelper.class, "Failed to identify unique element", invalidSelectorException);
                                }
                            }
                            var threadRect = myExecutor.submit(() -> {
                                // BLOCK #2 :: GETTING THE ELEMENT LOCATION (RECT)
                                try {
                                    elementInformation.setElementRect(targetElement[0].getRect());
                                } catch (ElementNotInteractableException elementNotInteractableException) {
                                    // this exception happens sometimes with certain browsers and causes a timeout
                                    // this empty block should handle that issue
                                }
                            });
                            var threadLocate = myExecutor.submit(() -> {
                                // BLOCK #3 :: SCROLLING TO ELEMENT | CONFIRMING IT IS DISPLAYED
                                if (isValidToCheckForVisibility) {
                                    if (!isMobileExecution) {
                                        try {
                                            // native Javascript scroll to center (smooth / auto)
                                            var scriptOutput = ((JavascriptExecutor) driver).executeScript("""
                                                    arguments[0].scrollIntoView({behavior: "smooth", block: "center", inline: "center"});""", targetElement[0]);
                                        } catch (Throwable throwable) {
                                            try {
                                                // w3c compliant scroll
                                                new Actions(driver).scrollToElement(targetElement[0]).perform();
                                            } catch (Throwable throwable1) {
                                                // old school selenium scroll
                                                ((Locatable) driver).getCoordinates().inViewPort();
                                            }
                                        }
                                    } else {
                                        targetElement[0].isDisplayed();
                                    }
                                }
                            });
                            var threadCount = myExecutor.submit(() -> {
                                // BLOCK #4 :: GETTING THE NUMBER OF FOUND ELEMENTS
                                if (shadowDomLocator != null && cssSelector == elementLocator) {
                                    elementInformation.setNumberOfFoundElements(driver.findElement(shadowDomLocator)
                                            .getShadowRoot()
                                            .findElements(cssSelector)
                                            .size());
                                } else {
                                    elementInformation.setNumberOfFoundElements(driver.findElements(elementLocator).size());
                                }
                            });
                            var threadHTML = myExecutor.submit(() -> {
                                // BLOCK #5 :: GETTING THE INNER AND OUTER HTML
                                if (!isMobileExecution && GET_ELEMENT_HTML) {
                                    elementInformation.setOuterHTML(targetElement[0].getAttribute("outerHTML"));
                                    elementInformation.setInnerHTML(targetElement[0].getAttribute("innerHTML"));
                                }
                            });
                            var threadName = myExecutor.submit(() -> {
                                // BLOCK #6 :: GETTING ELEMENT NAME
                                if (SHAFT.Properties.reporting.captureElementName()) {
                                    var elementName = JavaHelper.formatLocatorToString(elementLocator);
                                    try {
                                        var accessibleName = targetElement[0].getAccessibleName();
                                        if (accessibleName != null && !accessibleName.isBlank()) {
                                            elementName = accessibleName;
                                        }
                                    } catch (Throwable throwable) {
                                        //happens on some elements that show unhandled inspector error
                                        //this exception is thrown on some older selenium grid instances, I saw it with firefox running over selenoid
                                        //ignore
                                    }
                                    elementInformation.setElementName(elementName);
                                }
                            });

                            // SYNCHRONIZATION POINT
                            threadRect.get();
                            threadLocate.get();
                            threadCount.get();
                            threadHTML.get();
                            threadName.get();

                            elementInformation.setFirstElement(targetElement[0]);
                            elementInformation.setLocator(elementLocator);

                            if (action != null && action.length > 0) {
                                // fail if multiple elements are found and flag is enabled
                                if (elementInformation.getNumberOfFoundElements() > 1
                                        && SHAFT.Properties.flags.forceCheckElementLocatorIsUnique() &&
                                        !(elementLocator instanceof RelativeLocator.RelativeBy)) {
                                    reportActionResult(driver, null, null, null, null, null, false);
                                    FailureReporter.fail(ElementActionsHelper.class, "Failed to identify unique element", new MultipleElementsFoundException("Multiple elements found matching this locator \"" + JavaHelper.formatLocatorToString(elementLocator) + "\""));
                                }
                                // BLOCK #6 :: PERFORMING ACTION  (WITH OPTIONAL ARGS)
                                // attempt to perform action inside the loop to guarantee higher odds of success and reduced WebDriver calls
                                switch (action.length) {
                                    case 1 ->
                                            elementInformation.setActionResult(performAction(driver, elementInformation, (ElementAction) action[0], ""));
                                    case 2 ->
                                            elementInformation.setActionResult(performAction(driver, elementInformation, (ElementAction) action[0], action[1]));
                                }
                            }
                            return elementInformation.toList();
                            // int numberOfFoundElements
                            // WebElement firstElement
                            // By locator
                            // String outerHTML (or empty string)
                            // String innerHTML (or empty string)
                            // String elementName (or empty string)
                        } catch (ExecutionException | InterruptedException e) {
                            throw new RuntimeException(e);
                        }
                    });
        } catch (org.openqa.selenium.TimeoutException timeoutException) {
            // In case the element was not found / not visible and the timeout expired
            var causeMessage = timeoutException.getCause().getMessage();
            causeMessage = !causeMessage.isBlank() && causeMessage.contains("\n") ? timeoutException.getMessage() + " || " + causeMessage.substring(0, causeMessage.indexOf("\n")) : timeoutException.getMessage();
            ReportManager.logDiscrete(causeMessage);
            var elementInformation = new ArrayList<>();
            elementInformation.add(0);
            elementInformation.add(null);
            elementInformation.add(timeoutException);
            return elementInformation;
        } catch (org.openqa.selenium.InvalidSelectorException invalidSelectorException) {
            // In case the selector is not valid
            ReportManager.logDiscrete(invalidSelectorException.getMessage());
            var elementInformation = new ArrayList<>();
            elementInformation.add(0);
            elementInformation.add(null);
            elementInformation.add(invalidSelectorException);
            return elementInformation;
        }
    }

    private String performAction(WebDriver driver, ElementInformation elementInformation, ElementAction action, Object parameter) {
        switch (action) {
            case CLICK -> {
                //move to element
                try {
                    (new Actions(driver)).moveToElement(elementInformation.getFirstElement()).perform();
                    ReportManager.logDiscrete("Moved the mouse to the middle of the element.");
                } catch (Throwable throwable) {
                    //ignored
                }
                //perform click
                try {
                    elementInformation.getFirstElement().click();
                } catch (Throwable throwable) {
                    if (DriverFactoryHelper.isNotMobileExecution()) {
                        if (SHAFT.Properties.flags.clickUsingJavascriptWhenWebDriverClickFails()) {
                            var scriptResult = ((JavascriptExecutor) driver).executeScript("arguments[0].click();", elementInformation.getFirstElement());
                            ReportManager.logDiscrete("Performed Click using JavaScript.");
                            ReportManager.logDiscrete("If the report is showing that the click passed but you observe that no action was taken, we recommend trying a different element locator.");
                        } else {
                            throw throwable;
                        }
                    }
                }
            }
            case CLEAR -> elementInformation.getFirstElement().clear();
            case BACKSPACE -> elementInformation.getFirstElement().sendKeys(Keys.BACK_SPACE);
            case GET_TEXT -> {
                return elementInformation.getFirstElement().getText();
            }
            case GET_VALUE -> {
                return elementInformation.getFirstElement().getAttribute(TextDetectionStrategy.VALUE.getValue());
            }
            case GET_CONTENT -> {
                return elementInformation.getFirstElement().getAttribute(TextDetectionStrategy.CONTENT.getValue());
            }
            case GET_ATTRIBUTE -> {
                return elementInformation.getFirstElement().getAttribute((String) parameter);
            }
            case SEND_KEYS -> elementInformation.getFirstElement().sendKeys((CharSequence) parameter);
            case IS_DISPLAYED -> {
                return String.valueOf(elementInformation.getFirstElement().isDisplayed());
            }
            case SET_VALUE_USING_JAVASCRIPT ->
                    ((JavascriptExecutor) driver).executeScript("arguments[0].value = arguments[1];", elementInformation.getFirstElement(), parameter);
            case HOVER ->
                    (new Actions(driver)).pause(Duration.ofMillis(400)).moveToElement(elementInformation.getFirstElement()).perform();
        }
        return "";
    }

    public List<Object> scrollToFindElement(WebDriver driver, By elementLocator) {
        try {
            return new SynchronizationManager(driver).fluentWait().until(f -> {
                WebElement targetElement;
                try {
                    targetElement = driver.findElement(elementLocator);
                } catch (NoSuchElementException noSuchElementException) {
                    new Actions(driver).scrollByAmount(0, driver.manage().window().getSize().getHeight()).perform();
                    targetElement = driver.findElement(elementLocator);
                }
                var elementInformation = new ArrayList<>();
                elementInformation.add(driver.findElements(elementLocator).size());
                elementInformation.add(targetElement);
                return elementInformation;
            });
        } catch (org.openqa.selenium.TimeoutException timeoutException) {
            // In case the element was not found / not visible and the timeout expired
            ReportManager.logDiscrete(timeoutException.getMessage() + " || " + timeoutException.getCause().getMessage().substring(0, timeoutException.getCause().getMessage().indexOf("\n")));
            var elementInformation = new ArrayList<>();
            elementInformation.add(0);
            elementInformation.add(null);
            elementInformation.add(timeoutException);
            return elementInformation;
        }
    }


    //TODO: delete this method after understanding what the heck it's supposed to be doing!
    public boolean waitForElementToBeClickable(WebDriver driver, By elementLocator, String actionToExecute) {
        var clickUsingJavascriptWhenWebDriverClickFails = SHAFT.Properties.flags.clickUsingJavascriptWhenWebDriverClickFails();

        if (!DriverFactoryHelper.isMobileNativeExecution()) {
            try {
                new SynchronizationManager(driver).fluentWait(false)
                        .until(f -> driver.findElement(elementLocator).isDisplayed() && driver.findElement(elementLocator).isEnabled());

                return new SynchronizationManager(driver).fluentWait(true)
                        .until(f -> {
                            if (!actionToExecute.isEmpty()) {
                                if (actionToExecute.equalsIgnoreCase("ClickAndHold")) {
                                    (new Actions(driver)).clickAndHold(((WebElement) this.identifyUniqueElement(driver, elementLocator).get(1))).build().perform();
                                }
                            }
                            return true;
                        });
            } catch (org.openqa.selenium.TimeoutException e) {
                ReportManagerHelper.logDiscrete(e);
                return false;
            }
        }
        return true;
    }

    public boolean waitForElementTextToBeNot(WebDriver driver, By elementLocator, String textShouldNotBe) {
        try {
            new SynchronizationManager(driver).fluentWait()
                    .until(f -> !driver.findElement(elementLocator).getText().equals(textShouldNotBe));
        } catch (org.openqa.selenium.TimeoutException e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
        }
        return true;
    }

    /**
     * Waits for the attribute of the specified element to be a specific value.
     *
     * @param driver         the WebDriver instance used to interact with the browser
     * @param elementLocator the locator used to find the element
     * @param att            the name of the attribute to wait for
     * @param expectedValue  the expected value of the attribute
     * @return true if the attribute value matches the expected value within the timeout period, otherwise false
     */
    public boolean waitForElementAttributeToBe(WebDriver driver, By elementLocator, String att, String expectedValue) {
        try {
            new SynchronizationManager(driver).fluentWait(false)
                    .until(f -> driver.findElement(elementLocator).getAttribute(att).equals(expectedValue));
        } catch (org.openqa.selenium.TimeoutException e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
        }
        return true;
    }

    public WebElement getWebElementFromPointUsingJavascript(WebDriver driver, List<Integer> point, boolean scrollToElement) {
        if (DriverFactoryHelper.isNotMobileExecution()) {
            if (Boolean.TRUE.equals(scrollToElement)) {
                return (WebElement) ((JavascriptExecutor) driver).executeScript(JavaScriptHelper.ELEMENT_SCROLL_TO_VIEWPORT.getValue(), point.get(0), point.get(1));
            } else {
                return (WebElement) ((JavascriptExecutor) driver).executeScript("return document.elementFromPoint(arguments[0], arguments[1])", point.get(0), point.get(1));
            }
        } else {
            return null;
        }
    }

    public void clickUsingJavascript(WebDriver driver, By elementLocator) {
        if (DriverFactoryHelper.isNotMobileExecution()) {
            ((JavascriptExecutor) driver).executeScript("arguments[arguments.length - 1].click();", this.identifyUniqueElement(driver, elementLocator).get(1));
        }
    }

    public void dragAndDropUsingJavascript(WebDriver driver, By sourceElementLocator, By destinationElementLocator) {
        if (DriverFactoryHelper.isNotMobileExecution()) {
            JavascriptExecutor js = (JavascriptExecutor) driver;
            String jQueryLoader = JavaScriptHelper.LOAD_JQUERY.getValue();
            js.executeAsyncScript(jQueryLoader /* , http://localhost:8080/jquery-1.7.2.js */);
            String dragAndDropHelper = JavaScriptHelper.ELEMENT_DRAG_AND_DROP.getValue();
            dragAndDropHelper = dragAndDropHelper + "$(arguments[0]).simulateDragDrop({dropTarget:arguments[1]});";
            ((JavascriptExecutor) driver).executeScript(dragAndDropHelper, this.identifyUniqueElement(driver, sourceElementLocator).get(1), this.identifyUniqueElement(driver, destinationElementLocator).get(1));
        }
    }

    public void dragAndDropUsingActions(WebDriver driver, By sourceElementLocator, By destinationElementLocator) {
        new Actions(driver).dragAndDrop(((WebElement) this.identifyUniqueElement(driver, sourceElementLocator).get(1)), ((WebElement) this.identifyUniqueElement(driver, destinationElementLocator).get(1))).build().perform();
    }

    public void executeNativeMobileCommandUsingJavascript(WebDriver driver, String command, Map<String, String> parameters) {
        ((JavascriptExecutor) driver).executeScript(command, parameters);
    }

    public void submitFormUsingJavascript(WebDriver driver, By elementLocator) {
        if (DriverFactoryHelper.isNotMobileExecution()) {
            ((JavascriptExecutor) driver).executeScript("arguments[0].submit();", this.identifyUniqueElement(driver, elementLocator).get(1));
        }
    }

    public void changeWebElementVisibilityUsingJavascript(WebDriver driver, By elementLocator, boolean desiredIsVisibleState) {
        if (DriverFactoryHelper.isNotMobileExecution()) {

            if (Boolean.TRUE.equals(desiredIsVisibleState)) {
                ((JavascriptExecutor) driver).executeScript("arguments[0].setAttribute('style', 'display:block !important;');", this.identifyUniqueElement(driver, elementLocator).get(1));
            } else {
                ((JavascriptExecutor) driver).executeScript("arguments[0].setAttribute('style', 'display:none');", this.identifyUniqueElement(driver, elementLocator).get(1));
            }
        }
    }

    public boolean setValueUsingJavascript(WebDriver driver, By elementLocator, String value) {
        try {
            if (DriverFactoryHelper.isNotMobileExecution()) {
                performActionAgainstUniqueElementIgnoringVisibility(driver, elementLocator, ElementAction.SET_VALUE_USING_JAVASCRIPT, value);
            }
            return true;
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
        }
    }

    public boolean setValueUsingJavascript(WebDriver driver, ElementInformation elementInformation, String value) {
        try {
            if (DriverFactoryHelper.isNotMobileExecution()) {
                try {
                    ((JavascriptExecutor) driver).executeScript("arguments[0].value = arguments[1];", elementInformation.getFirstElement(), value);
                } catch (WebDriverException webDriverException) {
                    this.performActionAgainstUniqueElementIgnoringVisibility(driver, elementInformation.getLocator(), ElementAction.SET_VALUE_USING_JAVASCRIPT, value);
                }
            }
            return true;
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
        }
    }

    public String suggestNewXpathUsingJavascript(WebDriver driver, WebElement targetElement) {
        ReportHelper.disableLogging();
        var suggestedXpath = suggestNewXpathUsingJavascript(driver, targetElement, null);
        ReportHelper.enableLogging();
        return suggestedXpath;
    }

    public String suggestNewXpathUsingJavascript(WebDriver driver, WebElement targetElement, By deprecatedElementLocator) {
        if (DriverFactoryHelper.isNotMobileExecution()) {
            // attempt to find an optimal xpath for the targetElement
            var maximumXpathNodes = 6;
            var newXpath = "";
            for (var i = 0; i < maximumXpathNodes; i++) {
                String xpathFindingAlgorithm = getXpathFindingAlgorithm(i);

                try {
                    newXpath = (String) ((JavascriptExecutor) driver).executeScript(xpathFindingAlgorithm, targetElement);
                    if (newXpath != null && driver.findElements(By.xpath(newXpath)).size() == 1) {
                        // if unique element was found, break, else keep iterating
                        break;
                    }
                } catch (JavascriptException e) {
                    ReportManagerHelper.logDiscrete(e);
                    ReportManager.logDiscrete("Failed to suggest a new XPath for the target element with this deprecated locator \"" + deprecatedElementLocator + "\"");
                }
            }
            if (newXpath != null) {
                boolean initialLoggingState = ReportManagerHelper.getDiscreteLogging();
                ReportManagerHelper.setDiscreteLogging(false);
                ReportManager.log("New AI-Suggested XPath \"" + newXpath.replace("\"", "'") + "\"");
                ReportManagerHelper.setDiscreteLogging(initialLoggingState);
                return newXpath;
            } else {
                ReportManager.log("Failed to suggest a new XPath for the target element with this deprecated locator \"" + deprecatedElementLocator + "\"");
                return null;
            }
        } else {
            return null;
        }
    }

    private String getXpathFindingAlgorithm(int i) {
        String xpathFindingAlgorithm = JavaScriptHelper.ELEMENT_GET_XPATH.getValue();
        /*
         * $$GetIndex$$ $$GetId$$ $$GetName$$ $$GetType$$ $$GetClass$$ $$GetText$$
         * $$MaxCount$$
         */
        var maxCount = String.valueOf(i);
        var getId = String.valueOf(true);
        String getIndex;
        String getName;
        String getType;
        String getClass;
        String getText;
        getIndex = getName = getType = getClass = getText = String.valueOf(false);

        if (i == 0) {
            maxCount = String.valueOf(1);
        } else if (i == 1 || i == 2) {
            getName = String.valueOf(true);
            getType = String.valueOf(true);
            getText = String.valueOf(true);
        } else if (i == 3 || i == 4) {
            getName = String.valueOf(true);
            getType = String.valueOf(true);
            getClass = String.valueOf(true);
            getText = String.valueOf(true);
        } else {
            getIndex = String.valueOf(true);
            getName = String.valueOf(true);
            getType = String.valueOf(true);
            getText = String.valueOf(true);
            getClass = String.valueOf(true);
        }

        xpathFindingAlgorithm = xpathFindingAlgorithm.replaceAll("\\$\\$MaxCount\\$\\$", maxCount).replaceAll("\\$\\$GetId\\$\\$", getId).replaceAll("\\$\\$GetIndex\\$\\$", getIndex).replaceAll("\\$\\$GetName\\$\\$", getName).replaceAll("\\$\\$GetType\\$\\$", getType).replaceAll("\\$\\$GetClass\\$\\$", getClass).replaceAll("\\$\\$GetText\\$\\$", getText);
        return xpathFindingAlgorithm;
    }

    public List<Object> takeScreenshot(WebDriver driver, By elementLocator, String actionName, String testData, boolean passFailStatus) {
        if (passFailStatus) {
            try {
                if (elementLocator != null) {
                    return new ScreenshotManager().takeScreenshot(driver, elementLocator, actionName, true);
                } else if (testData != null) {
                    return new ScreenshotManager().takeScreenshot(driver, null, actionName, true);
                }
                // else only happens when switching to default content so there is no need to
                // take a screenshot
            } catch (Exception e) {
                ReportManagerHelper.logDiscrete(e);
                ReportManager.logDiscrete("Failed to take a screenshot of the element as it doesn't exist anymore. Taking a screenshot of the whole page.");
                return new ScreenshotManager().takeScreenshot(driver, null, actionName, true);
            }
        } else {
            return new ScreenshotManager().takeScreenshot(driver, null, actionName, false);
        }
        return new ArrayList<>();
    }

    public String getElementName(WebDriver driver, By elementLocator) {
        if (SHAFT.Properties.reporting.captureElementName()) {
            try {
                var accessibleName = ((WebElement) identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1)).getAccessibleName();
                if (accessibleName != null && !accessibleName.isBlank()) {
                    return accessibleName;
                }
            } catch (Throwable throwable) {
                var rootCause = Throwables.getRootCause(throwable).getClass();

                if (rootCause.equals(NoSuchElementException.class) || rootCause.equals(InvalidSelectorException.class) || rootCause.equals(MultipleElementsFoundException.class)) {

                    throw throwable;
                }
                //happens on some elements that show unhandled inspector error
                //this exception is thrown on some older selenium grid instances, I saw it with firefox running over selenoid
                //ignore
            }
        }
        return JavaHelper.formatLocatorToString(elementLocator);
    }

    public boolean performClipboardActions(WebDriver driver, ClipboardAction action) {
        try {
            Keys cmdCtrl = SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(Platform.MAC.name()) ? Keys.COMMAND : Keys.CONTROL;
            switch (action) {
                case COPY -> (new Actions(driver)).keyDown(cmdCtrl).sendKeys("c").keyUp(cmdCtrl).perform();
                case PASTE -> (new Actions(driver)).keyDown(cmdCtrl).sendKeys("v").keyUp(cmdCtrl).perform();
                case CUT -> (new Actions(driver)).keyDown(cmdCtrl).sendKeys("x").keyUp(cmdCtrl).perform();
                case SELECT_ALL -> (new Actions(driver)).keyDown(cmdCtrl).sendKeys("a").keyUp(cmdCtrl).perform();
                case UNSELECT_ALL -> (new Actions(driver)).sendKeys(Keys.ESCAPE).perform();
                default -> {
                    return false;
                }
            }
            return true;
        } catch (HeadlessException e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
        }
    }

    private void performType(WebDriver driver, ElementInformation elementInformation, String text) {
        if (driver instanceof AppiumDriver appiumDriver) {
            //mobile execution
            try {
                (elementInformation.getFirstElement()).sendKeys(text);
            } catch (WebDriverException webDriverException2) {
                performActionAgainstUniqueElement(appiumDriver, elementInformation.getLocator(), ElementAction.SEND_KEYS, text);
            }
        } else {
            //desktop execution
            if (SHAFT.Properties.flags.attemptToClickBeforeTyping()) {
                try {
                    (elementInformation.getFirstElement()).click();
                    (elementInformation.getFirstElement()).sendKeys(text);
                } catch (WebDriverException webDriverException) {
                    try {
                        (elementInformation.getFirstElement()).sendKeys(text);
                    } catch (WebDriverException webDriverException2) {
                        performActionAgainstUniqueElement(driver, elementInformation.getLocator(), ElementAction.SEND_KEYS, text);
                    }
                }

            } else {
                try {
                    (elementInformation.getFirstElement()).sendKeys(text);
                } catch (WebDriverException webDriverException2) {
                    performActionAgainstUniqueElement(driver, elementInformation.getLocator(), ElementAction.SEND_KEYS, text);
                }
            }

        }
    }

    public String readElementText(WebDriver driver, ElementInformation elementInformation) {
        String elementText;
        try {
            elementText = (elementInformation.getFirstElement()).getText();
        } catch (WebDriverException webDriverException) {
            elementText = ElementInformation.fromList(this.performActionAgainstUniqueElementIgnoringVisibility(driver, elementInformation.getLocator(), ElementAction.GET_TEXT)).getActionResult();
        }
        if ((elementText == null || elementText.isBlank()) && !DriverFactoryHelper.isMobileNativeExecution()) {
            try {
                elementText = (elementInformation.getFirstElement()).getAttribute(ElementActionsHelper.TextDetectionStrategy.CONTENT.getValue());
            } catch (WebDriverException webDriverException) {
                elementText = ElementInformation.fromList(this.performActionAgainstUniqueElementIgnoringVisibility(driver, elementInformation.getLocator(), ElementAction.GET_CONTENT)).getActionResult();
            }
        }
        if ((elementText == null || elementText.isBlank()) && !DriverFactoryHelper.isMobileNativeExecution()) {
            try {
                elementText = (elementInformation.getFirstElement()).getAttribute(ElementActionsHelper.TextDetectionStrategy.VALUE.getValue());
            } catch (WebDriverException webDriverException) {
                elementText = ElementInformation.fromList(this.performActionAgainstUniqueElementIgnoringVisibility(driver, elementInformation.getLocator(), ElementAction.GET_VALUE)).getActionResult();
            }
        }
        if (elementText == null) {
            elementText = "";
        }
        return elementText;
    }

    private void clearBeforeTyping(WebDriver driver, ElementInformation elementInformation) {
        if (SHAFT.Properties.flags.attemptClearBeforeTyping()) {
            if (SHAFT.Properties.flags.attemptClearBeforeTypingUsingBackspace()) {
                clearBeforeTypingUsingBackSpace(driver, elementInformation);
            } else {
                clearBeforeTypingUsingNativeClear(driver, elementInformation);
            }
        }
    }

    private void clearBeforeTypingUsingNativeClear(WebDriver driver, ElementInformation elementInformation) {
        // try clearing text
        try {
            elementInformation.getFirstElement().clear();
        } catch (Throwable throwable) {
            this.performActionAgainstUniqueElement(driver, elementInformation.getLocator(), ElementAction.CLEAR);
        }
        var currentTextAfterClearingUsingNativeClear = readElementText(driver, elementInformation);
        if (currentTextAfterClearingUsingNativeClear.isBlank()) {
            ReportManagerHelper.logDiscrete("Text cleared Using Native Clear", Level.DEBUG);
        } else {
            this.failAction(driver, "Expected to clear existing text, but ended up with: \"" + currentTextAfterClearingUsingNativeClear + "\"", elementInformation.getLocator());
        }
    }

    private void clearBeforeTypingUsingBackSpace(WebDriver driver, ElementInformation elementInformation) {
        var currentText = readElementText(driver, elementInformation);
        // try deleting letter by letter using backspaces
        for (var ignored : currentText.toCharArray()) {
            try {
                (elementInformation.getFirstElement()).sendKeys(Keys.BACK_SPACE);
            } catch (WebDriverException webDriverException) {
                this.performActionAgainstUniqueElement(driver, elementInformation.getLocator(), ElementAction.BACKSPACE);
            }
        }
        var currentTextAfterClearingUsingBackSpace = readElementText(driver, elementInformation);
        if (currentTextAfterClearingUsingBackSpace.isBlank()) {
            ReportManagerHelper.logDiscrete("Text cleared Using Backspace.", Level.DEBUG);
        } else {
            this.failAction(driver, "Expected to clear existing text, but ended up with: \"" + currentTextAfterClearingUsingBackSpace + "\"", elementInformation.getLocator());
        }
    }

    private String confirmTextWasTypedCorrectly(WebDriver driver, ElementInformation elementInformation, String adjustedTargetText) {
        //get a fresh instance of the element
        var updatedElementInformation = ElementInformation.fromList(identifyUniqueElementIgnoringVisibility(driver, elementInformation.getLocator()));
        String actualTextAfterPerformType = readElementText(driver, elementInformation);
        if (adjustedTargetText.equals(actualTextAfterPerformType) || OBFUSCATED_STRING.repeat(adjustedTargetText.length()).equals(actualTextAfterPerformType)) {
            return adjustedTargetText;
        } else {
            // attempt once to type using javascript then confirm typing was successful
            // again
            this.setValueUsingJavascript(driver, elementInformation, adjustedTargetText);
            var textAfterSettingValueUsingJavascript = new ElementActions(driver).getText(elementInformation.getLocator());
            if (textAfterSettingValueUsingJavascript.isEmpty()) {
                return adjustedTargetText;
            }
            return textAfterSettingValueUsingJavascript;
        }

    }

    // TypeWrapper responsible for clearing 'if user enabled any clear flag'
    // and performing type ,
    // and double check if typed correctly 'if user enabled the flag'
    public String typeWrapper(WebDriver driver, ElementInformation elementInformation, String targetText) {
        clearBeforeTyping(driver, elementInformation);
        var adjustedTargetText = targetText != null && !targetText.isEmpty() ? targetText : "";
        performType(driver, elementInformation, adjustedTargetText);
        //sometimes the text is returned as empty
        if (SHAFT.Properties.flags.forceCheckTextWasTypedCorrectly()) {
            return confirmTextWasTypedCorrectly(driver, elementInformation, adjustedTargetText);
        } else {
            return adjustedTargetText;
        }
    }

    public boolean isFoundInStacktrace(Class<?> classObject, Throwable throwable) {
        var targetClassName = classObject.getName();
        for (StackTraceElement element : throwable.getStackTrace()) {
            if (element.getClassName().equals(targetClassName)) {
                return true;
            }
        }
        return false;
    }

    public List<Object> performActionAgainstUniqueElement(WebDriver driver, By elementLocator, Object... action) {
        return identifyUniqueElement(driver, elementLocator, true, action);
    }

    public List<Object> performActionAgainstUniqueElementIgnoringVisibility(WebDriver driver, By elementLocator, Object... action) {
        return identifyUniqueElement(driver, elementLocator, false, action);
    }

    public List<Object> identifyUniqueElement(WebDriver driver, By elementLocator) {
        return identifyUniqueElement(driver, elementLocator, true);
    }

    public List<Object> identifyUniqueElementIgnoringVisibility(WebDriver driver, By elementLocator) {
        return identifyUniqueElement(driver, elementLocator, false);
    }

    private List<Object> identifyUniqueElement(WebDriver driver, By elementLocator, boolean checkForVisibility, Object... action) {
        var matchingElementsInformation = getMatchingElementsInformation(driver, elementLocator, checkForVisibility, action);

        if (elementLocator != null) {
            // in case of regular locator
            switch (Integer.parseInt(matchingElementsInformation.get(0).toString())) {
                case 0 -> {
                    reportActionResult(driver, null, null, null, null, null, false);
                    if (matchingElementsInformation.size() > 2 && matchingElementsInformation.get(2) instanceof Throwable) {
                        FailureReporter.fail(ElementActionsHelper.class, "Failed to identify unique element using this locator \"" + JavaHelper.formatLocatorToString(elementLocator) + "\"", (Throwable) matchingElementsInformation.get(2));
                    }
                    FailureReporter.fail("Failed to identify unique element using this locator \"" + JavaHelper.formatLocatorToString(elementLocator) + "\"");
                }
                case 1 -> {
                    return matchingElementsInformation;
                }
                default -> {
                    if (SHAFT.Properties.flags.forceCheckElementLocatorIsUnique() && !(elementLocator instanceof RelativeLocator.RelativeBy)) {
                        reportActionResult(driver, null, null, null, null, null, false);
                        FailureReporter.fail(ElementActionsHelper.class, "Failed to identify unique element", new MultipleElementsFoundException("Multiple elements found matching this locator \"" + JavaHelper.formatLocatorToString(elementLocator) + "\""));
                    }
                    return matchingElementsInformation;
                }
            }
        } else {
            // in case locator is null
            failAction(driver, "element locator is NULL.", null);
        }
        //unreachable code
        return matchingElementsInformation;
    }

    public List<Object> getMatchingElementsInformation(WebDriver driver, By elementLocator, boolean checkForVisibility, Object... action) {
        if (elementLocator == null) {
            var elementInformation = new ArrayList<>();
            elementInformation.add(0);
            elementInformation.add(null);
            return elementInformation;
        }
        if (!elementLocator.equals(By.tagName("html"))) {
            return this.waitForElementPresence(driver, elementLocator, checkForVisibility, action);
        } else {
            //if locator is just tag-name html
            var elementInformation = new ArrayList<>();
            elementInformation.add(1);
            elementInformation.add(null);
            return elementInformation;
        }
    }

    /**
     * Returns the number of elements that match a certain elementLocator
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return integer value that represents the number of elements that match the
     * desired elementLocator
     */
    public int getElementsCount(WebDriver driver, By elementLocator) {
        return Integer.parseInt(this.getMatchingElementsInformation(driver, elementLocator, false).getFirst().toString());
    }

    public void passAction(WebDriver driver, By elementLocator, String testData, List<Object> screenshot, String elementName) {
        //TODO: open calling methods, and test if Appium can also fetch the element name instead of passing null
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        List<List<Object>> attachments = new LinkedList<>();
        attachments.add(screenshot);
        passAction(driver, elementLocator, actionName, testData, attachments, elementName);
    }

    public void passAction(WebDriver driver, By elementLocator, String actionName, String testData, List<List<Object>> screenshots, String elementName) {
        reportActionResult(driver, actionName, testData, elementLocator, screenshots, elementName, true);
    }

    public void failAction(WebDriver driver, By elementLocator, Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, null, elementLocator, null, rootCauseException);
    }

    public void failAction(WebDriver driver, String testData, By elementLocator, Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, testData, elementLocator, null, rootCauseException);
    }

    public void failAction(WebDriver driver, String testData, By elementLocator, List<List<Object>> attachments, Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, testData, elementLocator, attachments, rootCauseException);
    }

    public void failAction(WebDriver driver, String actionName, String testData, By elementLocator, List<List<Object>> screenshots, Throwable... rootCauseException) {
        //TODO: merge all fail actions, make all methods call this one, get elementName where applicable instead of reporting null
        //this condition works if this is the first level of failure, but the first level is usually caught by the calling method

//        boolean skipPageScreenshot = rootCauseException.length >= 1 && (
//                TimeoutException.class.getName().equals(rootCauseException[0].getClass().getName()) //works to capture fluent wait failure
//                        || (
//                        rootCauseException[0].getMessage().contains("Identify unique element")
//                                && isFoundInStacktrace(ValidationsHelper.class, rootCauseException[0])
//                )//works to capture calling elementAction failure in case this is an assertion
//        );

        //don't take a second screenshot in case of validation failure  because the original element action will have always failed first
        boolean skipPageScreenshot = rootCauseException.length >= 1 && (isFoundInStacktrace(ValidationsHelper.class, rootCauseException[0]) && isFoundInStacktrace(ElementActionsHelper.class, rootCauseException[0]));

        String elementName = elementLocator != null ? JavaHelper.formatLocatorToString(elementLocator) : "";
        if (elementLocator != null && (rootCauseException.length >= 1 && Throwables.getRootCause(rootCauseException[0]).getClass() != MultipleElementsFoundException.class && Throwables.getRootCause(rootCauseException[0]).getClass() != NoSuchElementException.class && Throwables.getRootCause(rootCauseException[0]).getClass() != InvalidSelectorException.class)) {
            try {
                var accessibleName = ((WebElement) this.identifyUniqueElement(driver, elementLocator).get(1)).getAccessibleName();
                if (accessibleName != null && !accessibleName.isBlank()) {
                    elementName = accessibleName;
                }
            } catch (WebDriverException e) {
                //happens on some elements that show unhandled inspector error
                //this exception is thrown on some older selenium grid instances, I saw it with firefox running over selenoid
                //ignore
            }
        }

        String message;
        if (skipPageScreenshot) {
            //don't try to take a screenshot again and set element locator to null in case element was not found by timeout or by nested element actions call
            message = createReportMessage(actionName, testData, elementName, false);
            ReportManager.logDiscrete(message);
        } else {
            if (rootCauseException.length >= 1) {
                message = reportActionResult(driver, actionName, testData, elementLocator, screenshots, elementName, false, rootCauseException[0]);
            } else {
                message = reportActionResult(driver, actionName, testData, null, screenshots, elementName, false);
            }
        }
        if (rootCauseException.length >= 1) {
            Assert.fail(message, rootCauseException[0]);
        } else {
            Assert.fail(message);
        }
    }

    public String createReportMessage(String actionName, String testData, String elementName, Boolean passFailStatus) {
        String message = "";

        if (Boolean.FALSE.equals(passFailStatus)) {
            message = message + "Failed to ";
        }

        actionName = JavaHelper.convertToSentenceCase(actionName);

        message = message + actionName;

        if (testData != null && !testData.isEmpty() && testData.length() < 500) {
            message = message + " \"" + testData.trim() + "\"";
        }

        if ((elementName != null && !elementName.isEmpty())) {
            var preposition = getPreposition(actionName);
            message = message + preposition + "\"" + elementName.trim() + "\"";
        }

        message = message + ".";
        return message;
    }

    private String getPreposition(String actionName) {
        var preposition = " ";
        if (actionName.toLowerCase().contains("type") || actionName.toLowerCase().contains("setproperty value using javascript")) {
            preposition = " into ";
        } else if (actionName.toLowerCase().contains("get") || actionName.toLowerCase().contains("select")) {
            preposition = " from ";
        } else if (actionName.toLowerCase().contains("clipboard")) {
            preposition = " on ";
        } else if (actionName.toLowerCase().contains("drag and drop") || actionName.toLowerCase().contains("key press") || actionName.toLowerCase().contains("wait") || actionName.toLowerCase().contains("submit") || actionName.toLowerCase().contains("switch")) {
            preposition = " against ";
        } else if (actionName.toLowerCase().contains("hover")) {
            preposition = " over ";
        }
        return preposition;
    }

    private List<List<Object>> createReportAttachments(WebDriver driver, String actionName, String testData, By elementLocator, List<List<Object>> screenshots, Boolean passFailStatus, Throwable... rootCauseException) {
        actionName = JavaHelper.convertToSentenceCase(actionName);
        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("Element Action Test Data - " + actionName, "Actual Value", testData);
            attachments.add(actualValueAttachment);
        }
        if (screenshots != null && !screenshots.equals(new ArrayList<>())) {
            // screenshot taken before action (in case of click)
            attachments.addAll(screenshots);
        } else if (driver != null) {
            List<Object> newScreenshot;
            if (actionName.equals("Identify unique element")) {
                newScreenshot = takeScreenshot(driver, null, actionName, testData, passFailStatus);
            } else {
                newScreenshot = takeScreenshot(driver, elementLocator, actionName, testData, passFailStatus);
            }
            if (newScreenshot != null && !newScreenshot.isEmpty()) {
                attachments.add(newScreenshot);
            }
        }

        if (driver != null && (Boolean.FALSE.equals(passFailStatus)
                || SHAFT.Properties.visuals.whenToTakePageSourceSnapshot().equalsIgnoreCase("always"))) {
            var logMessage = "";
            var pageSnapshot = new BrowserActionsHelper(false).capturePageSnapshot(driver);
            if (pageSnapshot.startsWith("From: <Saved by Blink>")) {
                logMessage = "page snapshot";
            } else if (pageSnapshot.startsWith("<html")) {
                logMessage = "page HTML";
            }
            List<Object> sourceAttachment = Arrays.asList(actionName, logMessage, pageSnapshot);
            attachments.add(sourceAttachment);
        }

        if (rootCauseException != null && rootCauseException.length >= 1) {
            List<Object> actualValueAttachment = Arrays.asList("Element Action Exception - " + actionName, "Stacktrace", ReportManagerHelper.formatStackTraceToLogEntry(rootCauseException[0]));
            attachments.add(actualValueAttachment);
        }

        if (attachments.isEmpty() || (attachments.size() == 1 && attachments.getFirst().isEmpty())) {
            return null;
        } else {
            return attachments;
        }
    }

    public String reportActionResult(WebDriver driver, String actionName, String testData, By elementLocator, List<List<Object>> screenshots, String elementName, Boolean passFailStatus, Throwable... rootCauseException) {
        if (actionName == null) {
            actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        }
        String message = createReportMessage(actionName, testData, elementName, passFailStatus);
        List<List<Object>> attachments = createReportAttachments(driver, actionName, testData, elementLocator, screenshots, passFailStatus, rootCauseException);

        if (message.contains("Failed") && rootCauseException != null && rootCauseException.length > 0) {
            String rootCause = " Root cause: \"" + Throwables.getRootCause(rootCauseException[0]).getClass().getName() + ": " + Throwables.getRootCause(rootCauseException[0]).getLocalizedMessage().split("\n")[0] + "\"";
            message += rootCause;
        }
        if (!isSilent || actionName.equals("identifyUniqueElement")) {
            if (attachments != null && !attachments.equals(new ArrayList<>())) {
                ReportManagerHelper.log(message, attachments);
            } else {
                ReportManager.log(message);
            }
        }
        return message;
    }

    @Getter
    public enum TextDetectionStrategy {
        TEXT("text"), CONTENT("textContent"), VALUE("value"), UNDEFINED("undefined");
        private final String value;

        TextDetectionStrategy(String strategy) {
            this.value = strategy;
        }

    }
}
