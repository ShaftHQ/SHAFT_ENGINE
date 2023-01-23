package io.github.shafthq.shaft.gui.element;

import com.shaft.cli.FileActions;
import com.shaft.gui.element.SikuliActions;
import com.shaft.tools.io.ReportManager;
import io.github.shafthq.shaft.driver.DriverFactoryHelper;
import io.github.shafthq.shaft.gui.image.ImageProcessingActions;
import io.github.shafthq.shaft.gui.image.ScreenshotManager;
import io.github.shafthq.shaft.tools.io.helpers.ReportManagerHelper;
import io.github.shafthq.shaft.tools.support.JavaHelper;
import io.github.shafthq.shaft.tools.support.JavaScriptHelper;
import io.github.shafthq.shaft.validations.helpers.ValidationsHelper;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.interactions.Locatable;
import org.openqa.selenium.support.locators.RelativeLocator;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.FluentWait;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.sikuli.script.App;
import org.sikuli.script.Pattern;
import org.sikuli.script.Screen;
import org.testng.Assert;

import java.awt.*;
import java.time.Duration;
import java.util.List;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

public class ElementActionsHelper {
    public static final String OBFUSCATED_STRING = "•";
    private static long DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT = Integer
            .parseInt(System.getProperty("defaultElementIdentificationTimeout").trim()) * 1000L; //milliseconds
    private static final int ELEMENT_IDENTIFICATION_POLLING_DELAY = 100; // milliseconds
    private static final boolean FORCE_CHECK_FOR_ELEMENT_VISIBILITY = Boolean
            .parseBoolean(System.getProperty("forceCheckForElementVisibility").trim());

    private ElementActionsHelper() {
        throw new IllegalStateException("Utility class");
    }

    public static int waitForElementPresenceWithReducedTimeout(WebDriver driver, By elementLocator) {
        DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT = 300; //this is used for faster mobile native scrolling. default for ios is 200 and for android is 250, this covers both
        var numberOfFoundElements = waitForElementPresence(driver, elementLocator);
        DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT = Integer
                .parseInt(System.getProperty("defaultElementIdentificationTimeout").trim()) * 1000L;
        return Integer.parseInt(numberOfFoundElements.get(0).toString());
    }

    public static List<Object> waitForElementPresence(WebDriver driver, By elementLocator) {
        return waitForElementPresence(driver, elementLocator, 1, FORCE_CHECK_FOR_ELEMENT_VISIBILITY);
    }

    public static List<Object> waitForElementPresence(WebDriver driver, By elementLocator, int numberOfAttempts) {
        return waitForElementPresence(driver, elementLocator, numberOfAttempts, FORCE_CHECK_FOR_ELEMENT_VISIBILITY);
    }

    public static List<Object> waitForElementPresence(WebDriver driver, By elementLocator, boolean checkForVisibility) {
        return waitForElementPresence(driver, elementLocator, 1, checkForVisibility);
    }

    public static List<Object> waitForElementPresence(WebDriver driver, String elementReferenceScreenshot) {
        long startTime = System.currentTimeMillis();
        long elapsedTime;
        List<Integer> coordinates;
        boolean isFound = false;
        byte[] currentScreenImage;

        List<Object> returnedValue = new LinkedList<>();
        if (FileActions.getInstance().doesFileExist(elementReferenceScreenshot)) {
            do {
                try {
                    Thread.sleep(ELEMENT_IDENTIFICATION_POLLING_DELAY);
                } catch (InterruptedException e) {
                    ReportManagerHelper.logDiscrete(e);
                }
                currentScreenImage = ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
                coordinates = ImageProcessingActions.findImageWithinCurrentPage(elementReferenceScreenshot, currentScreenImage);
                if (!Collections.emptyList().equals(coordinates)) {
                    isFound = true;
                }
                elapsedTime = System.currentTimeMillis() - startTime;
            } while (!isFound && elapsedTime < DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT);
            returnedValue.add(currentScreenImage);
            returnedValue.add(FileActions.getInstance().readFileAsByteArray(elementReferenceScreenshot));
            returnedValue.add(coordinates);
        } else {
            // reference screenshot doesn't exist
            currentScreenImage = ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
            returnedValue.add(currentScreenImage);
            returnedValue.add(new byte[0]);
            returnedValue.add(Collections.emptyList());
        }
        return returnedValue;
    }

    private static boolean isValidToCheckForVisibility(By elementLocator, boolean checkForVisibility) {
        return checkForVisibility && !formatLocatorToString(elementLocator).contains("input[@type='file']")
                && !elementLocator.equals(By.tagName("html"));
    }

    private static boolean isSafariBrowser() {
        boolean isSafariBrowser= false;
        try {
            isSafariBrowser= DriverFactoryHelper.getTargetBrowserName().toLowerCase().contains("safari");
        }
        catch (NullPointerException exception){
        }
        return isSafariBrowser ;
    }

    public static ArrayList<Class<? extends Exception>> getExpectedExceptions(boolean isValidToCheckForVisibility) {
        ArrayList<Class<? extends Exception>> expectedExceptions = new ArrayList<>();
        expectedExceptions.add(org.openqa.selenium.NoSuchElementException.class);
        expectedExceptions.add(org.openqa.selenium.StaleElementReferenceException.class);
        expectedExceptions.add(org.openqa.selenium.ElementNotInteractableException.class);
        if (isValidToCheckForVisibility) {
            expectedExceptions.add(org.openqa.selenium.InvalidElementStateException.class);
            expectedExceptions.add(org.openqa.selenium.interactions.MoveTargetOutOfBoundsException.class);
        }
        if (isSafariBrowser()) {
            // the generic exception is added to handle a case with WebKit whereby the browser doesn't state the cause of the issue
            expectedExceptions.add(org.openqa.selenium.WebDriverException.class);
        }
        return expectedExceptions;
    }

    public static List<Object> waitForElementPresence(WebDriver driver, By elementLocator, int numberOfAttempts, boolean checkForVisibility) {
        boolean isValidToCheckForVisibility = isValidToCheckForVisibility(elementLocator, checkForVisibility);

        var isMobileExecution = DriverFactoryHelper.isMobileNativeExecution() || DriverFactoryHelper.isMobileWebExecution();

        try {
            AtomicBoolean attemptedToUseActionsToScrollToElement = new AtomicBoolean(false);
            return new FluentWait<>(driver)
                    .withTimeout(Duration.ofMillis(
                            DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT * numberOfAttempts))
                    .pollingEvery(Duration.ofMillis(ELEMENT_IDENTIFICATION_POLLING_DELAY))
                    .ignoreAll(getExpectedExceptions(isValidToCheckForVisibility))
                    .until(nestedDriver -> {
                        WebElement targetElement = nestedDriver.findElement(elementLocator);
                        if (isValidToCheckForVisibility) {
                            if (!isMobileExecution) {
                                if (isSafariBrowser() || attemptedToUseActionsToScrollToElement.get()) {
                                    ((Locatable) targetElement).getCoordinates().inViewPort();
                                } else {
                                    attemptedToUseActionsToScrollToElement.set(true);
                                    new Actions(driver).scrollToElement(targetElement).perform();
                                }
                            } else {
                                targetElement.isDisplayed();
                            }
                        }
                        var elementInformation = new ArrayList<>();
                        elementInformation.add(nestedDriver.findElements(elementLocator).size());
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

    public static List<Object> scrollToFindElement(WebDriver driver, By elementLocator) {
        try {
            return new FluentWait<>(driver)
                    .withTimeout(Duration.ofMillis(
                            DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT))
                    .pollingEvery(Duration.ofMillis(ELEMENT_IDENTIFICATION_POLLING_DELAY))
                    .ignoreAll(getExpectedExceptions(true))
                    .until(nestedDriver -> {
                        WebElement targetElement;
                        try {
                            targetElement = nestedDriver.findElement(elementLocator);
                        } catch (NoSuchElementException noSuchElementException) {
                            new Actions(nestedDriver).scrollByAmount(0, nestedDriver.manage().window().getSize().getHeight()).perform();
                            targetElement = nestedDriver.findElement(elementLocator);
                        }
                        var elementInformation = new ArrayList<>();
                        elementInformation.add(nestedDriver.findElements(elementLocator).size());
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


    public static boolean waitForElementToBeClickable(WebDriver driver, By elementLocator, Optional<String> actionToExecute) {
        var clickUsingJavascriptWhenWebDriverClickFails = Boolean.parseBoolean(System.getProperty("clickUsingJavascriptWhenWebDriverClickFails"));

        if (!DriverFactoryHelper.isMobileNativeExecution()) {
            try {
                (new WebDriverWait(driver, Duration.ofMillis(DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT)))
                        .until(ExpectedConditions.elementToBeClickable(elementLocator));


                var expectedExceptions = getExpectedExceptions(true);
                if (!clickUsingJavascriptWhenWebDriverClickFails) {
                    expectedExceptions.add(ElementClickInterceptedException.class);
                }

                return new FluentWait<>(driver)
                        .withTimeout(Duration.ofMillis(
                                DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT))
                        .pollingEvery(Duration.ofMillis(ELEMENT_IDENTIFICATION_POLLING_DELAY))
                        .ignoreAll(expectedExceptions)
                        .until(nestedDriver -> {
                            if (actionToExecute.isPresent()) {
                                switch (actionToExecute.get().toLowerCase()) {
                                    case "click" -> driver.findElement(elementLocator).click();
                                    case "clickandhold" ->
                                            (new Actions(driver)).clickAndHold(driver.findElement(elementLocator)).build().perform();
                                }
                            }
                            return true;
                        });
            } catch (org.openqa.selenium.TimeoutException e) {
                if (clickUsingJavascriptWhenWebDriverClickFails && actionToExecute.isPresent() && actionToExecute.get().equalsIgnoreCase("click")) {
                    ElementActionsHelper.clickUsingJavascript(driver, elementLocator);
                    return true;
                }
                ReportManagerHelper.logDiscrete(e);
                return false;
            }
        }
        return true;
    }

    public static boolean waitForElementTextToBeNot(WebDriver driver, By elementLocator, String textShouldNotBe) {
        try {
            (new WebDriverWait(driver, Duration.ofMillis(DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT)))
                    .until(ExpectedConditions.not(ExpectedConditions.textToBe(elementLocator, textShouldNotBe)));
        } catch (org.openqa.selenium.TimeoutException e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
        }
        return true;
    }

    public static WebElement getWebElementFromPointUsingJavascript(WebDriver driver, List<Integer> point, boolean scrollToElement) {
        if (DriverFactoryHelper.isWebExecution()) {
            if (Boolean.TRUE.equals(scrollToElement)) {
                return (WebElement) ((JavascriptExecutor) driver)
                        .executeScript(JavaScriptHelper.ELEMENT_SCROLL_TO_VIEWPORT.getValue(), point.get(0), point.get(1));
            } else {
                return (WebElement) ((JavascriptExecutor) driver).executeScript(
                        "return document.elementFromPoint(arguments[0], arguments[1])", point.get(0), point.get(1));
            }
        } else {
            return null;
        }
    }

    public static void clickUsingJavascript(WebDriver driver, By elementLocator) {
        if (DriverFactoryHelper.isWebExecution()) {
            ((JavascriptExecutor) driver).executeScript("arguments[arguments.length - 1].click();", driver.findElement(elementLocator));
        }
    }

    public static void dragAndDropUsingJavascript(WebDriver driver, By sourceElementLocator, By destinationElementLocator) {
        if (DriverFactoryHelper.isWebExecution()) {
            JavascriptExecutor js = (JavascriptExecutor) driver;
            String jQueryLoader = JavaScriptHelper.LOAD_JQUERY.getValue();
            js.executeAsyncScript(jQueryLoader /* , http://localhost:8080/jquery-1.7.2.js */);
            String dragAndDropHelper = JavaScriptHelper.ELEMENT_DRAG_AND_DROP.getValue();
            dragAndDropHelper = dragAndDropHelper + "$(arguments[0]).simulateDragDrop({dropTarget:arguments[1]});";
            ((JavascriptExecutor) driver).executeScript(dragAndDropHelper, driver.findElement(sourceElementLocator), driver.findElement(destinationElementLocator));
        }
    }

    public static void dragAndDropUsingActions(WebDriver driver, By sourceElementLocator, By destinationElementLocator) {
        new Actions(driver)
                .dragAndDrop(((WebElement) ElementActionsHelper.identifyUniqueElement(driver, sourceElementLocator).get(1))
                        , ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, destinationElementLocator).get(1)))
                .build().perform();
    }

    public static void executeNativeMobileCommandUsingJavascript(WebDriver driver, String command, Map<String, String> parameters) {
        ((JavascriptExecutor) driver).executeScript(command, parameters);
    }

    public static void submitFormUsingJavascript(WebDriver driver, By elementLocator) {
        if (DriverFactoryHelper.isWebExecution()) {
            ((JavascriptExecutor) driver).executeScript("arguments[0].submit();",
                    driver.findElement(elementLocator));
        }
    }

    public static void changeWebElementVisibilityUsingJavascript(WebDriver driver, By elementLocator, boolean desiredIsVisibleState) {
        if (DriverFactoryHelper.isWebExecution()) {

            if (Boolean.TRUE.equals(desiredIsVisibleState)) {
                ((JavascriptExecutor) driver).executeScript("arguments[0].setAttribute('style', 'display:block !important;');", driver.findElement(elementLocator));
            } else {
                ((JavascriptExecutor) driver).executeScript("arguments[0].setAttribute('style', 'display:none');", driver.findElement(elementLocator));
            }
        }
    }

    public static boolean setValueUsingJavascript(WebDriver driver, By elementLocator, String value) {
        try {
            if (DriverFactoryHelper.isWebExecution()) {
                ((JavascriptExecutor) driver).executeScript("arguments[0].value='" + value + "';", driver.findElement(elementLocator));
            }
            return true;
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
        }
    }

    public static String suggestNewXpathUsingJavascript(WebDriver driver, WebElement targetElement, By deprecatedElementLocator) {
        if (DriverFactoryHelper.isWebExecution()) {
            // attempt to find an optimal xpath for the targetElement
            var maximumXpathNodes = 6;
            var newXpath = "";
            for (var i = 0; i < maximumXpathNodes; i++) {
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

                xpathFindingAlgorithm = xpathFindingAlgorithm.replaceAll("\\$\\$MaxCount\\$\\$", maxCount)
                        .replaceAll("\\$\\$GetId\\$\\$", getId).replaceAll("\\$\\$GetIndex\\$\\$", getIndex)
                        .replaceAll("\\$\\$GetName\\$\\$", getName).replaceAll("\\$\\$GetType\\$\\$", getType)
                        .replaceAll("\\$\\$GetClass\\$\\$", getClass).replaceAll("\\$\\$GetText\\$\\$", getText);

                try {
                    newXpath = (String) ((JavascriptExecutor) driver).executeScript(xpathFindingAlgorithm, targetElement);
                    if (newXpath != null && driver.findElements(By.xpath(newXpath)).size() == 1) {
                        // if unique element was found, break, else keep iterating
                        break;
                    }
                } catch (JavascriptException e) {
                    ReportManagerHelper.logDiscrete(e);
                    ReportManager.logDiscrete("Failed to suggest a new XPath for the target element with this deprecated locator \""
                            + deprecatedElementLocator + "\"");
                }
            }
            if (newXpath != null) {
                boolean initialLoggingState = ReportManagerHelper.getDiscreteLogging();
                ReportManagerHelper.setDiscreteLogging(false);
                ReportManager.log("New AI-Suggested XPath \"" + newXpath.replace("\"", "'") + "\"");
                ReportManagerHelper.setDiscreteLogging(initialLoggingState);
                return newXpath;
            } else {
                ReportManager.log("Failed to suggest a new XPath for the target element with this deprecated locator \""
                        + deprecatedElementLocator + "\"");
                return null;
            }
        } else {
            return null;
        }
    }

    public static List<Object> takeScreenshot(WebDriver driver, By elementLocator, String actionName, String testData,
                                              boolean passFailStatus) {
        if (passFailStatus) {
            try {
                if (elementLocator != null) {
                    return ScreenshotManager.captureScreenShot(driver, elementLocator, actionName, true);
                } else if (testData != null) {
                    return ScreenshotManager.captureScreenShot(driver, actionName, true);
                }
                // else only happens when switching to default content so there is no need to
                // take a screenshot
            } catch (Exception e) {
                ReportManagerHelper.logDiscrete(e);
                ReportManager.logDiscrete(
                        "Failed to take a screenshot of the element as it doesn't exist anymore. Taking a screenshot of the whole page.");
                return ScreenshotManager.captureScreenShot(driver, actionName, true);
            }
        } else {
            return ScreenshotManager.captureScreenShot(driver, actionName, false);
        }
        return new ArrayList<>();
    }

    public static String getElementName(WebDriver driver, By elementLocator) {
        if (Boolean.TRUE.equals(Boolean.parseBoolean(System.getProperty("captureElementName")))) {
            try {
                var accessibleName = ((WebElement) identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1)).getAccessibleName();
                if (accessibleName != null && !accessibleName.isBlank()) {
                    return accessibleName;
                }
            } catch (WebDriverException e) {
                //happens on some elements that show unhandled inspector error
                //this exception is thrown on some older selenium grid instances, I saw it with firefox running over selenoid
            }
        }
        return formatLocatorToString(elementLocator);
    }

    private static void clearBeforeTyping(WebDriver driver, By elementLocator,
                                          TextDetectionStrategy successfulTextLocationStrategy) {
        try {
            // attempt clear using clear
            ((WebElement) identifyUniqueElement(driver, elementLocator).get(1)).clear();
            // attempt clear using letter by letter backspace
            var attemptClearBeforeTypingUsingBackspace = Boolean.parseBoolean(System.getProperty("attemptClearBeforeTypingUsingBackspace"));
            if (attemptClearBeforeTypingUsingBackspace) {
                String elementText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
                        successfulTextLocationStrategy);

                for (var character : elementText.toCharArray()) {
                    ((WebElement) identifyUniqueElement(driver, elementLocator).get(1)).sendKeys(Keys.BACK_SPACE);
                }
            }
        } catch (InvalidElementStateException e) {
            // this was seen in case of attempting to type in an invalid element (an image)
            ReportManagerHelper.logDiscrete(e);
        }
    }

    private static String confirmTypingWasSuccessful(WebDriver driver, By elementLocator,
                                                     TextDetectionStrategy successfulTextLocationStrategy) {
        TextDetectionStrategy updatedSuccessfulTextLocationStrategy = successfulTextLocationStrategy;
        if (updatedSuccessfulTextLocationStrategy.equals(TextDetectionStrategy.UNDEFINED)) {
            updatedSuccessfulTextLocationStrategy = determineSuccessfulTextLocationStrategy(driver,
                    elementLocator);
        }
        return readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
                updatedSuccessfulTextLocationStrategy);
    }

    private static TextDetectionStrategy determineSuccessfulTextLocationStrategy(WebDriver driver, By elementLocator) {
        if (DriverFactoryHelper.isMobileNativeExecution()) {
            return TextDetectionStrategy.TEXT;
        }
        String text = ((WebElement) identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1)).getText();
        // fixing https://github.com/ShaftHQ/SHAFT_ENGINE/issues/533
        String content = "";
        try {
            content = ((WebElement) identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1)).getAttribute(TextDetectionStrategy.CONTENT.getValue());
        } catch (Exception exception) {
            // ignore exception
        }
        String value = "";
        try {
            value = ((WebElement) identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1)).getAttribute(TextDetectionStrategy.VALUE.getValue());
        } catch (Exception exception) {
            // ignore exception
        }
        TextDetectionStrategy successfulTextLocationStrategy;
        if (text != null && !"".equals(text.trim())) {
            successfulTextLocationStrategy = TextDetectionStrategy.TEXT;
        } else if (content != null && !"".equals(content.trim())) {
            successfulTextLocationStrategy = TextDetectionStrategy.CONTENT;
        } else if (value != null && !"".equals(value.trim())) {
            successfulTextLocationStrategy = TextDetectionStrategy.VALUE;
        } else {
            successfulTextLocationStrategy = TextDetectionStrategy.UNDEFINED;
        }
        return successfulTextLocationStrategy;
    }

    private static String readTextBasedOnSuccessfulLocationStrategy(WebDriver driver, By elementLocator,
                                                                    TextDetectionStrategy successfulTextLocationStrategy) {
        String temp;
        switch (successfulTextLocationStrategy) {
            case TEXT -> {
                temp = ((WebElement) identifyUniqueElement(driver, elementLocator).get(1)).getText();
                return (temp == null) ? "" : temp;
            }
            case CONTENT -> {
                temp = ((WebElement) identifyUniqueElement(driver, elementLocator).get(1)).getAttribute(TextDetectionStrategy.CONTENT.getValue());
                return (temp == null) ? "" : temp;
            }
            case VALUE -> {
                temp = ((WebElement) identifyUniqueElement(driver, elementLocator).get(1)).getAttribute(TextDetectionStrategy.VALUE.getValue());
                return (temp == null) ? "" : temp;
            }
        }
        return "";
    }

    public static boolean performClipboardActions(WebDriver driver, By elementLocator, String action, Keys CommandOrControl) {
        try {
            switch (action.toLowerCase()) {
                case "copy" -> (new Actions(driver)).sendKeys(Keys.chord(CommandOrControl, "c")).perform();
                case "paste" -> (new Actions(driver)).sendKeys(Keys.chord(CommandOrControl, "v")).perform();
                case "cut" -> (new Actions(driver)).sendKeys(Keys.chord(CommandOrControl, "x")).perform();
                case "select all" -> (new Actions(driver)).sendKeys(Keys.chord(CommandOrControl, "a")).perform();
                case "unselect" -> (new Actions(driver)).sendKeys(Keys.ESCAPE).perform();
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

    private static void performType(WebDriver driver, By elementLocator, String text) {
        ArrayList<Class<? extends Exception>> expectedExceptions = getExpectedExceptions(true);
        try {
            new FluentWait<>(driver)
                    .withTimeout(Duration.ofSeconds(5))
                    .pollingEvery(Duration.ofSeconds(1))
                    .ignoreAll(expectedExceptions)
                    .until(nestedDriver -> {
                        nestedDriver.findElement(elementLocator).sendKeys(text);
                        return true;
                    });
        } catch (TimeoutException e) {
            // In case typing failed and the timeout expired
            ReportManagerHelper.logDiscrete(e);
        }
    }

    public static String typeWrapper(WebDriver driver, By elementLocator, String targetText) {
        try {
            TextDetectionStrategy successfulTextLocationStrategy = TextDetectionStrategy.UNDEFINED;
            if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("forceCheckTextWasTypedCorrectly")))) {
                successfulTextLocationStrategy = determineSuccessfulTextLocationStrategy(driver,
                        elementLocator);
            }
            clearBeforeTyping(driver, elementLocator, successfulTextLocationStrategy);
            if (!"".equals(targetText)) {
                performType(driver, elementLocator, targetText);
            }
            if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("forceCheckTextWasTypedCorrectly")))) {
                String actualText = confirmTypingWasSuccessful(driver, elementLocator, successfulTextLocationStrategy);
                if (targetText.equals(actualText) || OBFUSCATED_STRING.repeat(targetText.length()).equals(actualText)) {
                    return targetText;
                } else {
                    // attempt once to type using javascript then confirm typing was successful
                    // again
                    ElementActionsHelper.setValueUsingJavascript(driver, elementLocator, targetText);
                    var textAfterSettingValueUsingJavascript = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator, TextDetectionStrategy.VALUE);
                    if ("".equals(textAfterSettingValueUsingJavascript) && successfulTextLocationStrategy.equals(TextDetectionStrategy.UNDEFINED)) {
                        return targetText;
                    }
                    return textAfterSettingValueUsingJavascript;
                }
            } else {
                return targetText;
            }
        } catch (Exception throwable) {
            ReportManager.log("Failed to identify Target element with locator \"" + elementLocator + "\".");
            throw throwable;
//            return null;
        }
    }

    private static boolean isFoundInStacktrace(Class<?> classObject, Throwable throwable) {
        var targetClassName = classObject.getName();
        for (StackTraceElement element : throwable.getStackTrace()) {
            if (element.getClassName().equals(targetClassName)) {
                return true;
            }
        }
        return false;
    }

    public static List<Object> identifyUniqueElement(WebDriver driver, By elementLocator) {
        return identifyUniqueElement(driver, elementLocator, true);
    }

    public static List<Object> identifyUniqueElementIgnoringVisibility(WebDriver driver, By elementLocator) {
        return identifyUniqueElement(driver, elementLocator, false);
    }

    private static List<Object> identifyUniqueElement(WebDriver driver, By elementLocator,
                                                      boolean checkForVisibility) {
        var matchingElementsInformation = getMatchingElementsInformation(driver, elementLocator, Optional.empty(), Optional.of(checkForVisibility));

        if (elementLocator != null) {
            if (!(elementLocator instanceof RelativeLocator.RelativeBy)) {
                // in case of regular locator
                switch (Integer.parseInt(matchingElementsInformation.get(0).toString())) {
                    case 0 ->
                            failAction(driver, "zero elements found matching this locator", null, (Throwable) matchingElementsInformation.get(2));
                    case 1 -> {
                        return matchingElementsInformation;
                    }
                    default -> {
                        if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("forceCheckElementLocatorIsUnique")))) {
                            failAction(driver, "multiple elements found matching this locator",
                                    elementLocator);
                        }
                        return matchingElementsInformation;
                    }
                }
            }
            //in case of relativeLocator
            return matchingElementsInformation;
        } else {
            // in case locator is null
            failAction(driver, "element locator is NULL.", null);
        }
        //unreachable code
        return matchingElementsInformation;
    }

    public static List<Object> getMatchingElementsInformation(WebDriver driver, By elementLocator, Optional<Integer> numberOfAttempts, Optional<Boolean> checkForVisibility) {
        if (elementLocator == null) {
            var elementInformation = new ArrayList<>();
            elementInformation.add(0);
            elementInformation.add(null);
            return elementInformation;
        }
//        JavaScriptWaitManager.waitForLazyLoading(driver);
        if (!elementLocator.equals(By.tagName("html"))) {
            if (numberOfAttempts.isEmpty() && checkForVisibility.isEmpty()) {
                return ElementActionsHelper.waitForElementPresence(driver, elementLocator);
            } else if (numberOfAttempts.isPresent() && checkForVisibility.isEmpty()) {
                return ElementActionsHelper.waitForElementPresence(driver, elementLocator, numberOfAttempts.get());
            } else if (numberOfAttempts.isEmpty()) {
                return ElementActionsHelper.waitForElementPresence(driver, elementLocator, checkForVisibility.get());
            } else {
                return ElementActionsHelper.waitForElementPresence(driver, elementLocator, numberOfAttempts.get(), checkForVisibility.get());
            }
        } else {
            //if locator is just tagname html
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
     *                       selector, name ...etc)
     * @return integer value that represents the number of elements that match the
     * desired elementLocator
     */
    public static int getElementsCount(WebDriver driver, By elementLocator) {
        return Integer.parseInt(ElementActionsHelper.getMatchingElementsInformation(driver, elementLocator, Optional.empty(), Optional.empty()).get(0).toString());
    }

    /**
     * Returns the number of elements that match a certain elementLocator
     *
     * @param driver           the current instance of Selenium WebDriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param numberOfAttempts the number of retries before returning a count
     *                         [returns zero if no elements were found after all the
     *                         retries]
     * @return integer value that represents the number of elements that match the
     * desired elementLocator
     */
    public static int getElementsCount(WebDriver driver, By elementLocator, int numberOfAttempts) {
        return Integer.parseInt(ElementActionsHelper.getMatchingElementsInformation(driver, elementLocator, Optional.of(numberOfAttempts), Optional.empty()).get(0).toString());
    }

    public static void passAction(WebDriver driver, By elementLocator, String testData, List<Object> screenshot, String elementName) {
        //TODO: open calling methods, and test if Appium can also fetch the element name instead of passing null
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        List<List<Object>> attachments = new LinkedList<>();
        attachments.add(screenshot);
        passAction(driver, elementLocator, actionName, testData, attachments, elementName);
    }

    public static void passAction(Screen screen, App applicationWindow, Pattern element, String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        List<List<Object>> attachments = new LinkedList<>();
        attachments.add(SikuliActions.prepareElementScreenshotAttachment(screen, applicationWindow, element, actionName, true));
        passAction(null, null, actionName, testData, attachments, null);
    }

    public static void passAction(WebDriver driver, By elementLocator, String actionName, String testData,
                                  List<List<Object>> screenshots, String elementName) {
        reportActionResult(driver, actionName, testData, elementLocator, screenshots, elementName, true);
    }

    public static void failAction(WebDriver driver, By elementLocator, Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, null, elementLocator, null, rootCauseException);
    }

    public static void failAction(WebDriver driver, String testData, By elementLocator, Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, testData, elementLocator, null, rootCauseException);
    }

    public static void failAction(WebDriver driver, String testData, By elementLocator, List<List<Object>> attachments, Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, testData, elementLocator, attachments, rootCauseException);
    }

    public static void failAction(Screen screen, App applicationWindow, Pattern element, String testData, Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        List<List<Object>> attachments = new LinkedList<>();
        attachments.add(SikuliActions.prepareElementScreenshotAttachment(screen, applicationWindow, element, actionName, false));
        failAction(null, actionName, testData, null, attachments, rootCauseException);
    }

    public static void failAction(WebDriver driver, String actionName, String testData, By elementLocator, List<List<Object>> screenshots,
                                  Throwable... rootCauseException) {
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
        boolean skipPageScreenshot = rootCauseException.length >= 1 && (
                isFoundInStacktrace(ValidationsHelper.class, rootCauseException[0])
                        && isFoundInStacktrace(ElementActionsHelper.class, rootCauseException[0]));

        String elementName = "";
        if (elementLocator != null) {
            elementName = formatLocatorToString(elementLocator);
            try {
                var accessibleName = driver.findElement(elementLocator).getAccessibleName();
                if (accessibleName != null && !accessibleName.isBlank()) {
                    elementName = accessibleName;
                }
            } catch (Exception throwable) {
                //do nothing
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
                message = reportActionResult(driver, actionName, testData, elementLocator, screenshots, elementName, false);
            }
        }

        if (rootCauseException.length >= 1) {
            Assert.fail(message, rootCauseException[0]);
        } else {
            Assert.fail(message);
        }
    }

    private static String createReportMessage(String actionName, String testData, String elementName, Boolean passFailStatus) {
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
            message = message + preposition + "\"" + elementName.trim() + "\"";
        }

        message = message + ".";
        return message;
    }

    private static List<List<Object>> createReportAttachments(WebDriver driver, String actionName, String testData, By elementLocator,
                                                              List<List<Object>> screenshots, Boolean passFailStatus, Throwable... rootCauseException) {
        actionName = JavaHelper.convertToSentenceCase(actionName);
        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("Element Action Test Data - " + actionName,
                    "Actual Value", testData);
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
            if (newScreenshot != null && !newScreenshot.equals(new ArrayList<>())) {
                attachments.add(newScreenshot);
            }
        }

        if (rootCauseException != null && rootCauseException.length >= 1) {
            List<Object> actualValueAttachment = Arrays.asList("Element Action Exception - " + actionName,
                    "Stacktrace", ReportManagerHelper.formatStackTraceToLogEntry(rootCauseException[0]));
            attachments.add(actualValueAttachment);
        }

        if (attachments.size() == 1 && attachments.get(0).isEmpty()) {
            return null;
        } else {
            return attachments;
        }
    }

    private static String reportActionResult(WebDriver driver, String actionName, String testData, By elementLocator,
                                             List<List<Object>> screenshots, String elementName, Boolean passFailStatus, Throwable... rootCauseException) {
        String message = createReportMessage(actionName, testData, elementName, passFailStatus);
        List<List<Object>> attachments = createReportAttachments(driver, actionName, testData, elementLocator, screenshots, passFailStatus, rootCauseException);

        if (attachments != null && !attachments.equals(new ArrayList<>())) {
            ReportManagerHelper.log(message, attachments);
        } else {
            ReportManager.log(message);
        }
        return message;
    }

    public enum TextDetectionStrategy {
        TEXT("text"), CONTENT("textContent"), VALUE("value"), UNDEFINED("undefined");
        private final String value;

        TextDetectionStrategy(String strategy) {
            this.value = strategy;
        }

        public String getValue() {
            return value;
        }
    }

    public static String formatLocatorToString(By locator) {
        if (locator instanceof RelativeLocator.RelativeBy relativeLocator) {
            return "Relative Locator: " + relativeLocator.getRemoteParameters().value().toString();
        } else {
            return locator.toString();
        }
    }
}