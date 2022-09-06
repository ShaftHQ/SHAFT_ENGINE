package com.shaft.gui.element;

import com.shaft.cli.FileActions;
import com.shaft.driver.DriverFactoryHelper;
import com.shaft.gui.image.ImageProcessingActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import com.shaft.tools.support.JavaScriptHelper;
import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.Locatable;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.FluentWait;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.time.Duration;
import java.util.*;

class ElementActionsHelper {
    private static long DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT = Integer
            .parseInt(System.getProperty("defaultElementIdentificationTimeout").trim())* 1000L; //milliseconds
    private static final int ELEMENT_IDENTIFICATION_POLLING_DELAY = 100; // milliseconds
    private static final boolean FORCE_CHECK_FOR_ELEMENT_VISIBILITY = Boolean
            .parseBoolean(System.getProperty("forceCheckForElementVisibility").trim());

    private ElementActionsHelper() {
        throw new IllegalStateException("Utility class");
    }

    protected static int waitForElementPresence_reducedTimeout(WebDriver driver, By elementLocator){
        DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT = 300; //this is used for faster mobile native scrolling. default for ios is 200 and for android is 250, this covers both
        var numberOfFoundElements = waitForElementPresence(driver, elementLocator);
        DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT = Integer
                .parseInt(System.getProperty("defaultElementIdentificationTimeout").trim())* 1000L;
        return Integer.parseInt(numberOfFoundElements.get(0).toString());
    }

    protected static List<Object> waitForElementPresence(WebDriver driver, By elementLocator) {
        return waitForElementPresence(driver, elementLocator, 1, FORCE_CHECK_FOR_ELEMENT_VISIBILITY);
    }

    protected static List<Object> waitForElementPresence(WebDriver driver, By elementLocator, int numberOfAttempts) {
        return waitForElementPresence(driver, elementLocator, numberOfAttempts, FORCE_CHECK_FOR_ELEMENT_VISIBILITY);
    }

    protected static List<Object> waitForElementPresence(WebDriver driver, By elementLocator, boolean checkForVisibility) {
        return waitForElementPresence(driver, elementLocator, 1, checkForVisibility);
    }

    protected static List<Object> waitForElementPresence(WebDriver driver, String elementReferenceScreenshot) {
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
                    ReportManagerHelper.log(e);
                }
                currentScreenImage = ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
                coordinates = ImageProcessingActions.findImageWithinCurrentPage(elementReferenceScreenshot, currentScreenImage);
                if (!Collections.emptyList().equals(coordinates)) {
                    isFound = true;
                }
                elapsedTime = System.currentTimeMillis() - startTime;
            } while (!isFound && elapsedTime < DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT);
            returnedValue.add(currentScreenImage);
            returnedValue.add(FileActions.getInstance().readFromImageFile(elementReferenceScreenshot));
            returnedValue.add(coordinates);
        }else{
            // reference screenshot doesn't exist
            currentScreenImage = ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
            returnedValue.add(currentScreenImage);
            returnedValue.add(new byte[0]);
            returnedValue.add(Collections.emptyList());
        }
        return returnedValue;
    }

    protected static List<Object> waitForElementPresence(WebDriver driver, By elementLocator, int numberOfAttempts, boolean checkForVisibility) {
        boolean validToCheckForVisibility = checkForVisibility && !elementLocator.toString().contains("input[@type='file']")
                && !elementLocator.equals(By.tagName("html"));

        ArrayList<Class<? extends Exception>> expectedExceptions = new ArrayList<>();
        expectedExceptions.add(org.openqa.selenium.NoSuchElementException.class);
        expectedExceptions.add(org.openqa.selenium.StaleElementReferenceException.class);
        expectedExceptions.add(org.openqa.selenium.ElementNotInteractableException.class);
        if (validToCheckForVisibility) {
            expectedExceptions.add(org.openqa.selenium.InvalidElementStateException.class);
        }
        // the generic exception is added to handle a case with WebKit whereby the browser doesn't state the cause of the issue
        expectedExceptions.add(org.openqa.selenium.WebDriverException.class);

        try {
            return new FluentWait<>(driver)
                    .withTimeout(Duration.ofMillis(
                            DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT * numberOfAttempts))
                    .pollingEvery(Duration.ofMillis(ELEMENT_IDENTIFICATION_POLLING_DELAY))
                    .ignoreAll(expectedExceptions)
                    .until(nestedDriver -> {
                        WebElement targetElement = null;
                        if (validToCheckForVisibility) {
                            if (!(driver instanceof AppiumDriver)) {
                                ((Locatable) driver.findElement(elementLocator)).getCoordinates().inViewPort();
                            } else {
                                nestedDriver.findElement(elementLocator).isDisplayed();
                            }
                        } else {
                            targetElement = nestedDriver.findElement(elementLocator);
                        }
                        var elementInformation = new ArrayList<>();
                        elementInformation.add(nestedDriver.findElements(elementLocator).size());
                        elementInformation.add(targetElement);
                        return elementInformation;
//                        return nestedDriver.findElements(elementLocator).size();
                    });
        } catch (org.openqa.selenium.TimeoutException e) {
            // In case the element was not found / not visible and the timeout expired
//            ReportManagerHelper.logDiscrete(e);
            ReportManager.logDiscrete(e.getMessage() + " || " +e.getCause().getMessage().substring(0,e.getCause().getMessage().indexOf("\n")));
            var elementInformation = new ArrayList<>();
            elementInformation.add(0);
            elementInformation.add(null);
            return elementInformation;
        }
    }

    protected static boolean waitForElementToBeClickable(WebDriver driver, By elementLocator) {
        if (!DriverFactoryHelper.isMobileNativeExecution()) {
            try {
                (new WebDriverWait(driver, Duration.ofMillis(DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT)))
                        .until(ExpectedConditions.elementToBeClickable(elementLocator));
            } catch (org.openqa.selenium.TimeoutException e) {
                ReportManagerHelper.logDiscrete(e);
                return false;
            }
        }
        return true;
    }

    protected static boolean waitForElementTextToBeNot(WebDriver driver, By elementLocator, String textShouldNotBe) {
        try {
            (new WebDriverWait(driver, Duration.ofMillis(DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT)))
                    .until(ExpectedConditions.not(ExpectedConditions.textToBe(elementLocator, textShouldNotBe)));
        } catch (org.openqa.selenium.TimeoutException e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
        }
        return true;
    }

    protected static WebElement getWebElementFromPointUsingJavascript(WebDriver driver, List<Integer> point, boolean scrollToElement) {
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

    protected static void clickUsingJavascript(WebDriver driver, By elementLocator) {
        if (DriverFactoryHelper.isWebExecution()) {
            ((JavascriptExecutor) driver).executeScript("arguments[arguments.length - 1].click();", driver.findElement(elementLocator));
        }
    }

    protected static void dragAndDropUsingJavascript(WebDriver driver, By sourceElementLocator, By destinationElementLocator) {
        if (DriverFactoryHelper.isWebExecution()) {
            JavascriptExecutor js = (JavascriptExecutor) driver;
            String jQueryLoader = JavaScriptHelper.LOAD_JQUERY.getValue();
            js.executeAsyncScript(jQueryLoader /* , http://localhost:8080/jquery-1.7.2.js */);
            String dragAndDropHelper = JavaScriptHelper.ELEMENT_DRAG_AND_DROP.getValue();
            dragAndDropHelper = dragAndDropHelper + "$(arguments[0]).simulateDragDrop({dropTarget:arguments[1]});";
            ((JavascriptExecutor) driver).executeScript(dragAndDropHelper, driver.findElement(sourceElementLocator), driver.findElement(destinationElementLocator));
        }
    }

    protected static void executeNativeMobileCommandUsingJavascript(WebDriver driver, String command, Map<String, String> parameters) {
        ((JavascriptExecutor) driver).executeScript(command, parameters);
    }

    protected static void submitFormUsingJavascript(WebDriver driver, By elementLocator) {
        if (DriverFactoryHelper.isWebExecution()) {
            ((JavascriptExecutor) driver).executeScript("arguments[0].submit();",
                    driver.findElement(elementLocator));
        }
    }

    protected static void changeWebElementVisibilityUsingJavascript(WebDriver driver, By elementLocator, boolean desiredIsVisibleState) {
        if (DriverFactoryHelper.isWebExecution()) {

            if (Boolean.TRUE.equals(desiredIsVisibleState)) {
                ((JavascriptExecutor) driver).executeScript("arguments[0].setAttribute('style', 'display:block !important;');", driver.findElement(elementLocator));
            } else {
                ((JavascriptExecutor) driver).executeScript("arguments[0].setAttribute('style', 'display:none');", driver.findElement(elementLocator));
            }
        }
    }

    protected static boolean setValueUsingJavascript(WebDriver driver, By elementLocator, String value) {
        try {
            if (DriverFactoryHelper.isWebExecution()) {
                ((JavascriptExecutor) driver).executeScript("arguments[0].value='" + value + "';", driver.findElement(elementLocator));
            }
            return true;
        } catch (Exception e) {
            ReportManagerHelper.log(e);
            return false;
        }
    }

    protected static String suggestNewXpathUsingJavascript(WebDriver driver, WebElement targetElement, By deprecatedElementLocator) {
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
                    ReportManagerHelper.log(e);
                    ReportManager.log("Failed to suggest a new XPath for the target element with this deprecated locator \""
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
}
