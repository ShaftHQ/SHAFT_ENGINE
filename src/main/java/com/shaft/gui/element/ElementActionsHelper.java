package com.shaft.gui.element;

import com.shaft.driver.DriverFactoryHelper;
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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

class ElementActionsHelper {
    private static final int DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER = Integer
            .parseInt(System.getProperty("defaultElementIdentificationTimeout").trim());
    private static final int ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION = Integer
            .parseInt(System.getProperty("attemptsBeforeThrowingElementNotFoundException").trim());
    private static final int ELEMENT_IDENTIFICATION_POLLING_DELAY = 1; // seconds
    private static final boolean FORCE_CHECK_FOR_ELEMENT_VISIBILITY = Boolean
            .parseBoolean(System.getProperty("forceCheckForElementVisibility").trim());

    private ElementActionsHelper() {
        throw new IllegalStateException("Utility class");
    }

    protected static int waitForElementPresence(WebDriver driver, By elementLocator) {
        return waitForElementPresence(driver, elementLocator, ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION, FORCE_CHECK_FOR_ELEMENT_VISIBILITY);
    }

    protected static int waitForElementPresence(WebDriver driver, By elementLocator, int numberOfAttempts) {
        return waitForElementPresence(driver, elementLocator, numberOfAttempts, FORCE_CHECK_FOR_ELEMENT_VISIBILITY);
    }

    protected static int waitForElementPresence(WebDriver driver, By elementLocator, boolean checkForVisibility) {
        return waitForElementPresence(driver, elementLocator, ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION, checkForVisibility);
    }

    protected static int waitForElementPresence(WebDriver driver, By elementLocator, int numberOfAttempts, boolean checkForVisibility) {
        boolean validToCheckForVisibility = checkForVisibility && !elementLocator.toString().contains("input[@type='file']")
                && !elementLocator.equals(By.tagName("html"));

        ArrayList<Class<? extends Exception>> expectedExceptions = new ArrayList<>();
        expectedExceptions.add(org.openqa.selenium.NoSuchElementException.class);
        expectedExceptions.add(org.openqa.selenium.StaleElementReferenceException.class);
        expectedExceptions.add(org.openqa.selenium.ElementNotInteractableException.class);
        if (validToCheckForVisibility){
            expectedExceptions.add(org.openqa.selenium.ElementNotVisibleException.class);
        }
        expectedExceptions.add(org.openqa.selenium.WebDriverException.class);

        try {
            return new FluentWait<>(driver)
                    .withTimeout(Duration.ofSeconds(
                            (long) DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER * numberOfAttempts))
                    .pollingEvery(Duration.ofSeconds(ELEMENT_IDENTIFICATION_POLLING_DELAY))
                    .ignoreAll(expectedExceptions)
                    .until(nestedDriver -> {
                        if (validToCheckForVisibility){
                            if (!(driver instanceof AppiumDriver)){
                                ((Locatable) driver.findElement(elementLocator)).getCoordinates().inViewPort();
                            }else {
                                nestedDriver.findElement(elementLocator).isDisplayed();
                            }
                        }else {
                            nestedDriver.findElement(elementLocator);
                        }
                        return nestedDriver.findElements(elementLocator).size();
                    });
        } catch (org.openqa.selenium.TimeoutException e) {
            // In case the element was not found / not visible and the timeout expired
            ReportManagerHelper.logDiscrete(e);
            return 0;
        }
    }

    @Deprecated(forRemoval = true)
    protected static boolean waitForElementToBeVisible(WebDriver driver, By elementLocator) {
        if (FORCE_CHECK_FOR_ELEMENT_VISIBILITY && !DriverFactoryHelper.isMobileNativeExecution()) {
            ArrayList<Class<? extends Exception>> expectedExceptions = new ArrayList<>();
            expectedExceptions.add(org.openqa.selenium.NoSuchElementException.class);
            expectedExceptions.add(org.openqa.selenium.StaleElementReferenceException.class);
            expectedExceptions.add(org.openqa.selenium.ElementNotVisibleException.class);
            expectedExceptions.add(org.openqa.selenium.WebDriverException.class);
            // UnsupportedCommandException getElementLocationOnceScrolledIntoView
            // TODO: appium -> swipe element into view

            try {
                new FluentWait<>(driver)
                        .withTimeout(Duration.ofSeconds(
                                (long) DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER * ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION))
                        .pollingEvery(Duration.ofSeconds(ELEMENT_IDENTIFICATION_POLLING_DELAY))
                        .ignoreAll(expectedExceptions)
                        .until(nestedDriver -> {
                            ((Locatable) driver.findElement(elementLocator)).getCoordinates().inViewPort();
                            return true;
                        });
            } catch (org.openqa.selenium.TimeoutException e) {
                // In case the element was not visible and the timeout expired
                ReportManagerHelper.logDiscrete(e);
            }
            if (Boolean.FALSE.equals(driver.findElement(elementLocator).isDisplayed())) {
                try {
                    new WebDriverWait(driver, Duration.ofSeconds((long) DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER * ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION)).until(ExpectedConditions.visibilityOfElementLocated(elementLocator));
                } catch (org.openqa.selenium.TimeoutException e) {
                    ReportManagerHelper.logDiscrete(e);
                    return false;
                }
            }
        }
        return true;
    }

    protected static boolean waitForElementToBeClickable(WebDriver driver, By elementLocator) {
        if (!DriverFactoryHelper.isMobileNativeExecution()) {
            try {
                (new WebDriverWait(driver, Duration.ofSeconds(DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER)))
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
            (new WebDriverWait(driver, Duration.ofSeconds(DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER)))
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
    	}else {
    	return (WebElement) ((JavascriptExecutor) driver).executeScript(
                "return document.elementFromPoint(arguments[0], arguments[1])", point.get(0), point.get(1));
    	}
    	}else {
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
    	}else {
    		((JavascriptExecutor) driver).executeScript("arguments[0].setAttribute('style', 'display:none');",driver.findElement(elementLocator));
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

    @Deprecated(forRemoval = true)
    protected static void performHoverUsingJavascript(WebDriver driver, By elementLocator) {
    	if (DriverFactoryHelper.isWebExecution()) {
    		var createMouseEvent = "var evObj = document.createEvent('MouseEvents');";
    		var dispatchMouseEvent = "arguments[arguments.length -1].dispatchEvent(evObj);";

    		var mouseEventFirstHalf = "evObj.initMouseEvent(\"";
    		var mouseEventSecondHalf = "\", true, false, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);";

            String javaScript = createMouseEvent + mouseEventFirstHalf + "mousemove" + mouseEventSecondHalf
                    + dispatchMouseEvent;
            ((JavascriptExecutor) driver).executeScript(javaScript, driver.findElement(elementLocator));

            javaScript = createMouseEvent + mouseEventFirstHalf + "mouseenter" + mouseEventSecondHalf + dispatchMouseEvent;
            ((JavascriptExecutor) driver).executeScript(javaScript, driver.findElement(elementLocator));

            javaScript = createMouseEvent + mouseEventFirstHalf + "mouseover" + mouseEventSecondHalf + dispatchMouseEvent;
            ((JavascriptExecutor) driver).executeScript(javaScript, driver.findElement(elementLocator));

//            (new Actions(driver)).moveToElement(driver.findElement(elementLocator)).perform();
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
                ReportManager.log("Failed to suggest a new XPath for the target element with this deprecated locator ["
                        + deprecatedElementLocator + "]");
            }
        }
        if (newXpath != null) {
            boolean initialLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(false);
            ReportManager.log("New AI-Suggested XPath [" + newXpath.replace("\"", "'") + "]");
            ReportManagerHelper.setDiscreteLogging(initialLoggingState);
            return newXpath;
        } else {
            ReportManager.log("Failed to suggest a new XPath for the target element with this deprecated locator ["
                    + deprecatedElementLocator + "]");
            return null;
        }
    	} else {
    		return null;
    	}
    }
}
