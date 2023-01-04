package io.github.shafthq.shaft.listeners;

import com.shaft.tools.io.ReportManager;
import io.github.shafthq.shaft.driver.DriverFactoryHelper;
import io.github.shafthq.shaft.gui.browser.WebDriverBrowserActions;
import io.github.shafthq.shaft.gui.element.WebDriverElementActions;
import io.github.shafthq.shaft.gui.image.ScreenshotManager;
import io.github.shafthq.shaft.tools.io.helpers.ReportManagerHelper;
import io.github.shafthq.shaft.tools.support.JavaHelper;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.Sequence;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.FluentWait;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.time.Duration;
import java.util.*;

public class WebDriverListener implements org.openqa.selenium.support.events.WebDriverListener, io.appium.java_client.proxy.MethodCallListener {
    private static final long DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT = Integer.parseInt(System.getProperty("defaultElementIdentificationTimeout").trim()) * 1000L;
    private static final int ELEMENT_IDENTIFICATION_POLLING_DELAY = 100; // milliseconds

    // Global

    public void beforeAnyCall(Object target, Method method, Object[] args) {
    }

    public void afterAnyCall(Object target, Method method, Object[] args, Object result) {
//        ReportManager.log(JavaHelper.convertToSentenceCase(method.getName() )+ " action performed.");
    }

    public void onError(Object target, Method method, Object[] args, InvocationTargetException e) {
        if (!isEngineCallToNativeMethod()) {
            ReportManager.log(JavaHelper.convertToSentenceCase(method.getName()) + " action failed.");
            ReportManagerHelper.attach(ScreenshotManager.captureScreenShot(DriverFactoryHelper.getDriver().get(), method.getName(), false));
            ReportManagerHelper.log(e);
        }
    }

    // WebDriver

    public void beforeAnyWebDriverCall(WebDriver driver, Method method, Object[] args) {
    }

    public void afterAnyWebDriverCall(WebDriver driver, Method method, Object[] args, Object result) {
    }

    public void beforeGet(WebDriver driver, String url) {
    }

    public void afterGet(WebDriver driver, String url) {
        if (!isEngineCallToNativeMethod()) {
            ReportManager.log("Navigate to: \"" + url + "\".");
        }
    }

    public void beforeGetCurrentUrl(WebDriver driver) {
    }

    public void afterGetCurrentUrl(String result, WebDriver driver) {
        if (!isEngineCallToNativeMethod()) {
            ReportManager.log("Current url is: \"" + result + "\".");
        }
    }

    public void beforeGetTitle(WebDriver driver) {
    }

    public void afterGetTitle(WebDriver driver, String result) {
        if (!isEngineCallToNativeMethod()) {
            ReportManager.log("Current Window Title is: \"" + result + "\".");
        }
    }

    public void beforeFindElement(WebDriver driver, By locator) {
        if (!isEngineCallToNativeMethod()) {
            ArrayList<Class<? extends Exception>> expectedExceptions = new ArrayList<>();
            expectedExceptions.add(org.openqa.selenium.NoSuchElementException.class);
            expectedExceptions.add(org.openqa.selenium.StaleElementReferenceException.class);
            expectedExceptions.add(org.openqa.selenium.ElementNotInteractableException.class);
            // the generic exception is added to handle a case with WebKit whereby the browser doesn't state the cause of the issue
            expectedExceptions.add(org.openqa.selenium.WebDriverException.class);

            try {
                new FluentWait<>(driver)
                        .withTimeout(Duration.ofMillis(DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT))
                        .pollingEvery(Duration.ofMillis(ELEMENT_IDENTIFICATION_POLLING_DELAY))
                        .ignoreAll(expectedExceptions)
                        .until(nestedDriver -> nestedDriver.findElement(locator));
            } catch (org.openqa.selenium.TimeoutException timeoutException) {
                // In case the element was not found / not visible and the timeout expired
                ReportManager.logDiscrete(timeoutException.getMessage() + " || " + timeoutException.getCause().getMessage().substring(0, timeoutException.getCause().getMessage().indexOf("\n")));
                throw timeoutException;
            }
        }
    }

    public void afterFindElement(WebDriver driver, By locator, WebElement result) {
    }

    public void beforeFindElements(WebDriver driver, By locator) {
    }

    public void afterFindElements(WebDriver driver, By locator, List<WebElement> result) {
    }

    public void beforeGetPageSource(WebDriver driver) {
    }

    public void afterGetPageSource(WebDriver driver, String result) {
    }

    public void beforeClose(WebDriver driver) {
    }

    public void afterClose(WebDriver driver) {
        if (!isEngineCallToNativeMethod()) {
            ReportManager.log("Successfully Closed Driver.");
        }
    }

    public void beforeQuit(WebDriver driver) {
    }

    public void afterQuit(WebDriver driver) {
        if (!isEngineCallToNativeMethod()) {
            ReportManager.log("Successfully Quit Driver.");
        }
    }

    public void beforeGetWindowHandles(WebDriver driver) {
    }

    public void afterGetWindowHandles(WebDriver driver, Set<String> result) {
    }

    public void beforeGetWindowHandle(WebDriver driver) {
    }

    public void afterGetWindowHandle(WebDriver driver, String result) {
    }

    public void beforeExecuteScript(WebDriver driver, String script, Object[] args) {
    }

    public void afterExecuteScript(WebDriver driver, String script, Object[] args, Object result) {
    }

    public void beforeExecuteAsyncScript(WebDriver driver, String script, Object[] args) {
    }

    public void afterExecuteAsyncScript(WebDriver driver, String script, Object[] args, Object result) {
    }

    public void beforePerform(WebDriver driver, Collection<Sequence> actions) {
    }

    public void afterPerform(WebDriver driver, Collection<Sequence> actions) {
    }

    public void beforeResetInputState(WebDriver driver) {
    }

    public void afterResetInputState(WebDriver driver) {
    }

    // WebElement

    public void beforeAnyWebElementCall(WebElement element, Method method, Object[] args) {
    }

    public void afterAnyWebElementCall(WebElement element, Method method, Object[] args, Object result) {
    }

    public void beforeClick(WebElement element) {
        if (!isEngineCallToNativeMethod()) {
            try {
                (new WebDriverWait(DriverFactoryHelper.getDriver().get(), Duration.ofMillis(DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT)))
                        .until(ExpectedConditions.elementToBeClickable(element));
            } catch (org.openqa.selenium.TimeoutException timeoutException) {
                ReportManagerHelper.logDiscrete(timeoutException);
                throw timeoutException;
            }
        }
    }

    public void afterClick(WebElement element) {
        if (!isEngineCallToNativeMethod()) {
            try {
                ReportManager.log("Click " + getElementName(element) + ".");
            } catch (Throwable throwable) {
                ReportManager.log("Click.");
            }
        }
    }

    public void beforeSubmit(WebElement element) {
    }

    public void afterSubmit(WebElement element) {
        if (!isEngineCallToNativeMethod()) {
            try {
                ReportManager.log("Submit " + getElementName(element) + ".");
            } catch (Throwable throwable) {
                ReportManager.log("Submit.");
            }
        }
    }

    public void beforeSendKeys(WebElement element, CharSequence... keysToSend) {
    }

    public void afterSendKeys(WebElement element, CharSequence... keysToSend) {
        if (!isEngineCallToNativeMethod()) {
            var stringBuilder = new StringBuilder();
            Arrays.stream(keysToSend).toList().forEach(stringBuilder::append);
            try {
                ReportManager.log("Type \"" + stringBuilder + "\" into " + getElementName(element) + ".");
            } catch (Throwable throwable) {
                ReportManager.log("Type \"" + stringBuilder + "\".");
            }
        }
    }

    public void beforeClear(WebElement element) {
    }

    public void afterClear(WebElement element) {
        if (!isEngineCallToNativeMethod()) {
            try {
                ReportManager.log("Clear " + getElementName(element) + ".");
            } catch (Throwable throwable) {
                ReportManager.log("Clear.");
            }
        }
    }

    public void beforeGetTagName(WebElement element) {
    }

    public void afterGetTagName(WebElement element, String result) {
    }

    public void beforeGetAttribute(WebElement element, String name) {
    }

    public void afterGetAttribute(WebElement element, String name, String result) {
    }

    public void beforeIsSelected(WebElement element) {
    }

    public void afterIsSelected(WebElement element, boolean result) {
    }

    public void beforeIsEnabled(WebElement element) {
    }

    public void afterIsEnabled(WebElement element, boolean result) {
    }

    public void beforeGetText(WebElement element) {
    }

    public void afterGetText(WebElement element, String result) {
        if (!isEngineCallToNativeMethod()) {
            try {
                ReportManager.log("Get Text from " + getElementName(element) + ", text is \"" + result + "\".");
            } catch (Throwable throwable) {
                ReportManager.log("Get Text, text is :\"" + result + "\".");
            }
        }
    }

    public void beforeFindElement(WebElement element, By locator) {
    }

    public void afterFindElement(WebElement element, By locator, WebElement result) {
    }

    public void beforeFindElements(WebElement element, By locator) {
    }

    public void afterFindElements(WebElement element, By locator, List<WebElement> result) {
    }

    public void beforeIsDisplayed(WebElement element) {
    }

    public void afterIsDisplayed(WebElement element, boolean result) {
    }

    public void beforeGetLocation(WebElement element) {
    }

    public void afterGetLocation(WebElement element, Point result) {
    }

    public void beforeGetSize(WebElement element) {
    }

    public void afterGetSize(WebElement element, Dimension result) {
    }

    public void beforeGetCssValue(WebElement element, String propertyName) {
    }

    public void afterGetCssValue(WebElement element, String propertyName, String result) {
    }

    // Navigation

    public void beforeAnyNavigationCall(WebDriver.Navigation navigation, Method method, Object[] args) {
    }

    public void afterAnyNavigationCall(WebDriver.Navigation navigation, Method method, Object[] args, Object result) {
    }

    public void beforeTo(WebDriver.Navigation navigation, String url) {
        if (!isEngineCallToNativeMethod()) {
            ReportManager.log("Navigate to url: \"" + url + "\".");
        }
    }

    public void afterTo(WebDriver.Navigation navigation, String url) {
    }

    public void beforeTo(WebDriver.Navigation navigation, URL url) {
        if (!isEngineCallToNativeMethod()) {
            ReportManager.log("Navigate to url: \"" + url + "\".");
        }
    }

    public void afterTo(WebDriver.Navigation navigation, URL url) {
    }

    public void beforeBack(WebDriver.Navigation navigation) {
        if (!isEngineCallToNativeMethod()) {
            ReportManager.log("Navigate back.");
        }
    }

    public void afterBack(WebDriver.Navigation navigation) {
    }

    public void beforeForward(WebDriver.Navigation navigation) {
        if (!isEngineCallToNativeMethod()) {
            ReportManager.log("Navigate forward.");
        }
    }

    public void afterForward(WebDriver.Navigation navigation) {
    }

    public void beforeRefresh(WebDriver.Navigation navigation) {
        if (!isEngineCallToNativeMethod()) {
            ReportManager.log("Refresh current page.");
        }
    }

    public void afterRefresh(WebDriver.Navigation navigation) {
    }

    // Alert

    public void beforeAnyAlertCall(Alert alert, Method method, Object[] args) {
    }

    public void afterAnyAlertCall(Alert alert, Method method, Object[] args, Object result) {
    }

    public void beforeAccept(Alert alert) {
    }

    public void afterAccept(Alert alert) {
    }

    public void beforeDismiss(Alert alert) {
    }

    public void afterDismiss(Alert alert) {
    }

    public void beforeGetText(Alert alert) {
    }

    public void afterGetText(Alert alert, String result) {
    }

    public void beforeSendKeys(Alert alert, String text) {
        if (!isEngineCallToNativeMethod()) {
            ReportManager.log("Type \"" + text + "\" into Alert.");
        }
    }

    public void afterSendKeys(Alert alert, String text) {
    }

    // Options

    public void beforeAnyOptionsCall(WebDriver.Options options, Method method, Object[] args) {
    }

    public void afterAnyOptionsCall(WebDriver.Options options, Method method, Object[] args, Object result) {
    }

    public void beforeAddCookie(WebDriver.Options options, Cookie cookie) {
    }

    public void afterAddCookie(WebDriver.Options options, Cookie cookie) {
    }

    public void beforeDeleteCookieNamed(WebDriver.Options options, String name) {
    }

    public void afterDeleteCookieNamed(WebDriver.Options options, String name) {
    }

    public void beforeDeleteCookie(WebDriver.Options options, Cookie cookie) {
    }

    public void afterDeleteCookie(WebDriver.Options options, Cookie cookie) {
    }

    public void beforeDeleteAllCookies(WebDriver.Options options) {
    }

    public void afterDeleteAllCookies(WebDriver.Options options) {
    }

    public void beforeGetCookies(WebDriver.Options options) {
    }

    public void afterGetCookies(WebDriver.Options options, Set<Cookie> result) {
    }

    public void beforeGetCookieNamed(WebDriver.Options options, String name) {
    }

    public void afterGetCookieNamed(WebDriver.Options options, String name, Cookie result) {
    }

    // Timeouts

    public void beforeAnyTimeoutsCall(WebDriver.Timeouts timeouts, Method method, Object[] args) {
    }

    public void afterAnyTimeoutsCall(WebDriver.Timeouts timeouts, Method method, Object[] args, Object result) {
    }

    public void beforeImplicitlyWait(WebDriver.Timeouts timeouts, Duration duration) {
    }

    public void afterImplicitlyWait(WebDriver.Timeouts timeouts, Duration duration) {
    }

    public void beforeSetScriptTimeout(WebDriver.Timeouts timeouts, Duration duration) {
    }

    public void afterSetScriptTimeout(WebDriver.Timeouts timeouts, Duration duration) {
    }

    public void beforePageLoadTimeout(WebDriver.Timeouts timeouts, Duration duration) {
    }

    public void afterPageLoadTimeout(WebDriver.Timeouts timeouts, Duration duration) {
    }

    // Window

    public void beforeAnyWindowCall(WebDriver.Window window, Method method, Object[] args) {
    }

    public void afterAnyWindowCall(WebDriver.Window window, Method method, Object[] args, Object result) {
    }

    public void beforeGetSize(WebDriver.Window window) {
    }

    public void afterGetSize(WebDriver.Window window, Dimension result) {
    }

    public void beforeSetSize(WebDriver.Window window, Dimension size) {
    }

    public void afterSetSize(WebDriver.Window window, Dimension size) {
    }

    public void beforeGetPosition(WebDriver.Window window) {
    }

    public void afterGetPosition(WebDriver.Window window, Point result) {
    }

    public void beforeSetPosition(WebDriver.Window window, Point position) {
    }

    public void afterSetPosition(WebDriver.Window window, Point position) {
    }

    public void beforeMaximize(WebDriver.Window window) {
        if (!isEngineCallToNativeMethod()) {
            ReportManager.log("Maximize Current Window.");
        }
    }

    public void afterMaximize(WebDriver.Window window) {
    }

    public void beforeFullscreen(WebDriver.Window window) {
    }

    public void afterFullscreen(WebDriver.Window window) {
    }

    private String getElementName(WebElement element) {
        var accessibleName = element.getAccessibleName();
        if ("".equals(accessibleName)) {
            return "element";
        } else {
            return accessibleName;
        }
    }

    private boolean isEngineCallToNativeMethod() {
        //declaringClass="io.github.shafthq.shaft.gui.browser.WebDriverBrowserActions"
        var callingstack = Thread.currentThread().getStackTrace();
        ArrayList<String> internalClasses = new ArrayList<>();
        internalClasses.add(WebDriverElementActions.class.getCanonicalName());
        internalClasses.add(WebDriverBrowserActions.class.getCanonicalName());

        for (StackTraceElement element : callingstack) {
            if (internalClasses.contains(element.getClassName())) {
                return true;
            }
        }
        return false;
    }
}