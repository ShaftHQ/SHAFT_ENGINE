package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactoryHelper;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.openqa.selenium.Alert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.FluentWait;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.time.Duration;
import java.util.Arrays;

import static com.shaft.gui.element.internal.ElementActionsHelper.getExpectedExceptions;

public class WebDriverListener implements org.openqa.selenium.support.events.WebDriverListener, io.appium.java_client.proxy.MethodCallListener {
    private static final double DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT = SHAFT.Properties.timeouts.defaultElementIdentificationTimeout();
    private static final int ELEMENT_IDENTIFICATION_POLLING_DELAY = 100; // milliseconds

    // Global

    public void afterAnyCall(Object target, Method method, Object[] args, Object result) {
//        ReportManager.log(JavaHelper.convertToSentenceCase(method.getName() )+ " action performed.");
    }

    public void onError(Object target, Method method, Object[] args, InvocationTargetException e) {
        ReportManager.log(JavaHelper.convertToSentenceCase(method.getName()) + " action failed.");
        ReportManagerHelper.attach(ScreenshotManager.captureScreenShot(DriverFactoryHelper.getDriver(), method.getName(), false));
        ReportManagerHelper.logDiscrete(e);
    }

    // WebDriver

    public void afterGet(WebDriver driver, String url) {
        ReportManager.log("Navigate to \"" + url + "\".");
    }

    public void afterGetCurrentUrl(String result, WebDriver driver) {
        ReportManager.log("Current url is: \"" + result + "\".");
    }

    public void afterGetTitle(WebDriver driver, String result) {
        ReportManager.log("Current Window Title is: \"" + result + "\".");
    }

    public void beforeFindElement(WebDriver driver, By locator) {
        if (SHAFT.Properties.flags.respectBuiltInWaitsInNativeMode()) {
            try {
                new FluentWait<>(driver)
                        .withTimeout(Duration.ofMillis((long) DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT))
                        .pollingEvery(Duration.ofMillis(ELEMENT_IDENTIFICATION_POLLING_DELAY))
                        .ignoreAll(getExpectedExceptions(false))
                        .until(nestedDriver -> nestedDriver.findElement(locator));
            } catch (org.openqa.selenium.TimeoutException timeoutException) {
                // In case the element was not found / not visible and the timeout expired
                ReportManager.logDiscrete(timeoutException.getMessage() + " || " + timeoutException.getCause().getMessage().substring(0, timeoutException.getCause().getMessage().indexOf("\n")));
                throw timeoutException;
            }
        }
    }

    public void afterClose(WebDriver driver) {
            ReportManager.log("Successfully Closed Driver.");
    }

    public void afterQuit(WebDriver driver) {
            ReportManager.log("Successfully Quit Driver.");
    }

    // WebElement

    public void beforeClick(WebElement element) {
        if (SHAFT.Properties.flags.respectBuiltInWaitsInNativeMode()) {
            try {
                (new WebDriverWait(DriverFactoryHelper.getDriver(), Duration.ofMillis((long) DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT)))
                        .until(ExpectedConditions.elementToBeClickable(element));
            } catch (org.openqa.selenium.TimeoutException timeoutException) {
                ReportManagerHelper.logDiscrete(timeoutException);
                throw timeoutException;
            }
        }
        try {
            ReportManager.log("Click " + getElementName(element) + ".");
        } catch (Exception throwable) {
            ReportManager.log("Click.");
        }
    }

    public void beforeSubmit(WebElement element) {
        try {
            ReportManager.log("Submit " + getElementName(element) + ".");
        } catch (Exception throwable) {
            ReportManager.log("Submit.");
        }
    }

    public void beforeSendKeys(WebElement element, CharSequence... keysToSend) {
        var stringBuilder = new StringBuilder();
        Arrays.stream(keysToSend).toList().forEach(stringBuilder::append);
        try {
            ReportManager.log("Type \"" + stringBuilder + "\" into " + getElementName(element) + ".");
        } catch (Exception throwable) {
            ReportManager.log("Type \"" + stringBuilder + "\".");
        }
    }

    public void beforeClear(WebElement element) {
        try {
            ReportManager.log("Clear " + getElementName(element) + ".");
        } catch (Exception throwable) {
            ReportManager.log("Clear.");
        }
    }

    public void afterGetAttribute(WebElement element, String name, String result) {
        try {
            ReportManager.log("Get Attribute \"" + name + "\" from " + getElementName(element) + ", value is \"" + result + "\".");
        } catch (Exception throwable) {
            ReportManager.log("Get Attribute \"" + name + "\", value is \"" + result + "\".");
        }
    }

    public void afterGetText(WebElement element, String result) {
        try {
            ReportManager.log("Get Text from " + getElementName(element) + ", text is \"" + result + "\".");
        } catch (Exception throwable) {
            ReportManager.log("Get Text, text is :\"" + result + "\".");
        }
    }

    // Navigation

    public void afterTo(WebDriver.Navigation navigation, String url) {
        ReportManager.log("Navigate to url \"" + url + "\".");
    }

    public void afterTo(WebDriver.Navigation navigation, URL url) {
        ReportManager.log("Navigate to url \"" + url + "\".");
    }

    public void afterBack(WebDriver.Navigation navigation) {
        ReportManager.log("Navigate back.");
    }

    public void afterForward(WebDriver.Navigation navigation) {
        ReportManager.log("Navigate forward.");
    }

    public void afterRefresh(WebDriver.Navigation navigation) {
        ReportManager.log("Refresh current page.");
    }

    // Alert

    public void beforeSendKeys(Alert alert, String text) {
            ReportManager.log("Type \"" + text + "\" into Alert.");
    }

    // Options

    // Timeouts

    // Window

    public void afterMaximize(WebDriver.Window window) {
        ReportManager.log("Maximize Current Window.");
    }

    private String getElementName(WebElement element) {
        var accessibleName = element.getAccessibleName();
        if ("".equals(accessibleName)) {
            return "element";
        } else {
            return "\"" + accessibleName + "\"";
        }
    }
}