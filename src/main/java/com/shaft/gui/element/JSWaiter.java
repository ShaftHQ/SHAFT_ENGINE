package com.shaft.gui.element;

//import java.time.Duration;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.support.ui.ExpectedCondition;
import org.openqa.selenium.support.ui.WebDriverWait;

import com.shaft.tools.io.ReportManager;
import com.shaft.tools.support.JSHelpers;

public class JSWaiter {
    private static WebDriver jsWaitDriver;
    private static JavascriptExecutor jsExec;
//    private static final Duration WAIT_DURATION = Duration.ofSeconds(15);
    private static final int WAIT_DURATION_INTEGER = 15;
    private static int delayBetweenPolls = 20; // milliseconds
    private static final String TARGET_DOCUMENT_READY_STATE = "complete";

    private JSWaiter() {
	throw new IllegalStateException("Utility class");
    }

    // Get the driver
    public static void setDriver(WebDriver driver) {
	jsWaitDriver = driver;
	jsExec = (JavascriptExecutor) jsWaitDriver;
    }

    /**
     * Waits for jQuery, Angular, and/or Javascript if present on the current page.
     * 
     * @return true in case waiting didn't face any isssues, and false in case of a
     *         severe exception
     */
    public static boolean waitForLazyLoading() {
	try {
	    waitForJQueryLoadIfDefined();
	    waitForAngularIfDefined();
	    waitForJSLoadIfDefined();
	    return true;
	} catch (WebDriverException e) {
	    ReportManager.log(e);
	    return true;
	} catch (NullPointerException e) {
	    return true;
	} catch (Exception e) {
	    if (e.getMessage().contains("jQuery is not defined")) {
		// do nothing
		return true;
	    } else if (e.getMessage().contains("Error communicating with the remote browser. It may have died.")) {
		ReportManager.log(e);
		return false;
	    } else {
		ReportManager.log(e);
		return true;
	    }
	}
    }

    // Wait for JQuery Load
    private static void waitForJQueryLoadIfDefined() {
	Boolean jQueryDefined = (Boolean) jsExec.executeScript("return typeof jQuery != 'undefined'");
	if (Boolean.TRUE.equals(jQueryDefined)) {
	    ExpectedCondition<Boolean> jQueryLoad = null;
	    try {
		// Wait for jQuery to load
		jQueryLoad = driver -> ((Long) ((JavascriptExecutor) jsWaitDriver)
			.executeScript("return jQuery.active") == 0);
	    } catch (NullPointerException e) {
		// do nothing
	    }
	    // Get JQuery is Ready
	    boolean jqueryReady = (Boolean) jsExec.executeScript("return jQuery.active==0");

	    if (!jqueryReady) {
		// Wait JQuery until it is Ready!
		int tryCounter = 0;
		while ((!jqueryReady) && (tryCounter < 5)) {
		    try {
			// Wait for jQuery to load
			(new WebDriverWait(jsWaitDriver, WAIT_DURATION_INTEGER)).until(jQueryLoad);
		    } catch (NullPointerException e) {
			// do nothing
		    }
		    sleep(delayBetweenPolls);
		    tryCounter++;
		    jqueryReady = (Boolean) jsExec.executeScript("return jQuery.active == 0");
		}
	    }
	}
    }

    // Wait for Angular Load
    private static void waitForAngularLoad() {
	JavascriptExecutor jsExec = (JavascriptExecutor) jsWaitDriver;

	String angularReadyScript = "return angular.element(document).injector().get('$http').pendingRequests.length === 0";

	// Wait for ANGULAR to load
	ExpectedCondition<Boolean> angularLoad = driver -> Boolean
		.valueOf(((JavascriptExecutor) driver).executeScript(angularReadyScript).toString());

	// Get Angular is Ready
	boolean angularReady = Boolean.parseBoolean(jsExec.executeScript(angularReadyScript).toString());

	if (!angularReady) {
	    // Wait ANGULAR until it is Ready!
	    int tryCounter = 0;
	    while ((!angularReady) && (tryCounter < 5)) {
		// Wait for Angular to load
		(new WebDriverWait(jsWaitDriver, WAIT_DURATION_INTEGER)).until(angularLoad);
		// More Wait for stability (Optional)
		sleep(delayBetweenPolls);
		tryCounter++;
		angularReady = Boolean.valueOf(jsExec.executeScript(angularReadyScript).toString());
	    }
	}
    }

    // Wait Until JS Ready
    private static void waitForJSLoadIfDefined() {
	JavascriptExecutor jsExec = (JavascriptExecutor) jsWaitDriver;

	// Wait for Javascript to load
	ExpectedCondition<Boolean> jsLoad = driver -> ((JavascriptExecutor) jsWaitDriver)
		.executeScript(JSHelpers.DOCUMENT_READYSTATE.getValue()).toString().trim()
		.equalsIgnoreCase(TARGET_DOCUMENT_READY_STATE);

	// Get JS is Ready
	boolean jsReady = (Boolean) jsExec.executeScript(JSHelpers.DOCUMENT_READYSTATE.getValue()).toString().trim()
		.equalsIgnoreCase(TARGET_DOCUMENT_READY_STATE);

	// Wait Javascript until it is Ready!
	if (!jsReady) {
	    // Wait JS until it is Ready!
	    int tryCounter = 0;
	    while ((!jsReady) && (tryCounter < 5)) {
		// Wait for Javascript to load
		(new WebDriverWait(jsWaitDriver, WAIT_DURATION_INTEGER)).until(jsLoad);
		// More Wait for stability (Optional)
		sleep(delayBetweenPolls);
		tryCounter++;
		jsReady = (Boolean) jsExec.executeScript(JSHelpers.DOCUMENT_READYSTATE.getValue()).toString().trim()
			.equalsIgnoreCase(TARGET_DOCUMENT_READY_STATE);
	    }
	}
    }

    private static void waitForAngularIfDefined() {
	try {
	    Boolean angularDefined = !((Boolean) jsExec.executeScript("return window.angular === undefined"));
	    if (Boolean.TRUE.equals(angularDefined)) {
		Boolean angularInjectorDefined = !((Boolean) jsExec
			.executeScript("return angular.element(document).injector() === undefined"));

		if (Boolean.TRUE.equals(angularInjectorDefined)) {
		    waitForAngularLoad();
		}
	    }
	} catch (WebDriverException e) {
	    // do nothing
	}
    }

    private static void sleep(Integer milliSeconds) {
	long secondsLong = (long) milliSeconds;
	try {
	    Thread.sleep(secondsLong);
	} catch (Exception e) {
	    ReportManager.log(e);
	    // InterruptedException
	}
    }
}