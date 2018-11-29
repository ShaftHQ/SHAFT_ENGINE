package com.shaft.element;

import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.support.ui.ExpectedCondition;
import org.openqa.selenium.support.ui.WebDriverWait;

import com.shaft.io.ReportManager;

public class JSWaiter {

    private static boolean debug = false;

    private static WebDriver jsWaitDriver;
    private static WebDriverWait jsWait;
    private static JavascriptExecutor jsExec;

    private JSWaiter() {
	throw new IllegalStateException("Utility class");
    }

    // Get the driver
    public static void setDriver(WebDriver driver) {
	jsWaitDriver = driver;
	jsWait = new WebDriverWait(jsWaitDriver, 10);
	jsExec = (JavascriptExecutor) jsWaitDriver;
    }

    // Wait for JQuery Load
    public static void waitForJQueryLoad() {
	// Wait for jQuery to load
	ExpectedCondition<Boolean> jQueryLoad = driver -> ((Long) ((JavascriptExecutor) jsWaitDriver)
		.executeScript("return jQuery.active") == 0);

	// Get JQuery is Ready
	boolean jqueryReady = (Boolean) jsExec.executeScript("return jQuery.active==0");

	if (!jqueryReady) {
	    // Wait JQuery until it is Ready!
	    if (debug) {
		ReportManager.log("Waiting for JQuery to be Ready!");
	    }
	    int tryCounter = 0;
	    while ((!jqueryReady) && (tryCounter < 5)) {
		if (debug) {
		    ReportManager.log("JQuery is NOT Ready!");
		}
		// Wait for jQuery to load
		jsWait.until(jQueryLoad);
		sleep(20);
		tryCounter++;
		jqueryReady = (Boolean) jsExec.executeScript("return jQuery.active == 0");
	    }
	    if (debug) {
		ReportManager.log("JQuery is Ready!");
	    }
	} else {
	    if (debug) {
		ReportManager.log("JQuery is Ready!");
	    }
	}
    }

    // Wait for Angular Load
    public static void waitForAngularLoad() {
	WebDriverWait wait = new WebDriverWait(jsWaitDriver, 15);
	JavascriptExecutor jsExec = (JavascriptExecutor) jsWaitDriver;

	String angularReadyScript = "return angular.element(document).injector().get('$http').pendingRequests.length === 0";

	// Wait for ANGULAR to load
	ExpectedCondition<Boolean> angularLoad = driver -> Boolean
		.valueOf(((JavascriptExecutor) driver).executeScript(angularReadyScript).toString());

	// Get Angular is Ready
	boolean angularReady = Boolean.parseBoolean(jsExec.executeScript(angularReadyScript).toString());

	if (!angularReady) {
	    // Wait ANGULAR until it is Ready!
	    if (debug) {
		ReportManager.log("Waiting for ANGULAR to be Ready!");
	    }
	    int tryCounter = 0;
	    while ((!angularReady) && (tryCounter < 5)) {
		if (debug) {
		    ReportManager.log("ANGULAR is NOT Ready!");
		}
		// Wait for Angular to load
		wait.until(angularLoad);
		// More Wait for stability (Optional)
		sleep(20);
		tryCounter++;
		angularReady = Boolean.valueOf(jsExec.executeScript(angularReadyScript).toString());
	    }
	    if (debug) {
		ReportManager.log("ANGULAR is Ready!");
	    }
	} else {
	    if (debug) {
		ReportManager.log("ANGULAR is Ready!");
	    }
	}
    }

    // Wait Until JS Ready
    public static void waitForJSLoad() {
	WebDriverWait wait = new WebDriverWait(jsWaitDriver, 15);
	JavascriptExecutor jsExec = (JavascriptExecutor) jsWaitDriver;

	// Wait for Javascript to load
	ExpectedCondition<Boolean> jsLoad = driver -> ((JavascriptExecutor) jsWaitDriver)
		.executeScript("return document.readyState").toString().trim().equalsIgnoreCase("complete");

	// Get JS is Ready
	boolean jsReady = (Boolean) jsExec.executeScript("return document.readyState").toString().trim()
		.equalsIgnoreCase("complete");

	// Wait Javascript until it is Ready!
	if (!jsReady) {
	    // Wait JS until it is Ready!
	    if (debug) {
		ReportManager.log("Waiting for JS to be Ready!");
	    }
	    int tryCounter = 0;
	    while ((!jsReady) && (tryCounter < 5)) {
		if (debug) {
		    ReportManager.log("JS in NOT Ready!");
		}
		// Wait for Javascript to load
		wait.until(jsLoad);
		// More Wait for stability (Optional)
		sleep(20);
		tryCounter++;
		jsReady = (Boolean) jsExec.executeScript("return document.readyState").toString().trim()
			.equalsIgnoreCase("complete");
	    }
	    if (debug) {
		ReportManager.log("JS is Ready!");
	    }
	} else {
	    if (debug) {
		ReportManager.log("JS is Ready!");
	    }
	}
    }

    /**
     * Waits for jQuery, Angular, and/or Javascript if present on the current page.
     */

    public static void waitForLazyLoading() {
	try {
	    Boolean jQueryDefined = (Boolean) jsExec.executeScript("return typeof jQuery != 'undefined'");
	    if (jQueryDefined) {
		waitForJQueryLoad();
	    } else {
		if (debug) {
		    ReportManager.log("jQuery is not defined on this site!");
		}
	    }

	    try {
		// check if angular is defined
		waitForAngularIfDefined();
	    } catch (org.openqa.selenium.WebDriverException e) {
		if (debug) {
		    ReportManager.log(e);
		    ReportManager.log("Angular is not defined on this site!");
		}
	    }
	    // wait for all nested iframes to load, then wait for the main page to load
	    int iframes = ElementActions.getElementsCount(jsWaitDriver, By.tagName("iframe"), 1, 0, false);
	    if (iframes > 0) {
		for (int i = 0; i < iframes; i++) {
		    Boolean jsReady = (Boolean) jsExec.executeScript(
			    "return document.getElementsByTagName('iframe')['" + i + "'].contentDocument.readyState")
			    .toString().trim().equalsIgnoreCase("complete");
		    if (!jsReady) {
			waitForJSLoad();
		    } else {
			if (debug) {
			    ReportManager.log("JS is Ready!");
			}
		    }
		}
	    }

	    Boolean jsReady = (Boolean) jsExec.executeScript("return document.readyState").toString().trim()
		    .equalsIgnoreCase("complete");
	    if (!jsReady) {
		waitForJSLoad();
	    } else {
		if (debug) {
		    ReportManager.log("JS is Ready!");
		}
	    }
	} catch (WebDriverException e) {
	    ReportManager.log(e);

	}
    }

    private static void waitForAngularIfDefined() {
	Boolean angularDefined = !((Boolean) jsExec.executeScript("return window.angular === undefined"));
	if (angularDefined) {
	    Boolean angularInjectorDefined = !((Boolean) jsExec
		    .executeScript("return angular.element(document).injector() === undefined"));

	    if (angularInjectorDefined) {
		waitForAngularLoad();
	    } else {
		if (debug) {
		    ReportManager.log("Angular injector is not defined on this site!");
		}
	    }
	} else {
	    if (debug) {
		ReportManager.log("Angular is not defined on this site!");
	    }
	}
    }

    public static void sleep(Integer milliSeconds) {
	long secondsLong = (long) milliSeconds;
	try {
	    Thread.sleep(secondsLong);
	} catch (Exception e) {
	    ReportManager.log(e);
	    // InterruptedException
	}
    }
}