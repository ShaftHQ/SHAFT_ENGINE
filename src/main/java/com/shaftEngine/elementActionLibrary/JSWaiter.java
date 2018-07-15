package com.shaftEngine.elementActionLibrary;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.ExpectedCondition;
import org.openqa.selenium.support.ui.WebDriverWait;

public class JSWaiter {

	private static boolean debug = false;

	private static WebDriver jsWaitDriver;
	private static WebDriverWait jsWait;
	private static JavascriptExecutor jsExec;

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
				System.out.println("Waiting for JQuery to be Ready!");
			}
			int tryCounter = 0;
			while ((!jqueryReady) && (tryCounter < 5)) {
				if (debug) {
					System.out.println("JQuery is NOT Ready!");
				}
				// Wait for jQuery to load
				jsWait.until(jQueryLoad);
				sleep(20);
				tryCounter++;
				jqueryReady = (Boolean) jsExec.executeScript("return jQuery.active==0");
			}
			if (debug) {
				System.out.println("JQuery is Ready!");
			}
		} else {
			if (debug) {
				System.out.println("JQuery is Ready!");
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
		boolean angularReady = Boolean.valueOf(jsExec.executeScript(angularReadyScript).toString());

		if (!angularReady) {
			// Wait ANGULAR until it is Ready!
			if (debug) {
				System.out.println("Waiting for ANGULAR to be Ready!");
			}
			int tryCounter = 0;
			while ((!angularReady) && (tryCounter < 5)) {
				if (debug) {
					System.out.println("ANGULAR is NOT Ready!");
				}
				// Wait for Angular to load
				wait.until(angularLoad);
				// More Wait for stability (Optional)
				sleep(20);
				tryCounter++;
				angularReady = Boolean.valueOf(jsExec.executeScript(angularReadyScript).toString());
			}
			if (debug) {
				System.out.println("ANGULAR is Ready!");
			}
		} else {
			if (debug) {
				System.out.println("ANGULAR is Ready!");
			}
		}
	}

	// Wait Until JS Ready
	public static void waitForJSLoad() {
		WebDriverWait wait = new WebDriverWait(jsWaitDriver, 15);
		JavascriptExecutor jsExec = (JavascriptExecutor) jsWaitDriver;

		// Wait for Javascript to load
		ExpectedCondition<Boolean> jsLoad = driver -> ((JavascriptExecutor) jsWaitDriver)
				.executeScript("return document.readyState").toString().equals("complete");

		// Get JS is Ready
		boolean jsReady = (Boolean) jsExec.executeScript("return document.readyState").toString().equals("complete");

		// Wait Javascript until it is Ready!
		if (!jsReady) {
			// Wait JS until it is Ready!
			if (debug) {
				System.out.println("Waiting for JS to be Ready!");
			}
			int tryCounter = 0;
			while ((!jsReady) && (tryCounter < 5)) {
				if (debug) {
					System.out.println("JS in NOT Ready!");
				}
				// Wait for Javascript to load
				wait.until(jsLoad);
				// More Wait for stability (Optional)
				sleep(20);
				tryCounter++;
				jsReady = (Boolean) jsExec.executeScript("return document.readyState").toString().equals("complete");
			}
			if (debug) {
				System.out.println("JS is Ready!");
			}
		} else {
			if (debug) {
				System.out.println("JS is Ready!");
			}
		}
	}
	//
	// // Wait Until JQuery and JS Ready
	// public static void waitUntilJQueryReady() {
	// JavascriptExecutor jsExec = (JavascriptExecutor) jsWaitDriver;
	//
	// // First check that JQuery is defined on the page. If it is, then wait AJAX
	// Boolean jQueryDefined = (Boolean) jsExec.executeScript("return typeof jQuery
	// != 'undefined'");
	// if (jQueryDefined == true) {
	// // Pre Wait for stability (Optional)
	// // sleep(20);
	//
	// // Wait JQuery Load
	// waitForJQueryLoad();
	//
	// // Wait JS Load
	// waitForJSLoad();
	//
	// // Post Wait for stability (Optional)
	// // sleep(20);
	// } else {
	// if (debug) {
	// System.out.println("jQuery is not defined on this site!");
	// }
	// }
	// }
	//
	// // Wait Until Angular and JS Ready
	// public static void waitUntilAngularReady() {
	// JavascriptExecutor jsExec = (JavascriptExecutor) jsWaitDriver;
	//
	// // First check that ANGULAR is defined on the page. If it is, then wait
	// ANGULAR
	// Boolean angularUnDefined = (Boolean) jsExec.executeScript("return
	// window.angular === undefined");
	// if (!angularUnDefined) {
	// Boolean angularInjectorUnDefined = (Boolean) jsExec
	// .executeScript("return angular.element(document).injector() === undefined");
	// if (!angularInjectorUnDefined) {
	// // Pre Wait for stability (Optional)
	// // sleep(20);
	//
	// // Wait Angular Load
	// waitForAngularLoad();
	//
	// // Wait JS Load
	// waitForJSLoad();
	//
	// // Post Wait for stability (Optional)
	// // sleep(20);
	// } else {
	// if (debug) {
	// System.out.println("Angular injector is not defined on this site!");
	// }
	// }
	// } else {
	// if (debug) {
	// System.out.println("Angular is not defined on this site!");
	// }
	// }
	// }
	//
	// // Wait Until JQuery Angular and JS is ready
	// public static void waitJQueryAngular() {
	// waitUntilJQueryReady();
	// waitUntilAngularReady();
	// waitForJSLoad(); // extra step in case there are no JQuery or Angular
	// requests, but there is JS.
	// }

	/**
	 * Waits for jQuery, Angular, and/or Javascript if present on the current page.
	 */

	public static void waitForLazyLoading() {
		Boolean jQueryDefined = (Boolean) jsExec.executeScript("return typeof jQuery != 'undefined'");
		if (jQueryDefined) {
			waitForJQueryLoad();
		} else {
			if (debug) {
				System.out.println("jQuery is not defined on this site!");
			}
		}

		try {
			Boolean angularDefined = !((Boolean) jsExec.executeScript("return window.angular === undefined"));
			if (angularDefined) {
				Boolean angularInjectorDefined = !((Boolean) jsExec
						.executeScript("return angular.element(document).injector() === undefined"));

				if (angularInjectorDefined) {
					waitForAngularLoad();
				} else {
					if (debug) {
						System.out.println("Angular injector is not defined on this site!");
					}
				}
			} else {
				if (debug) {
					System.out.println("Angular is not defined on this site!");
				}
			}
		} catch (org.openqa.selenium.WebDriverException ex) {
			if (debug) {
				System.out.println("Angular is not defined on this site!");
			}
		}

		Boolean jsReady = (Boolean) jsExec.executeScript("return document.readyState").toString().equals("complete");
		if (!jsReady) {
			waitForJSLoad();
		} else {
			if (debug) {
				System.out.println("JS is Ready!");
			}
		}

	}

	public static void sleep(Integer milliSeconds) {
		long secondsLong = (long) milliSeconds;
		try {
			Thread.sleep(secondsLong);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}