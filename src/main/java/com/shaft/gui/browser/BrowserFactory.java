package com.shaft.gui.browser;

import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.WebDriver;

import com.shaft.driver.DriverFactory.DriverType;
import com.shaft.driver.DriverFactoryHelper;

public class BrowserFactory {
	private BrowserFactory() {
		throw new IllegalStateException("Utility class");
	}

	/**
	 * Read the target browser value from the execution.properties file
	 *
	 * @return a new browser instance
	 */
	public static WebDriver getBrowser() {
		return DriverFactoryHelper.getDriver();
	}

	/**
	 * Creates a new browser instance
	 *
	 * @param browserType one of the supported browser types
	 * @return a new browser instance
	 */
	public static WebDriver getBrowser(DriverType browserType) {
		return DriverFactoryHelper.getDriver(browserType);
	}

	/**
	 * Creates a new browser instance with custom browser options
	 *
	 * @param browserType          one of the supported browser types
	 * @param customBrowserOptions the custom options that will be used to create
	 *                             this new browser instance
	 * @return a new browser instance
	 */
	public static WebDriver getBrowser(DriverType browserType, MutableCapabilities customBrowserOptions) {
		return DriverFactoryHelper.getDriver(browserType, customBrowserOptions);
	}
	
	/**
	 * Close all open browser instances.
	 */
	public static synchronized void closeAllBrowsers() {
		DriverFactoryHelper.closeAllDrivers();
	}
	
}
