package com.shaft.validation.internal;

/**
 * Extends ValidationsExecutor with constructors for WebDriver-specific builder types.
 * Lives in shaft-engine (which has the Selenium dependency) rather than shaft-core.
 */
public class WebDriverValidationsExecutor extends ValidationsExecutor {

    public WebDriverValidationsExecutor(WebDriverElementValidationsBuilder b) {
        super(b.validationCategory, b.validationType, b.validationMethod, b.reportMessageBuilder);
        setDriver(b.driver);
        this.locator = b.locator;
        this.visualValidationEngine = b.visualValidationEngine;
    }
}
