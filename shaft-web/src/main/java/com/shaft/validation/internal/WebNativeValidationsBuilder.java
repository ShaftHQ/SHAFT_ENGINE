package com.shaft.validation.internal;

/**
 * Extends NativeValidationsBuilder with constructors that accept WebDriver builder types.
 * Lives in shaft-engine (which has the Selenium dependency) rather than shaft-core.
 */
public class WebNativeValidationsBuilder extends NativeValidationsBuilder {

    public WebNativeValidationsBuilder(WebDriverElementValidationsBuilder b) {
        super(b.validationCategory, b.validationMethod,
              b.driver, b.locator, b.elementAttribute, b.elementCssProperty, null,
              b.reportMessageBuilder);
    }

    public WebNativeValidationsBuilder(WebDriverBrowserValidationsBuilder b) {
        super(b.validationCategory, b.validationMethod,
              b.driver, null, null, null, b.browserAttribute,
              b.reportMessageBuilder);
    }
}
