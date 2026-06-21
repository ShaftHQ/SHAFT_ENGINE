package com.shaft.gui.playwright.validation;

import com.shaft.gui.driver.BrowserAssertions;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.NativeValidationsBuilder;

public class PlaywrightBrowserValidationsBuilder implements BrowserAssertions {
    private final ValidationEnums.ValidationCategory validationCategory;
    private final PlaywrightSession session;
    private final StringBuilder reportMessageBuilder = new StringBuilder("the browser ");

    public PlaywrightBrowserValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, PlaywrightSession session) {
        this.validationCategory = validationCategory;
        this.session = session;
    }

    @Override
    public NativeValidationsBuilder attribute(String browserAttribute) {
        reportMessageBuilder.append("attribute \"").append(browserAttribute).append("\" ");
        return builder(browserAttribute);
    }

    @Override
    public NativeValidationsBuilder url() {
        reportMessageBuilder.append("URL ");
        return builder("url");
    }

    @Override
    public NativeValidationsBuilder title() {
        reportMessageBuilder.append("title ");
        return builder("title");
    }

    @Override
    public NativeValidationsBuilder text() {
        reportMessageBuilder.append("text ");
        return builder("text");
    }

    private NativeValidationsBuilder builder(String browserAttribute) {
        return new PlaywrightNativeValidationsBuilder(validationCategory, session, null, "browserAttributeEquals",
                null, null, browserAttribute, reportMessageBuilder);
    }
}
