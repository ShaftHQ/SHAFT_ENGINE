package com.shaft.gui.playwright.validation;

import com.microsoft.playwright.Locator;
import com.shaft.gui.driver.BrowserAssertions;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.VisualComparisonOptions;
import com.shaft.validation.internal.NativeValidationsBuilder;
import com.shaft.validation.internal.ValidationsExecutor;

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

    /**
     * Use this to check against the current browser alert text.
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    @Override
    public NativeValidationsBuilder alertText() {
        reportMessageBuilder.append("alert text ");
        return builder("alerttext");
    }

    @Override
    public NativeValidationsBuilder text() {
        reportMessageBuilder.append("text ");
        return builder("text");
    }

    @Override
    public ValidationsExecutor matchesScreenshot() {
        return matchesScreenshot(null);
    }

    @Override
    public ValidationsExecutor matchesScreenshot(VisualComparisonOptions options) {
        reportMessageBuilder.append("page matches the visual regression baseline screenshot.");
        var builder = new PlaywrightVisualValidationsBuilder(validationCategory, session, null, null, true, reportMessageBuilder);
        builder.applyOptions(options);
        if (options instanceof PlaywrightVisualComparisonOptions playwrightOptions) {
            builder.mask(playwrightOptions.getPlaywrightMaskLocators().toArray(new Locator[0]));
        }
        return builder.perform();
    }

    private NativeValidationsBuilder builder(String browserAttribute) {
        return new PlaywrightNativeValidationsBuilder(validationCategory, session, null, null, "browserAttributeEquals",
                null, null, browserAttribute, reportMessageBuilder);
    }
}
