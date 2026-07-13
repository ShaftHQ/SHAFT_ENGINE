package com.shaft.validation.internal;

import com.shaft.validation.ValidationEnums;
import com.shaft.validation.VisualComparisonOptions;
import org.openqa.selenium.WebDriver;

public class WebDriverBrowserValidationsBuilder implements com.shaft.gui.driver.BrowserAssertions {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected final WebDriver driver;
    protected final StringBuilder reportMessageBuilder;
    protected String validationMethod;
    protected String browserAttribute;

    public WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, WebDriver driver, StringBuilder reportMessageBuilder) {
        this.validationCategory = validationCategory;
        this.driver = driver;

        this.reportMessageBuilder = reportMessageBuilder;
    }

    /**
     * Use this to check against a certain browser attribute
     *
     * @param browserAttribute the target browser attribute that will be checked against
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    @Override
    public NativeValidationsBuilder attribute(String browserAttribute) {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = browserAttribute;
        reportMessageBuilder.append("attribute \"").append(browserAttribute).append("\" ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check against the current page URL
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    @SuppressWarnings("SpellCheckingInspection")
    @Override
    public NativeValidationsBuilder url() {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = "currenturl";
        reportMessageBuilder.append("URL ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check against the current page title
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    @Override
    public NativeValidationsBuilder title() {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = "title";
        reportMessageBuilder.append("title ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check against the current browser alert text.
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    @Override
    public NativeValidationsBuilder alertText() {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = "alerttext";
        reportMessageBuilder.append("alert text ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check against the current page text content.
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    @Override
    public NativeValidationsBuilder text() {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = "text";
        reportMessageBuilder.append("text ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check that the current page matches its visual-regression baseline screenshot
     * (full-page pixel diff via OpenCV). On the first test run this method takes a full-page screenshot
     * and the test passes, saving it as the baseline for subsequent runs. The comparison executes
     * immediately &mdash; no {@code perform()} is required.
     *
     * @return a ValidationsExecutor object to optionally set a custom validation message
     */
    @Override
    public ValidationsExecutor matchesScreenshot() {
        return matchesScreenshot(null);
    }

    /**
     * Same as {@link #matchesScreenshot()}, but with diff-budget/mask options (see
     * {@link VisualComparisonOptions}). The comparison executes immediately.
     *
     * @param options the visual comparison options (diff budgets, masks), or {@code null} for defaults
     * @return a ValidationsExecutor object to optionally set a custom validation message
     */
    @Override
    public ValidationsExecutor matchesScreenshot(VisualComparisonOptions options) {
        reportMessageBuilder.append("page matches the visual regression baseline screenshot.");
        return new VisualValidationsBuilder(validationCategory, driver, null, true, reportMessageBuilder)
                .applyOptions(options)
                .perform();
    }

}
