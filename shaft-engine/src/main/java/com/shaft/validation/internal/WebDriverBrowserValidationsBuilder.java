package com.shaft.validation.internal;

import com.shaft.gui.browser.internal.BidiConsoleLogSource;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptor;
import com.shaft.gui.browser.internal.LegacyConsoleLogSource;
import com.shaft.gui.capabilities.AutomationCapabilities;
import com.shaft.gui.capabilities.AutomationFeature;
import com.shaft.gui.capabilities.internal.AutomationCapabilityResolver;
import com.shaft.gui.driver.BrowserConsoleMessage;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.VisualComparisonOptions;
import org.openqa.selenium.HasDownloads;
import org.openqa.selenium.NoAlertPresentException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.RemoteWebDriver;

import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;

public class WebDriverBrowserValidationsBuilder implements com.shaft.gui.driver.BrowserAssertions {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected final WebDriver driver;
    protected final StringBuilder reportMessageBuilder;
    protected String validationMethod;
    protected String browserAttribute;
    private final String reportMessagePrefix;

    public WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, WebDriver driver, StringBuilder reportMessageBuilder) {
        this.validationCategory = validationCategory;
        this.driver = driver;

        this.reportMessageBuilder = reportMessageBuilder;
        this.reportMessagePrefix = reportMessageBuilder.toString();
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

    @Override
    public NativeValidationsBuilder pageSourceValue() {
        return attributeValue("pagesource", "page source ");
    }

    @Override
    public NativeValidationsBuilder windowHandleValue() {
        return attributeValue("windowhandle", "window handle ");
    }

    @Override
    public NativeValidationsBuilder windowPositionValue() {
        return attributeValue("windowposition", "window position ");
    }

    @Override
    public NativeValidationsBuilder windowSizeValue() {
        return attributeValue("windowsize", "window size ");
    }

    @Override
    public NativeValidationsBuilder browsingContextCountValue() {
        return attributeValue("browsingcontextcount", "browsing context count ");
    }

    @Override
    public NativeValidationsBuilder dialogPresentValue() {
        return value("dialog present state", () -> {
            requireFeature(AutomationFeature.BROWSER_AUTOMATION);
            try {
                driver.switchTo().alert();
                return true;
            } catch (NoAlertPresentException ignored) {
                return false;
            }
        });
    }

    @Override
    public NativeValidationsBuilder downloadCountValue() {
        return value("download count", () -> {
            requireFeature(AutomationFeature.DOWNLOADS);
            if (driver instanceof HasDownloads downloads) {
                return downloads.getDownloadedFiles().size();
            }
            throw unsupported(AutomationFeature.DOWNLOADS);
        });
    }

    @Override
    public NativeValidationsBuilder networkObservationCountValue() {
        return value("network observation count", () -> {
            requireFeature(AutomationFeature.NETWORK_OBSERVATION);
            return BrowserNetworkInterceptor.observationCountIfPresent(driver)
                    .orElseThrow(() -> new UnsupportedOperationException(
                            "No retained network-observation state exists for the live browser session."));
        });
    }

    @Override
    public NativeValidationsBuilder consoleMessageCountValue() {
        return value("console message count", () -> consoleSnapshot().size());
    }

    @Override
    public NativeValidationsBuilder consoleErrorCountValue() {
        return value("console error count", () -> consoleSnapshot().stream()
                .filter(entry -> new BrowserConsoleMessage(entry.source(), entry.level(), entry.message(),
                        entry.timestamp()).isError())
                .count());
    }

    @Override
    public NativeValidationsBuilder featureSupportedValue(AutomationFeature feature) {
        AutomationFeature required = Objects.requireNonNull(feature, "feature");
        return value("feature support for " + required.name(), () -> capabilities().supports(required));
    }

    private NativeValidationsBuilder value(String name, Supplier<Object> reader) {
        return NativeValidationsBuilder.browserValue(validationCategory, driver, reader, name,
                new StringBuilder(reportMessagePrefix).append(name).append(' '));
    }

    private List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> consoleSnapshot() {
        requireFeature(AutomationFeature.CONSOLE_LOGS);
        if (!BidiConsoleLogSource.isHealthy(driver)) {
            return LegacyConsoleLogSource.snapshotIfPresent(driver).orElseThrow(() ->
                    new UnsupportedOperationException(
                            "No session-bound console-observation state exists for the live browser session."));
        }
        return BidiConsoleLogSource.snapshot(driver);
    }

    private AutomationCapabilities capabilities() {
        if (driver instanceof RemoteWebDriver remote && remote.getSessionId() == null) {
            throw new UnsupportedOperationException("Browser validations require a live WebDriver session.");
        }
        return AutomationCapabilityResolver.forWebDriver(driver);
    }

    private void requireFeature(AutomationFeature feature) {
        capabilities().require(feature);
    }

    private static UnsupportedOperationException unsupported(AutomationFeature feature) {
        return new UnsupportedOperationException("The live browser session does not support " + feature.name() + ".");
    }

    private NativeValidationsBuilder attributeValue(String attribute, String description) {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = attribute;
        reportMessageBuilder.append(description);
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check that the current page matches its visual-regression baseline screenshot
     * (full-page pixel diff via OpenCV). On the first test run this method takes a full-page screenshot
     * and the test passes, saving it as the baseline for subsequent runs. The comparison runs
     * when this terminal assertion is called.
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
