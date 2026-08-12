package com.shaft.gui.playwright.validation;

import com.microsoft.playwright.Locator;
import com.shaft.gui.capabilities.AutomationCapabilities;
import com.shaft.gui.capabilities.AutomationFeature;
import com.shaft.gui.capabilities.internal.AutomationCapabilityResolver;
import com.shaft.gui.driver.BrowserAssertions;
import com.shaft.gui.driver.BrowserConsoleMessage;
import com.shaft.gui.browser.internal.PlaywrightNetworkInterceptor;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.VisualComparisonOptions;
import com.shaft.validation.internal.NativeValidationsBuilder;
import com.shaft.validation.internal.ValidationsExecutor;

import java.util.Objects;
import java.util.function.Supplier;

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
    public NativeValidationsBuilder pageSourceValue() {
        reportMessageBuilder.append("page source ");
        return builder("pagesource");
    }

    @Override
    public NativeValidationsBuilder windowHandleValue() {
        reportMessageBuilder.append("window handle ");
        return builder("windowhandle");
    }

    @Override
    public NativeValidationsBuilder windowPositionValue() {
        reportMessageBuilder.append("window position ");
        return builder("windowposition");
    }

    @Override
    public NativeValidationsBuilder windowSizeValue() {
        reportMessageBuilder.append("window size ");
        return builder("windowsize");
    }

    @Override
    public NativeValidationsBuilder browsingContextCountValue() {
        reportMessageBuilder.append("browsing context count ");
        return builder("browsingcontextcount");
    }

    @Override
    public NativeValidationsBuilder dialogPresentValue() {
        return value("dialog observed state", () -> {
            requireFeature(AutomationFeature.BROWSER_AUTOMATION);
            return session.isDialogSeen();
        });
    }

    @Override
    public NativeValidationsBuilder downloadCountValue() {
        return value("download count", () -> {
            requireFeature(AutomationFeature.DOWNLOADS);
            return session.downloadSnapshot().size();
        });
    }

    @Override
    public NativeValidationsBuilder networkObservationCountValue() {
        return value("network observation count", () -> {
            requireFeature(AutomationFeature.NETWORK_OBSERVATION);
            PlaywrightNetworkInterceptor interceptor = session.networkInterceptor();
            if (interceptor == null) {
                throw new UnsupportedOperationException(
                        "No retained network-observation state exists for the live Playwright session.");
            }
            return interceptor.observationCount();
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
        return new PlaywrightNativeValidationsBuilder(validationCategory, session, reader, name,
                new StringBuilder("the browser ").append(name).append(' '));
    }

    private java.util.List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> consoleSnapshot() {
        requireFeature(AutomationFeature.CONSOLE_LOGS);
        return session.consoleSnapshot();
    }

    private AutomationCapabilities capabilities() {
        AutomationCapabilities capabilities = AutomationCapabilityResolver.forPlaywright(session);
        if (capabilities.backend() == com.shaft.gui.capabilities.AutomationBackend.UNKNOWN) {
            throw new UnsupportedOperationException("Browser validations require a live Playwright session.");
        }
        return capabilities;
    }

    private void requireFeature(AutomationFeature feature) {
        capabilities().require(feature);
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
        return new PlaywrightNativeValidationsBuilder(validationCategory, session, (Locator) null, null, "browserAttributeEquals",
                null, null, browserAttribute, reportMessageBuilder);
    }
}
