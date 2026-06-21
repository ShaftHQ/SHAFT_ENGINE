package com.shaft.gui.playwright.validation;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.assertions.LocatorAssertions;
import com.microsoft.playwright.assertions.PageAssertions;
import com.microsoft.playwright.assertions.PlaywrightAssertions;
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ProgressBarLogger;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.ValidationsBuilder;
import com.shaft.validation.internal.ValidationsExecutor;
import com.shaft.validation.internal.ValidationsHelper;
import io.qameta.allure.Allure;
import io.qameta.allure.Step;
import io.qameta.allure.model.Parameter;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.function.Supplier;
import java.util.regex.Pattern;

final class PlaywrightValidationsExecutor extends ValidationsExecutor {
    private final ValidationEnums.ValidationCategory validationCategory;
    private final PlaywrightSession session;
    private final Locator locator;
    private final ValidationEnums.ValidationType validationType;
    private final String validationMethod;
    private final String elementAttribute;
    private final String elementCssProperty;
    private final String browserAttribute;
    private final ValidationEnums.ValidationComparisonType validationComparisonType;
    private final Object expectedValue;
    private final StringBuilder reportMessageBuilder;
    private String validationCategoryString;
    private String customReportMessage = "";

    PlaywrightValidationsExecutor(PlaywrightNativeValidationsBuilder builder) {
        super(new SeedBuilder(builder.validationCategory()));
        this.validationCategory = builder.validationCategory();
        this.session = builder.session();
        this.locator = builder.playwrightLocator();
        this.validationMethod = builder.validationMethod();
        this.validationType = builder.validationType();
        this.validationComparisonType = builder.validationComparisonType();
        this.expectedValue = builder.expectedValue();
        this.elementAttribute = builder.playwrightElementAttribute();
        this.elementCssProperty = builder.playwrightElementCssProperty();
        this.browserAttribute = builder.playwrightBrowserAttribute();
        this.reportMessageBuilder = builder.reportMessageBuilder();
    }

    @Override
    public ValidationsExecutor withCustomReportMessage(String customReportMessage) {
        this.customReportMessage = customReportMessage;
        return this;
    }

    @Override
    public void perform() {
        ReportManager.log(customReportMessage);
    }

    @Override
    public void internalPerform() {
        boolean generatedCustomReportMessage = false;
        if (customReportMessage.isBlank()) {
            customReportMessage = reportMessageBuilder.toString();
            generatedCustomReportMessage = true;
        }
        validationCategoryString = validationCategory == ValidationEnums.ValidationCategory.HARD_ASSERT ? "Assert" : "Verify";
        ReportManager.logDiscrete(validationCategoryString + " that " + customReportMessage);
        String progressTaskName = validationCategoryString.equals("Assert") ? "Asserting..." : "Verifying...";
        try {
            try (ProgressBarLogger ignored = new ProgressBarLogger(progressTaskName)) {
                performPlaywrightValidation();
            }
        } finally {
            if (generatedCustomReportMessage) {
                customReportMessage = "";
            }
        }
    }

    @Step(" {this.validationCategoryString} that {this.customReportMessage}")
    private void performPlaywrightValidation() {
        Outcome outcome = evaluate();
        updateAllureParameters(outcome.parameters());
        reportValidationState(outcome);
    }

    private Outcome evaluate() {
        Object reportedExpected = expectedValue;
        Supplier<Object> actualSupplier = this::readActual;
        Runnable assertion = createPlaywrightAssertion();
        if (assertion != null) {
            return runPlaywrightAssertion(assertion, actualSupplier, reportedExpected);
        }
        Object actual = safelyRead(actualSupplier);
        return new Outcome(compare(reportedExpected, actual), reportedExpected, actual,
                commonParameters(reportedExpected, actual));
    }

    private Runnable createPlaywrightAssertion() {
        return switch (validationMethod) {
            case "elementExists" -> () -> locatorAssertions().isAttached();
            case "elementSelected" -> () -> locatorAssertions().hasJSProperty("selected", true);
            case "elementChecked" -> () -> locatorAssertions().isChecked();
            case "elementVisible" -> () -> locatorAssertions().isVisible();
            case "elementEnabled" -> () -> locatorAssertions().isEnabled();
            case "elementAttributeEquals", "elementDomAttributeEquals" -> elementAttributeAssertion();
            case "elementDomPropertyEquals", "elementPropertyEquals" -> elementPropertyAssertion();
            case "elementCssPropertyEquals" -> elementCssAssertion();
            case "browserAttributeEquals" -> browserAssertion();
            default -> null;
        };
    }

    private Runnable elementAttributeAssertion() {
        if ("text".equalsIgnoreCase(elementAttribute)) {
            return textAssertion(locator);
        }
        if (expectedValue == null) {
            return null;
        }
        return switch (validationComparisonType) {
            case EQUALS -> () -> locatorAssertions().hasAttribute(elementAttribute, String.valueOf(expectedValue));
            case CONTAINS -> () -> locatorAssertions().hasAttribute(elementAttribute, containsPattern(expectedValue));
            case MATCHES -> () -> locatorAssertions().hasAttribute(elementAttribute, Pattern.compile(String.valueOf(expectedValue)));
            case CASE_INSENSITIVE -> () -> locatorAssertions().hasAttribute(elementAttribute, equalsIgnoreCasePattern(expectedValue));
        };
    }

    private Runnable elementPropertyAssertion() {
        if (validationComparisonType != ValidationEnums.ValidationComparisonType.EQUALS) {
            return null;
        }
        return () -> locatorAssertions().hasJSProperty(elementAttribute, expectedValue);
    }

    private Runnable elementCssAssertion() {
        if (expectedValue == null) {
            return null;
        }
        return switch (validationComparisonType) {
            case EQUALS -> () -> locatorAssertions().hasCSS(elementCssProperty, String.valueOf(expectedValue));
            case CONTAINS -> () -> locatorAssertions().hasCSS(elementCssProperty, containsPattern(expectedValue));
            case MATCHES -> () -> locatorAssertions().hasCSS(elementCssProperty, Pattern.compile(String.valueOf(expectedValue)));
            case CASE_INSENSITIVE -> () -> locatorAssertions().hasCSS(elementCssProperty, equalsIgnoreCasePattern(expectedValue));
        };
    }

    private Runnable browserAssertion() {
        String normalizedAttribute = browserAttribute.toLowerCase(Locale.ROOT);
        return switch (normalizedAttribute) {
            case "currenturl", "pageurl", "windowurl", "url" -> pageAssertion(true);
            case "title", "windowtitle", "pagetitle" -> pageAssertion(false);
            case "text", "pagetext", "windowtext" -> textAssertion(session.page().locator("body"));
            default -> null;
        };
    }

    private Runnable pageAssertion(boolean url) {
        if (expectedValue == null) {
            return null;
        }
        PageAssertions assertions = pageAssertions();
        return switch (validationComparisonType) {
            case EQUALS -> () -> {
                if (url) {
                    assertions.hasURL(String.valueOf(expectedValue));
                } else {
                    assertions.hasTitle(String.valueOf(expectedValue));
                }
            };
            case CONTAINS -> () -> {
                Pattern pattern = containsPattern(expectedValue);
                if (url) {
                    assertions.hasURL(pattern);
                } else {
                    assertions.hasTitle(pattern);
                }
            };
            case MATCHES -> () -> {
                Pattern pattern = Pattern.compile(String.valueOf(expectedValue));
                if (url) {
                    assertions.hasURL(pattern);
                } else {
                    assertions.hasTitle(pattern);
                }
            };
            case CASE_INSENSITIVE -> () -> {
                Pattern pattern = equalsIgnoreCasePattern(expectedValue);
                if (url) {
                    assertions.hasURL(pattern);
                } else {
                    assertions.hasTitle(pattern);
                }
            };
        };
    }

    private Runnable textAssertion(Locator textLocator) {
        if (expectedValue == null || "textTrimmed".equalsIgnoreCase(elementAttribute)) {
            return null;
        }
        return switch (validationComparisonType) {
            case EQUALS -> () -> locatorAssertions(textLocator).hasText(String.valueOf(expectedValue));
            case CONTAINS -> () -> locatorAssertions(textLocator).containsText(String.valueOf(expectedValue));
            case MATCHES -> () -> locatorAssertions(textLocator).hasText(Pattern.compile(String.valueOf(expectedValue)));
            case CASE_INSENSITIVE -> () -> locatorAssertions(textLocator).hasText(equalsIgnoreCasePattern(expectedValue));
        };
    }

    private Outcome runPlaywrightAssertion(Runnable assertion, Supplier<Object> actualSupplier, Object reportedExpected) {
        boolean passed = true;
        try {
            assertion.run();
        } catch (AssertionError | RuntimeException e) {
            passed = false;
        }
        Object actual = safelyRead(actualSupplier);
        return new Outcome(passed, reportedExpected, actual, commonParameters(reportedExpected, actual));
    }

    private Object readActual() {
        return switch (validationMethod) {
            case "elementExists" -> locator.count() > 0;
            case "elementSelected" -> locator.evaluate("element => !!element.selected");
            case "elementChecked" -> locator.isChecked();
            case "elementVisible" -> locator.isVisible();
            case "elementEnabled" -> locator.isEnabled();
            case "elementAttributeEquals" -> locator.getAttribute(elementAttribute);
            case "elementDomAttributeEquals" -> readElementDomAttribute();
            case "elementDomPropertyEquals", "elementPropertyEquals" ->
                    locator.evaluate("(element, property) => element[property]", elementAttribute);
            case "elementCssPropertyEquals" ->
                    locator.evaluate("(element, property) => getComputedStyle(element).getPropertyValue(property)",
                            elementCssProperty);
            case "browserAttributeEquals" -> readBrowserAttribute();
            case "playwrightUnsupportedVisualValidation" -> "Playwright visual reference validation is not implemented yet";
            default -> null;
        };
    }

    private Object readElementDomAttribute() {
        if ("text".equalsIgnoreCase(elementAttribute)) {
            return locator.textContent();
        }
        if ("textTrimmed".equalsIgnoreCase(elementAttribute)) {
            String text = locator.textContent();
            return text == null ? null : text.trim();
        }
        return locator.getAttribute(elementAttribute);
    }

    private Object readBrowserAttribute() {
        Page page = session.page();
        return switch (browserAttribute.toLowerCase(Locale.ROOT)) {
            case "currenturl", "pageurl", "windowurl", "url" -> page.url();
            case "pagesource", "windowsource", "source" -> page.content();
            case "title", "windowtitle", "pagetitle" -> page.title();
            case "text", "pagetext", "windowtext" ->
                    page.evaluate("() => document.body ? document.body.innerText : ''");
            case "useragent", "user-agent", "navigator.useragent" -> page.evaluate("() => navigator.userAgent");
            case "windowwidth", "innerwidth", "viewportwidth", "width" -> page.evaluate("() => window.innerWidth");
            case "windowheight", "innerheight", "viewportheight", "height" -> page.evaluate("() => window.innerHeight");
            case "devicepixelratio", "device-scale-factor", "devicescalefactor" ->
                    page.evaluate("() => window.devicePixelRatio");
            case "windowhandle", "pagehandle", "handle" -> session.pageHandle(page);
            case "windowsize", "pagesize", "size" ->
                    page.evaluate("() => `${window.innerWidth}x${window.innerHeight}`");
            default -> page.evaluate("(attribute) => document.documentElement.getAttribute(attribute)", browserAttribute);
        };
    }

    private Object safelyRead(Supplier<Object> actualSupplier) {
        try {
            return actualSupplier.get();
        } catch (RuntimeException e) {
            ReportManagerHelper.logDiscrete(e);
            return e.getMessage();
        }
    }

    private boolean compare(Object expected, Object actual) {
        return JavaHelper.compareTwoObjects(expected, actual, validationComparisonType.getValue(), validationType.getValue()) == 1;
    }

    private LocatorAssertions locatorAssertions() {
        return locatorAssertions(locator);
    }

    private LocatorAssertions locatorAssertions(Locator targetLocator) {
        LocatorAssertions assertions = PlaywrightAssertions.assertThat(targetLocator);
        return validationType == ValidationEnums.ValidationType.POSITIVE ? assertions : assertions.not();
    }

    private PageAssertions pageAssertions() {
        PageAssertions assertions = PlaywrightAssertions.assertThat(session.page());
        return validationType == ValidationEnums.ValidationType.POSITIVE ? assertions : assertions.not();
    }

    private Pattern containsPattern(Object value) {
        return Pattern.compile(".*" + regexLiteral(value) + ".*", Pattern.DOTALL);
    }

    private Pattern equalsIgnoreCasePattern(Object value) {
        return Pattern.compile("^" + regexLiteral(value) + "$",
                Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    }

    private String regexLiteral(Object value) {
        return String.valueOf(value).replaceAll("([\\\\.\\[\\]{}()*+\\-?^$|])", "\\\\$1");
    }

    private LinkedHashMap<String, String> commonParameters(Object expected, Object actual) {
        String comparisonType = validationType == ValidationEnums.ValidationType.NEGATIVE && !reportsAdjustedExpected(expected)
                ? "not " + validationComparisonType.name()
                : validationComparisonType.name();
        LinkedHashMap<String, String> parameters = new LinkedHashMap<>();
        if (locator != null) {
            parameters.put("Locator", String.valueOf(locator));
        }
        switch (validationMethod) {
            case "elementExists" -> {
                parameters.put("Should exist", String.valueOf(expected));
                parameters.put("Actual value", String.valueOf(actual));
                return parameters;
            }
            case "elementAttributeEquals" -> parameters.put("Attribute", elementAttribute);
            case "elementDomAttributeEquals" -> parameters.put("DOM Attribute", elementAttribute);
            case "elementDomPropertyEquals", "elementPropertyEquals" -> parameters.put("DOM Property", elementAttribute);
            case "elementSelected" -> parameters.put("DOM Property", "selected");
            case "elementChecked" -> parameters.put("DOM Property", "checked");
            case "elementVisible" -> parameters.put("State", "visible");
            case "elementEnabled" -> parameters.put("State", "enabled");
            case "elementCssPropertyEquals" -> parameters.put("CSS Property", elementCssProperty);
            case "browserAttributeEquals" -> parameters.put("Attribute", browserAttribute);
            default -> {
            }
        }
        parameters.put("Expected value", String.valueOf(expected));
        parameters.put("Comparison type", JavaHelper.convertToSentenceCase(comparisonType));
        parameters.put("Actual value", String.valueOf(actual));
        return parameters;
    }

    private boolean reportsAdjustedExpected(Object expected) {
        return expected instanceof Boolean && List.of(
                "elementExists",
                "elementSelected",
                "elementChecked",
                "elementVisible",
                "elementEnabled").contains(validationMethod);
    }

    private void updateAllureParameters(LinkedHashMap<String, String> parameters) {
        List<Parameter> allureParameters = new ArrayList<>();
        parameters.forEach((key, value) -> allureParameters.add(new Parameter()
                .setName(key)
                .setValue(String.valueOf(value))
                .setMode(Parameter.Mode.DEFAULT)));
        Allure.getLifecycle().updateStep(stepResult -> stepResult.setParameters(allureParameters));
    }

    private void reportValidationState(Outcome outcome) {
        ValidationsHelper.reportValidationState(validationCategory, outcome.passed(), outcome.expected(),
                outcome.actual(), attachments(outcome.passed()));
    }

    private List<List<Object>> attachments(boolean passed) {
        List<List<Object>> attachments = new ArrayList<>();
        if (shouldAttachScreenshot(passed)) {
            try {
                byte[] screenshot = session.page().screenshot(new Page.ScreenshotOptions().setFullPage(true));
                List<Object> screenshotAttachment = new ScreenshotManager()
                        .prepareImageForReport(screenshot, validationCategoryString);
                if (screenshotAttachment != null && !screenshotAttachment.isEmpty()) {
                    attachments.add(screenshotAttachment);
                }
            } catch (RuntimeException e) {
                ReportManagerHelper.logDiscrete(e);
            }
        }
        if (shouldAttachPageSnapshot(passed)) {
            try {
                attachments.add(List.of(validationCategoryString, "page HTML", session.page().content()));
            } catch (RuntimeException e) {
                ReportManagerHelper.logDiscrete(e);
            }
        }
        return attachments;
    }

    private boolean shouldAttachScreenshot(boolean passed) {
        if (!passed) {
            return true;
        }
        String policy = SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot().toLowerCase(Locale.ROOT);
        return List.of("always", "validationpointsonly").contains(policy);
    }

    private boolean shouldAttachPageSnapshot(boolean passed) {
        if (!passed) {
            return true;
        }
        String policy = SHAFT.Properties.visuals.whenToTakePageSourceSnapshot().toLowerCase(Locale.ROOT);
        return List.of("always", "validationpointsonly").contains(policy);
    }

    private record Outcome(boolean passed,
                           Object expected,
                           Object actual,
                           LinkedHashMap<String, String> parameters) {
    }

    private static final class SeedBuilder extends ValidationsBuilder {
        SeedBuilder(ValidationEnums.ValidationCategory validationCategory) {
            super(validationCategory);
        }
    }
}
