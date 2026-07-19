package com.shaft.gui.playwright.validation;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.assertions.LocatorAssertions;
import com.microsoft.playwright.assertions.PageAssertions;
import com.microsoft.playwright.assertions.PlaywrightAssertions;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.aria.AriaSnapshotHelper;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.gui.internal.image.VisualProcessingProvider;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.tools.io.internal.BrowserPerformanceExecutionReport;
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
import org.apache.logging.log4j.Level;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.function.Supplier;
import java.util.regex.Pattern;

final class PlaywrightValidationsExecutor extends ValidationsExecutor {
    private static final String VISUAL_COMPARISON_ATTACHMENT_NAME = "Visual Comparison";
    private final ValidationEnums.ValidationCategory validationCategory;
    private final PlaywrightSession session;
    private final Locator locator;
    private final String locatorDescription;
    private final ValidationEnums.ValidationType validationType;
    private final String validationMethod;
    private final ValidationEnums.VisualValidationEngine visualValidationEngine;
    private final String elementAttribute;
    private final String elementCssProperty;
    private final String browserAttribute;
    private final ValidationEnums.ValidationComparisonType validationComparisonType;
    private final Object expectedValue;
    private final StringBuilder reportMessageBuilder;
    private final Integer maxDiffPixels;
    private final Double maxDiffPixelRatio;
    private final List<Locator> maskLocators;
    private final String ariaSnapshotFileName;
    private String validationCategoryString;
    private String customReportMessage = "";

    PlaywrightValidationsExecutor(PlaywrightNativeValidationsBuilder builder) {
        super(new SeedBuilder(builder.validationCategory()));
        this.validationCategory = builder.validationCategory();
        this.session = builder.session();
        this.locator = builder.playwrightLocator();
        this.locatorDescription = builder.playwrightLocatorDescription();
        this.validationMethod = builder.validationMethod();
        this.validationType = builder.validationType();
        this.visualValidationEngine = builder.visualValidationEngine();
        this.validationComparisonType = builder.validationComparisonType();
        this.expectedValue = builder.expectedValue();
        this.elementAttribute = builder.playwrightElementAttribute();
        this.elementCssProperty = builder.playwrightElementCssProperty();
        this.browserAttribute = builder.playwrightBrowserAttribute();
        this.reportMessageBuilder = builder.reportMessageBuilder();
        this.maxDiffPixels = null;
        this.maxDiffPixelRatio = null;
        this.maskLocators = List.of();
        this.ariaSnapshotFileName = builder.ariaSnapshotFileName();
    }

    PlaywrightValidationsExecutor(PlaywrightVisualValidationsBuilder builder) {
        super(new SeedBuilder(builder.validationCategory()));
        this.validationCategory = builder.validationCategory();
        this.session = builder.session();
        this.locator = builder.playwrightLocator();
        this.locatorDescription = builder.playwrightLocatorDescription();
        this.validationMethod = builder.pageLevel() ? "pageMatchesScreenshot" : "elementMatchesScreenshot";
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        this.visualValidationEngine = null;
        this.validationComparisonType = null;
        this.expectedValue = null;
        this.elementAttribute = null;
        this.elementCssProperty = null;
        this.browserAttribute = null;
        this.reportMessageBuilder = builder.reportMessageBuilder();
        this.maxDiffPixels = builder.maxDiffPixelsValue();
        this.maxDiffPixelRatio = builder.maxDiffPixelRatioValue();
        this.maskLocators = builder.playwrightMaskLocators();
        this.ariaSnapshotFileName = null;
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
        long start = System.nanoTime();
        try {
            try (ProgressBarLogger ignored = new ProgressBarLogger(progressTaskName)) {
                performPlaywrightValidation();
            }
        } finally {
            BrowserPerformanceExecutionReport.recordBrowserAction(
                    "playwright.validation." + validationMethod,
                    System.nanoTime() - start);
            if (generatedCustomReportMessage) {
                customReportMessage = "";
            }
        }
    }

    @Step(" {this.validationCategoryString} that {this.customReportMessage}")
    private void performPlaywrightValidation() {
        long validationStartTime = System.currentTimeMillis();
        Outcome outcome = evaluate();
        updateAllureParameters(outcome.parameters());
        reportValidationState(outcome, validationStartTime);
    }

    private Outcome evaluate() {
        if ("elementMatches".equals(validationMethod)) {
            return evaluateElementMatches();
        }
        if ("elementMatchesScreenshot".equals(validationMethod) || "pageMatchesScreenshot".equals(validationMethod)) {
            return evaluateMatchesScreenshot();
        }
        if ("elementAriaSnapshotMatches".equals(validationMethod)) {
            return evaluateAriaSnapshot();
        }
        Object reportedExpected = expectedValue;
        Supplier<Object> actualSupplier = this::readActual;
        Runnable assertion = createPlaywrightAssertion();
        if (assertion != null) {
            return runPlaywrightAssertion(assertion, actualSupplier, reportedExpected);
        }
        Object actual = safelyRead(actualSupplier);
        return new Outcome(compare(reportedExpected, actual), reportedExpected, actual,
                commonParameters(reportedExpected, actual), List.of());
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
        return new Outcome(passed, reportedExpected, actual, commonParameters(reportedExpected, actual), List.of());
    }

    private Outcome evaluateElementMatches() {
        boolean expected = validationType.getValue();
        List<List<Object>> visualAttachments = new ArrayList<>();
        byte[] referenceImage = ImageProcessingActions.getReferenceImage(locatorDescription);
        boolean hasReferenceImage = referenceImage != null && referenceImage.length > 0;

        byte[] elementScreenshot = locator.screenshot();
        Boolean actualResult = ImageProcessingActions.compareAgainstBaseline(locatorDescription, elementScreenshot,
                ImageProcessingActions.VisualValidationEngine.valueOf(visualValidationEngine.name()));

        byte[] shutterbugDifferencesImage = new byte[0];
        if (visualValidationEngine.equals(ValidationEnums.VisualValidationEngine.EXACT_SHUTTERBUG)
                && !Boolean.TRUE.equals(actualResult)) {
            shutterbugDifferencesImage = ImageProcessingActions.getShutterbugDifferencesImage(locatorDescription);
        }

        boolean visualComparisonAttached = hasReferenceImage
                && attachVisualComparison(referenceImage, elementScreenshot, shutterbugDifferencesImage);
        boolean actual = Boolean.TRUE.equals(actualResult);
        return new Outcome(expected == actual, expected, actual, commonParameters(expected, actual),
                visualAttachments, visualComparisonAttached);
    }

    private Outcome evaluateMatchesScreenshot() {
        boolean pageLevel = "pageMatchesScreenshot".equals(validationMethod);
        byte[] actualScreenshot = pageLevel
                ? session.page().screenshot(new Page.ScreenshotOptions().setFullPage(true))
                : locator.screenshot();

        String baselineKey = pageLevel ? "page_" + ReportManagerHelper.getCallingMethodFullName() : locatorDescription;
        byte[] baselineImage = ImageProcessingActions.getReferenceImage(baselineKey);
        List<int[]> maskRects = resolveMaskRects();

        VisualProcessingProvider.ScreenshotComparisonResult comparisonResult = ImageProcessingActions
                .compareScreenshotAgainstBaseline(baselineKey, actualScreenshot, maskRects, maxDiffPixels, maxDiffPixelRatio);

        boolean visualComparisonAttached = baselineImage != null
                && attachVisualComparison(baselineImage, actualScreenshot, comparisonResult.diffImage());

        boolean expected = true;
        boolean actual = comparisonResult.matched();
        return new Outcome(expected == actual, expected, actual, commonParameters(expected, actual), List.of(), visualComparisonAttached);
    }

    private Outcome evaluateAriaSnapshot() {
        String actualYaml = AriaSnapshotHelper.captureAriaSnapshot(locator);
        String baselinePath = resolveAriaSnapshotPath(ariaSnapshotFileName);
        boolean updateSnapshots = SHAFT.Properties.visuals.updateSnapshots();
        boolean baselineExists = FileActions.getInstance(true).doesFileExist(baselinePath);
        boolean matched;
        String baselineYaml;
        List<List<Object>> attachments = new ArrayList<>();

        if (!baselineExists || updateSnapshots) {
            FileActions.getInstance(true).writeToFile(baselinePath, actualYaml);
            matched = true;
            baselineYaml = actualYaml;
        } else {
            baselineYaml = FileActions.getInstance(true).readFile(baselinePath);
            var matchResult = AriaSnapshotHelper.match(baselineYaml, actualYaml);
            matched = matchResult.matched();
            if (!matched) {
                attachments.add(List.of("Aria Snapshot Diff", "aria-snapshot-diff.txt", matchResult.diffMessage()));
            }
        }
        attachments.add(List.of("Expected Aria Snapshot", ariaSnapshotFileName + ".yaml", baselineYaml));
        attachments.add(List.of("Actual Aria Snapshot", ariaSnapshotFileName + "_actual.yaml", actualYaml));

        boolean expected = true;
        return new Outcome(expected == matched, expected, matched, commonParameters(expected, matched), attachments, false);
    }

    private static String resolveAriaSnapshotPath(String snapshotFileName) {
        String fileName = snapshotFileName.endsWith(".yaml") || snapshotFileName.endsWith(".yml")
                ? snapshotFileName : snapshotFileName + ".yaml";
        return SHAFT.Properties.paths.ariaSnapshot() + fileName;
    }

    private List<int[]> resolveMaskRects() {
        if (maskLocators == null || maskLocators.isEmpty()) {
            return List.of();
        }
        List<int[]> rects = new ArrayList<>();
        for (Locator mask : maskLocators) {
            var box = mask.boundingBox();
            if (box == null) {
                continue;
            }
            rects.add(new int[]{
                    (int) Math.round(box.x),
                    (int) Math.round(box.y),
                    (int) Math.round(box.width),
                    (int) Math.round(box.height)
            });
        }
        return rects;
    }

    private static boolean attachVisualComparison(byte[] expectedImage, byte[] actualImage, byte[] differenceImage) {
        try {
            var content = new JSONObject()
                    .put("expected", "data:image/png;base64," + Base64.getEncoder().encodeToString(expectedImage))
                    .put("actual", "data:image/png;base64," + Base64.getEncoder().encodeToString(actualImage));
            if (differenceImage != null && differenceImage.length > 0) {
                content.put("diff", "data:image/png;base64," + Base64.getEncoder().encodeToString(differenceImage));
            }
            Allure.addAttachment(VISUAL_COMPARISON_ATTACHMENT_NAME, "application/vnd.allure.image.diff", content.toString());
            return true;
        } catch (JSONException jsonException) {
            ReportManagerHelper.logDiscrete(jsonException, Level.DEBUG);
            return false;
        }
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
            case "alerttext", "alert", "dialogtext" -> session.lastDialogText();
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
            case "elementMatches" -> {
                parameters.put("Should match", String.valueOf(expected));
                parameters.put("Visual engine", visualValidationEngine.name());
                parameters.put("Actual value", String.valueOf(actual));
                return parameters;
            }
            case "elementMatchesScreenshot", "pageMatchesScreenshot" -> {
                parameters.put("Should match", String.valueOf(expected));
                parameters.put("Actual value", String.valueOf(actual));
                return parameters;
            }
            case "elementAriaSnapshotMatches" -> {
                parameters.put("Snapshot file", ariaSnapshotFileName);
                parameters.put("Should match", String.valueOf(expected));
                parameters.put("Actual value", String.valueOf(actual));
                return parameters;
            }
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

    private void reportValidationState(Outcome outcome, long validationStartTime) {
        ValidationsHelper.reportValidationState(validationCategory, outcome.passed(), outcome.expected(),
                outcome.actual(), attachments(outcome), validationStartTime, outcome.visualComparisonAttached());
    }

    private List<List<Object>> attachments(Outcome outcome) {
        List<List<Object>> attachments = new ArrayList<>(outcome.attachments());
        if (!outcome.visualComparisonAttached() && shouldAttachScreenshot(outcome.passed())) {
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
        if (shouldAttachPageSnapshot(outcome.passed())) {
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
                           LinkedHashMap<String, String> parameters,
                           List<List<Object>> attachments,
                           boolean visualComparisonAttached) {
        private Outcome(boolean passed, Object expected, Object actual, LinkedHashMap<String, String> parameters,
                        List<List<Object>> attachments) {
            this(passed, expected, actual, parameters, attachments, false);
        }
    }

    private static final class SeedBuilder extends ValidationsBuilder {
        SeedBuilder(ValidationEnums.ValidationCategory validationCategory) {
            super(validationCategory);
        }
    }
}
