package com.shaft.validation.internal;

import com.shaft.validation.ValidationEnums;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.util.function.Supplier;

public class NativeValidationsBuilder {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected final String validationMethod;
    protected final StringBuilder reportMessageBuilder;
    protected WebDriver driver;
    protected By locator;
    protected ValidationEnums.ValidationType validationType;
    protected String elementAttribute;
    protected String elementCssProperty;
    protected String browserAttribute;
    protected ValidationEnums.ValidationComparisonType validationComparisonType;
    protected Object expectedValue;
    protected Object actualValue;
    protected Object response;
    protected String jsonPath;
    protected String folderRelativePath;
    protected String fileName;
    protected boolean jsonIgnoringOrderComparison;
    protected Supplier<Object> mobileValueReader;
    protected String mobileValueName;
    protected Supplier<Object> browserValueReader;
    protected String browserValueName;
    protected Supplier<Object> apiValueReader;
    protected String apiValueDescription;

    public NativeValidationsBuilder(WebDriverElementValidationsBuilder webDriverElementValidationsBuilder) {
        this.validationCategory = webDriverElementValidationsBuilder.validationCategory;
        this.driver = webDriverElementValidationsBuilder.driver;
        this.locator = webDriverElementValidationsBuilder.locator;
        this.validationMethod = webDriverElementValidationsBuilder.validationMethod;
        this.elementAttribute = webDriverElementValidationsBuilder.elementAttribute;
        this.elementCssProperty = webDriverElementValidationsBuilder.elementCssProperty;

        this.reportMessageBuilder = webDriverElementValidationsBuilder.reportMessageBuilder;
    }

    public NativeValidationsBuilder(WebDriverBrowserValidationsBuilder webDriverBrowserValidationsBuilder) {
        this.validationCategory = webDriverBrowserValidationsBuilder.validationCategory;
        this.driver = webDriverBrowserValidationsBuilder.driver;
        this.validationMethod = webDriverBrowserValidationsBuilder.validationMethod;
        this.browserAttribute = webDriverBrowserValidationsBuilder.browserAttribute;

        this.reportMessageBuilder = webDriverBrowserValidationsBuilder.reportMessageBuilder;
    }

    public NativeValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, WebDriver driver,
                                    Supplier<Object> mobileValueReader, String mobileValueName,
                                    StringBuilder reportMessageBuilder) {
        this.validationCategory = validationCategory;
        this.driver = driver;
        this.validationMethod = "mobileValueEquals";
        this.mobileValueReader = mobileValueReader;
        this.mobileValueName = mobileValueName;
        this.reportMessageBuilder = reportMessageBuilder;
    }

    static NativeValidationsBuilder browserValue(ValidationEnums.ValidationCategory validationCategory,
                                                  WebDriver driver, Supplier<Object> reader, String name,
                                                  StringBuilder reportMessageBuilder) {
        NativeValidationsBuilder builder = new NativeValidationsBuilder(
                validationCategory, "browserValueEquals", reportMessageBuilder);
        builder.driver = driver;
        builder.browserValueReader = reader;
        builder.browserValueName = name;
        return builder;
    }

    static NativeValidationsBuilder apiValue(ValidationEnums.ValidationCategory validationCategory,
                                              String description, Supplier<Object> reader,
                                              StringBuilder reportMessageBuilder) {
        NativeValidationsBuilder builder = new NativeValidationsBuilder(
                validationCategory, "apiValueEquals", reportMessageBuilder);
        builder.apiValueReader = reader;
        builder.apiValueDescription = description;
        return builder;
    }

    private NativeValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, String validationMethod,
                                     StringBuilder reportMessageBuilder) {
        this.validationCategory = validationCategory;
        this.validationMethod = validationMethod;
        this.reportMessageBuilder = reportMessageBuilder;
    }

    public NativeValidationsBuilder(ValidationsBuilder validationsBuilder) {
        this.validationCategory = validationsBuilder.validationCategory;
        this.validationMethod = validationsBuilder.validationMethod;
        this.actualValue = validationsBuilder.actualValue;

        this.reportMessageBuilder = validationsBuilder.reportMessageBuilder;
    }

    public NativeValidationsBuilder(RestValidationsBuilder restValidationsBuilder) {
        this.validationCategory = restValidationsBuilder.validationCategory;
        this.validationMethod = restValidationsBuilder.validationMethod;
        this.jsonPath = restValidationsBuilder.jsonPath;
        this.response = restValidationsBuilder.response;

        this.reportMessageBuilder = restValidationsBuilder.reportMessageBuilder;
    }

    public NativeValidationsBuilder(FileValidationsBuilder fileValidationsBuilder) {
        this.validationCategory = fileValidationsBuilder.validationCategory;
        this.validationMethod = fileValidationsBuilder.validationMethod;
        this.folderRelativePath = fileValidationsBuilder.folderRelativePath;
        this.fileName = fileValidationsBuilder.fileName;

        this.reportMessageBuilder = fileValidationsBuilder.reportMessageBuilder;
    }

    private NativeValidationsBuilder(NativeValidationsBuilder sourceBuilder, String validationMethod, String elementAttribute, String browserAttribute) {
        this.validationCategory = sourceBuilder.validationCategory;
        this.driver = sourceBuilder.driver;
        this.locator = sourceBuilder.locator;
        this.validationMethod = validationMethod;
        this.elementAttribute = elementAttribute;
        this.browserAttribute = browserAttribute;
        this.jsonIgnoringOrderComparison = sourceBuilder.jsonIgnoringOrderComparison;
        this.reportMessageBuilder = sourceBuilder.reportMessageBuilder;
    }

    /**
     * Use this to check that the actual object is equal to the expected value
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object retained for source compatibility
     */
    public ValidationsExecutor isEqualTo(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("is equal to \"").append(reportedValue(expectedValue)).append("\".");
        var executor = createExecutor();
        executor.internalPerform();
        return executor;
    }

    /**
     * Overrides the default object method equals and runs the equivalent object comparison.
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return boolean value true if passed and throws AssertionError if failed (return value can be safely ignored)
     */
    @SuppressWarnings("EqualsWhichDoesntCheckParameterClass")
    @Override
    public boolean equals(Object expectedValue) {
        isEqualTo(expectedValue).perform();
        return true;
    }

    /**
     * Use this to check that the actual object is not equal to the expected value
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object retained for source compatibility
     */
    public ValidationsExecutor doesNotEqual(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("does not equal \"").append(reportedValue(expectedValue)).append("\".");
        var executor = createExecutor();
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the actual object contains the expected value
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object retained for source compatibility
     */
    public ValidationsExecutor contains(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("contains \"").append(reportedValue(expectedValue)).append("\".");
        var executor = createExecutor();
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the actual object does not contain the expected value
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object retained for source compatibility
     */
    public ValidationsExecutor doesNotContain(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CONTAINS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("does not contain \"").append(reportedValue(expectedValue)).append("\".");
        var executor = createExecutor();
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the actual object matches the expected regular expression
     *
     * @param expectedValue the test data / expected regular expression for the object under test
     * @return a ValidationsExecutor object retained for source compatibility
     */
    public ValidationsExecutor matchesRegex(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.MATCHES;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("matches this regular expression \"").append(reportedValue(expectedValue)).append("\".");
        var executor = createExecutor();
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the actual object does not match the expected regular expression
     *
     * @param expectedValue the test data / expected regular expression for the object under test
     * @return a ValidationsExecutor object retained for source compatibility
     */
    public ValidationsExecutor doesNotMatchRegex(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.MATCHES;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("does not match this regular expression \"").append(reportedValue(expectedValue)).append("\".");
        var executor = createExecutor();
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the actual object is equal to the expected value (ignoring case sensitivity)
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object retained for source compatibility
     */
    public ValidationsExecutor equalsIgnoringCaseSensitivity(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CASE_INSENSITIVE;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("equals \"").append(reportedValue(expectedValue)).append("\", ignoring case sensitivity.");
        var executor = createExecutor();
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the actual object is not equal to the expected value (ignoring case sensitivity)
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object retained for source compatibility
     */
    public ValidationsExecutor doesNotEqualIgnoringCaseSensitivity(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.CASE_INSENSITIVE;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("does not equal \"").append(reportedValue(expectedValue)).append("\", ignoring case sensitivity.");
        var executor = createExecutor();
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the actual object is null
     *
     * @return a ValidationsExecutor object retained for source compatibility
     */
    public ValidationsExecutor isNull() {
        this.expectedValue = null;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("is NULL.");
        var executor = createExecutor();
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the actual object is not null
     *
     * @return a ValidationsExecutor object retained for source compatibility
     */
    public ValidationsExecutor isNotNull() {
        this.expectedValue = null;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("is not NULL.");
        var executor = createExecutor();
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the actual object is true
     *
     * @return a ValidationsExecutor object retained for source compatibility
     */
    public ValidationsExecutor isTrue() {
        this.expectedValue = true;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("is TRUE.");
        var executor = createExecutor();
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the actual object is false
     *
     * @return a ValidationsExecutor object retained for source compatibility
     */
    public ValidationsExecutor isFalse() {
        this.expectedValue = false;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("is FALSE.");
        var executor = createExecutor();
        executor.internalPerform();
        return executor;
    }

    private Object reportedValue(Object value) {
        return apiValueReader == null ? value : ValidationsHelper.apiValueSummary(value);
    }

    /**
     * Use this under text assertions to validate text language.
     *
     * @return a TextLanguageValidationsBuilder object to continue building language validations
     * @throws IllegalStateException if called outside text-based assertions
     */
    public TextLanguageValidationsBuilder language() {
        ensureTextAssertionContext("language");
        reportMessageBuilder.append("language ");
        return new TextLanguageValidationsBuilder(this);
    }

    /**
     * Use this under text assertions to validate text direction as LTR/RTL.
     *
     * @return a TextDirectionValidationsBuilder object to continue building direction validations
     * @throws IllegalStateException if called outside text-based assertions
     */
    public TextDirectionValidationsBuilder direction() {
        return buildTextDirectionBuilder("textDirection", "direction", "direction");
    }

    /**
     * Use this under text assertions to validate text alignment as LTR/RTL.
     *
     * @return a TextDirectionValidationsBuilder object to continue building alignment validations
     * @throws IllegalStateException if called outside text-based assertions
     */
    public TextDirectionValidationsBuilder alignment() {
        return buildTextDirectionBuilder("textAlignment", "alignment", "alignment");
    }

    /**
     * Use this under text assertions to validate text orientation as LTR/RTL.
     *
     * @return a TextDirectionValidationsBuilder object to continue building orientation validations
     * @throws IllegalStateException if called outside text-based assertions
     */
    public TextDirectionValidationsBuilder orientation() {
        return buildTextDirectionBuilder("textOrientation", "orientation", "orientation");
    }

    /**
     * Use this under text assertions to validate text display style as LTR/RTL.
     *
     * @return a TextDirectionValidationsBuilder object to continue building display style validations
     * @throws IllegalStateException if called outside text-based assertions
     */
    public TextDirectionValidationsBuilder displayStyle() {
        return buildTextDirectionBuilder("textDisplayStyle", "display style", "displayStyle");
    }

    /**
     * Convenience method that validates Arabic text characters and RTL direction.
     *
     * @return a ValidationsExecutor object retained for source compatibility
     * @throws IllegalStateException if called outside text-based assertions
     */
    public ValidationsExecutor isArabic() {
        ensureTextAssertionContext("isArabic");
        language().is(ValidationEnums.TextLanguage.ARABIC);
        return direction().is(ValidationEnums.TextDirection.RTL);
    }

    private void ensureTextAssertionContext(String apiName) {
        boolean isElementTextAssertion = locator != null
                && "elementDomAttributeEquals".equals(validationMethod)
                && "text".equalsIgnoreCase(elementAttribute);
        boolean isBrowserTextAssertion = browserAttribute != null && "text".equalsIgnoreCase(browserAttribute);
        if (!(isElementTextAssertion || isBrowserTextAssertion)) {
            throw new IllegalStateException("The \"" + apiName + "\" assertion can only be called after text(), e.g. assertThat(...).text()." + apiName + "().");
        }
    }

    private TextDirectionValidationsBuilder buildTextDirectionBuilder(String metadataAttribute, String reportLabel, String apiName) {
        ensureTextAssertionContext(apiName);
        reportMessageBuilder.append(reportLabel).append(" ");
        NativeValidationsBuilder metadataBuilder;
        if (locator != null) {
            metadataBuilder = new NativeValidationsBuilder(this, "elementDomAttributeEquals", metadataAttribute, null);
        } else {
            metadataBuilder = new NativeValidationsBuilder(this, "browserAttributeEquals", null, metadataAttribute);
        }
        return new TextDirectionValidationsBuilder(metadataBuilder);
    }

    protected ValidationsExecutor createExecutor() {
        return new ValidationsExecutor(this);
    }
}
