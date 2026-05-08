package com.shaft.validation.internal;

import com.shaft.validation.ValidationEnums;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

/**
 * Coverage-focused unit tests for {@link WebDriverElementValidationsBuilder}.
 * These tests exercise builder configuration paths without requiring a live browser session.
 */
public class WebDriverElementValidationsBuilderCoverageUnitTest {
    private static final By SAMPLE_LOCATOR = By.id("sample");

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test(description = "attribute/dom/css/text builder entry points should set expected state")
    public void nativeBuilderEntryPointsShouldSetExpectedState() {
        assertNativeState(newBuilder().attribute("type"), "elementAttributeEquals", "type", null);
        assertNativeState(newBuilder().domAttribute("aria-label"), "elementDomAttributeEquals", "aria-label", null);
        assertNativeState(newBuilder().domProperty("value"), "elementDomPropertyEquals", "value", null);
        assertNativeState(newBuilder().property("checked"), "elementPropertyEquals", "checked", null);
        assertNativeState(newBuilder().text(), "elementDomAttributeEquals", "text", null);
        assertNativeState(newBuilder().textTrimmed(), "elementDomAttributeEquals", "textTrimmed", null);
        assertNativeState(newBuilder().cssProperty("color"), "elementCssPropertyEquals", null, "color");
    }

    @Test(description = "existence and image methods should configure state before execution")
    public void executorExistenceAndImageMethodsShouldConfigureStateBeforeExecution() {
        assertExecutorInvocationState(newBuilder(), WebDriverElementValidationsBuilder::exists,
                ValidationEnums.ValidationType.POSITIVE, "elementExists", null, null, "exists.");
        assertExecutorInvocationState(newBuilder(), WebDriverElementValidationsBuilder::doesNotExist,
                ValidationEnums.ValidationType.NEGATIVE, "elementExists", null, null, "does not exist.");
        assertExecutorInvocationState(newBuilder(), WebDriverElementValidationsBuilder::matchesReferenceImage,
                ValidationEnums.ValidationType.POSITIVE, "elementMatches",
                ValidationEnums.VisualValidationEngine.EXACT_SHUTTERBUG, null, "matches the reference image");
        assertExecutorInvocationState(newBuilder(),
                builder -> builder.matchesReferenceImage(ValidationEnums.VisualValidationEngine.EXACT_OPENCV),
                ValidationEnums.ValidationType.POSITIVE, "elementMatches",
                ValidationEnums.VisualValidationEngine.EXACT_OPENCV, null, "matches the reference image");
        assertExecutorInvocationState(newBuilder(), WebDriverElementValidationsBuilder::doesNotMatchReferenceImage,
                ValidationEnums.ValidationType.NEGATIVE, "elementMatches",
                ValidationEnums.VisualValidationEngine.EXACT_OPENCV, null, "does not match the reference image");
        assertExecutorInvocationState(newBuilder(),
                builder -> builder.doesNotMatchReferenceImage(ValidationEnums.VisualValidationEngine.EXACT_EYES),
                ValidationEnums.ValidationType.NEGATIVE, "elementMatches",
                ValidationEnums.VisualValidationEngine.EXACT_EYES, null, "does not match the reference image");
    }

    @Test(description = "boolean-like element helpers should configure selected/checked/hidden/disabled assertions")
    public void executorBooleanLikeHelpersShouldConfigureStateBeforeExecution() {
        assertExecutorInvocationState(newBuilder(), WebDriverElementValidationsBuilder::isSelected,
                null, "elementDomAttributeEquals", null, "selected", "is selected; selected attribute");
        assertExecutorInvocationState(newBuilder(), WebDriverElementValidationsBuilder::isChecked,
                null, "elementDomAttributeEquals", null, "checked", "is checked; checked attribute");
        assertExecutorInvocationState(newBuilder(), WebDriverElementValidationsBuilder::isVisible,
                null, "elementDomAttributeEquals", null, "hidden", "is visible; hidden attribute");
        assertExecutorInvocationState(newBuilder(), WebDriverElementValidationsBuilder::isEnabled,
                null, "elementDomAttributeEquals", null, "disabled", "is enabled; disabled attribute");
        assertExecutorInvocationState(newBuilder(), WebDriverElementValidationsBuilder::isNotSelected,
                null, "elementDomAttributeEquals", null, "selected", "is not selected; selected attribute");
        assertExecutorInvocationState(newBuilder(), WebDriverElementValidationsBuilder::isNotChecked,
                null, "elementDomAttributeEquals", null, "checked", "is not checked; checked attribute");
        assertExecutorInvocationState(newBuilder(), WebDriverElementValidationsBuilder::isHidden,
                null, "elementDomAttributeEquals", null, "hidden", "is hidden; hidden attribute");
        assertExecutorInvocationState(newBuilder(), WebDriverElementValidationsBuilder::isDisabled,
                null, "elementDomAttributeEquals", null, "disabled", "is disabled; disabled attribute");
    }

    private WebDriverElementValidationsBuilder newBuilder() {
        return new WebDriverElementValidationsBuilder(
                ValidationEnums.ValidationCategory.HARD_ASSERT,
                null,
                SAMPLE_LOCATOR,
                new StringBuilder());
    }

    private void assertNativeState(NativeValidationsBuilder nativeBuilder, String expectedMethod,
                                   String expectedElementAttribute, String expectedCssProperty) {
        Assert.assertEquals(nativeBuilder.validationMethod, expectedMethod);
        Assert.assertEquals(nativeBuilder.elementAttribute, expectedElementAttribute);
        Assert.assertEquals(nativeBuilder.elementCssProperty, expectedCssProperty);
    }

    private void assertExecutorInvocationState(WebDriverElementValidationsBuilder builder, ThrowingInvocation invocation,
                                               ValidationEnums.ValidationType expectedType, String expectedMethod,
                                               ValidationEnums.VisualValidationEngine expectedVisualEngine,
                                               String expectedElementAttribute, String expectedReportSnippet) {
        try {
            invocation.invoke(builder);
            Assert.fail("Expected invocation to fail without a live WebDriver session.");
        } catch (Throwable ignored) {
            // expected in unit context without a browser session
        }

        Assert.assertEquals(builder.validationType, expectedType);
        Assert.assertEquals(builder.validationMethod, expectedMethod);
        Assert.assertEquals(builder.visualValidationEngine, expectedVisualEngine);
        Assert.assertEquals(builder.elementAttribute, expectedElementAttribute);
        Assert.assertTrue(builder.reportMessageBuilder.toString().contains(expectedReportSnippet));
    }

    @FunctionalInterface
    private interface ThrowingInvocation {
        void invoke(WebDriverElementValidationsBuilder builder);
    }
}
