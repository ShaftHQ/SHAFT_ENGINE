package com.shaft.cucumber;

import com.shaft.driver.SHAFT;
import com.shaft.validation.ValidationEnums;
import io.cucumber.java.en.Then;

import java.util.Objects;

@SuppressWarnings("SpellCheckingInspection")
public class AssertionSteps {
    private final ThreadLocal<SHAFT.GUI.WebDriver> driver;

    public AssertionSteps(ThreadLocal<SHAFT.GUI.WebDriver> driver) {
        this.driver = Objects.requireNonNullElseGet(driver, ThreadLocal::new);
    }

    /**
     * Asserts browser attribute equals expectedValue. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize.
     *
     * @param browserAttribute the desired attribute of the browser window
     *                         under test
     * @param expectedValue    the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} attribute of the browser, equals {string}")
    public void assertBrowserAttributeEquals(String browserAttribute, String expectedValue) {
        driver.get().assertThat().browser()
                .attribute(browserAttribute)
                .isEqualTo(expectedValue)
                .withCustomReportMessage("I Assert that the [" + browserAttribute + "] attribute of the browser, equals [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts browser attribute does not equal expectedValue. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize.
     *
     * @param browserAttribute the desired attribute of the browser window
     *                         under test
     * @param expectedValue    the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} attribute of the browser, does not equal {string}")
    public void assertBrowserAttributeDoesNotEqual(String browserAttribute, String expectedValue) {
        driver.get().assertThat().browser()
                .attribute(browserAttribute)
                .doesNotEqual(expectedValue)
                .withCustomReportMessage("I Assert that the [" + browserAttribute + "] attribute of the browser, does not equal [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts browser attribute contains expectedValue. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize.
     *
     * @param browserAttribute the desired attribute of the browser window
     *                         under test
     * @param expectedValue    the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} attribute of the browser, contains {string}")
    public void assertBrowserAttributeContains(String browserAttribute, String expectedValue) {
        driver.get().assertThat().browser()
                .attribute(browserAttribute)
                .contains(expectedValue)
                .withCustomReportMessage("I Assert that the [" + browserAttribute + "] attribute of the browser, contains [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts browser attribute does not contain expectedValue. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize.
     *
     * @param browserAttribute the desired attribute of the browser window
     *                         under test
     * @param expectedValue    the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} attribute of the browser, does not contain {string}")
    public void assertBrowserAttributeDoesNotContain(String browserAttribute, String expectedValue) {
        driver.get().assertThat().browser()
                .attribute(browserAttribute)
                .doesNotContain(expectedValue)
                .withCustomReportMessage("I Assert that the [" + browserAttribute + "] attribute of the browser, does not contain [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts browser attribute matches expectedValue. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize.
     *
     * @param browserAttribute the desired attribute of the browser window
     *                         under test
     * @param expectedValue    the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} attribute of the browser, matches the regular expression {string}")
    public void assertBrowserAttributeMatches(String browserAttribute, String expectedValue) {
        driver.get().assertThat().browser()
                .attribute(browserAttribute)
                .matchesRegex(expectedValue)
                .withCustomReportMessage("I Assert that the [" + browserAttribute + "] attribute of the browser, matches the regular expression [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts browser attribute does not match expectedValue. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize.
     *
     * @param browserAttribute the desired attribute of the browser window
     *                         under test
     * @param expectedValue    the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} attribute of the browser, does not match the regular expression {string}")
    public void assertBrowserAttributeDoesNotMatch(String browserAttribute, String expectedValue) {
        driver.get().assertThat().browser()
                .attribute(browserAttribute)
                .doesNotMatchRegex(expectedValue)
                .withCustomReportMessage("I Assert that the [" + browserAttribute + "] attribute of the browser, does not match the regular expression [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts webElement attribute equals expectedValue.
     *
     * @param elementAttribute the desired attribute of the webElement under test
     * @param locatorType      can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue     the value/expression of the desired element locator
     * @param expectedValue    the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} attribute of the element found by {string}: {string}, equals {string}")
//    @اذاً("أقوم بالتأكد من ان قيمة الصفة {string} الخاصة بعنصر الويب المحدد بإستخدام {string} بقيمة {string}, من المفترض أن تكون {string}")
    public void assertElementAttributeEquals(String elementAttribute, String locatorType, String locatorValue, String expectedValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .attribute(elementAttribute)
                .isEqualTo(expectedValue)
                .withCustomReportMessage("I Assert that the [" + elementAttribute + "] attribute of the element found by [" + locatorType + ": " + locatorValue + "], equals [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts webElement attribute does not equal expectedValue.
     *
     * @param elementAttribute the desired attribute of the webElement under test
     * @param locatorType      can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue     the value/expression of the desired element locator
     * @param expectedValue    the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} attribute of the element found by {string}: {string}, does not equal {string}")
    public void assertElementAttributeDoesNotEqual(String elementAttribute, String locatorType, String locatorValue, String expectedValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .attribute(elementAttribute)
                .doesNotEqual(expectedValue)
                .withCustomReportMessage("I Assert that the [" + elementAttribute + "] attribute of the element found by [" + locatorType + ": " + locatorValue + "], does not equal [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts webElement attribute contains expectedValue.
     *
     * @param elementAttribute the desired attribute of the webElement under test
     * @param locatorType      can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue     the value/expression of the desired element locator
     * @param expectedValue    the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} attribute of the element found by {string}: {string}, contains {string}")
    public void assertElementAttributeContains(String elementAttribute, String locatorType, String locatorValue, String expectedValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .attribute(elementAttribute)
                .contains(expectedValue)
                .withCustomReportMessage("I Assert that the [" + elementAttribute + "] attribute of the element found by [" + locatorType + ": " + locatorValue + "], contains [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts webElement attribute does not contain expectedValue.
     *
     * @param elementAttribute the desired attribute of the webElement under test
     * @param locatorType      can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue     the value/expression of the desired element locator
     * @param expectedValue    the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} attribute of the element found by {string}: {string}, does not contain {string}")
    public void assertElementAttributeDoesNotContain(String elementAttribute, String locatorType, String locatorValue, String expectedValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .attribute(elementAttribute)
                .doesNotContain(expectedValue)
                .withCustomReportMessage("I Assert that the [" + elementAttribute + "] attribute of the element found by [" + locatorType + ": " + locatorValue + "], does not contain [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts webElement attribute matches expectedValue.
     *
     * @param elementAttribute the desired attribute of the webElement under test
     * @param locatorType      can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue     the value/expression of the desired element locator
     * @param expectedValue    the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} attribute of the element found by {string}: {string}, matches the regular expression {string}")
    public void assertElementAttributeMatches(String elementAttribute, String locatorType, String locatorValue, String expectedValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .attribute(elementAttribute)
                .matchesRegex(expectedValue)
                .withCustomReportMessage("I Assert that the [" + elementAttribute + "] attribute of the element found by [" + locatorType + ": " + locatorValue + "], matches the regular expression [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts webElement attribute does not match expectedValue.
     *
     * @param elementAttribute the desired attribute of the webElement under test
     * @param locatorType      can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue     the value/expression of the desired element locator
     * @param expectedValue    the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} attribute of the element found by {string}: {string}, does not match the regular expression {string}")
    public void assertElementAttributeDoesNotMatch(String elementAttribute, String locatorType, String locatorValue, String expectedValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .attribute(elementAttribute)
                .doesNotMatchRegex(expectedValue)
                .withCustomReportMessage("I Assert that the [" + elementAttribute + "] attribute of the element found by [" + locatorType + ": " + locatorValue + "], does not match the regular expression [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts that the webElement found using the provided driver and locator
     * exists.
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @Then("I Assert that the element found by {string}: {string}, does exist")
    public void assertElementExists(String locatorType, String locatorValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .exists()
                .withCustomReportMessage("I Assert that the element found by [" + locatorType + ": " + locatorValue + "], does exist")
                .perform();
    }

    /**
     * Asserts that the webElement found using the provided driver and locator
     * does not exist.
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @Then("I Assert that the element found by {string}: {string}, does not exist")
    public void assertElementDoesNotExist(String locatorType, String locatorValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .doesNotExist()
                .withCustomReportMessage("I Assert that the element found by [" + locatorType + ": " + locatorValue + "], does not exist")
                .perform();
    }

    /**
     * Asserts webElement CSSProperty equals expectedValue.
     *
     * @param elementCSSProperty the target CSS property of the webElement under test
     * @param locatorType        can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue       the value/expression of the desired element locator
     * @param expectedValue      the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} CSS property of the element found by {string}: {string}, equals {string}")
    public void assertElementCSSPropertyEquals(String elementCSSProperty, String locatorType, String locatorValue, String expectedValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .cssProperty(elementCSSProperty)
                .isEqualTo(expectedValue)
                .withCustomReportMessage("I Assert that the [" + elementCSSProperty + "] CSS property of the element found by [" + locatorType + ": " + locatorValue + "], equals [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts webElement CSSProperty does not equal expectedValue.
     *
     * @param elementCSSProperty the target CSS property of the webElement under test
     * @param locatorType        can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue       the value/expression of the desired element locator
     * @param expectedValue      the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} CSS property of the element found by {string}: {string}, does not equal {string}")
    public void assertElementCSSPropertyDoesNotEqual(String elementCSSProperty, String locatorType, String locatorValue, String expectedValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .cssProperty(elementCSSProperty)
                .doesNotEqual(expectedValue)
                .withCustomReportMessage("I Assert that the [" + elementCSSProperty + "] CSS property of the element found by [" + locatorType + ": " + locatorValue + "], does not equal [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts webElement CSSProperty contains expectedValue.
     *
     * @param elementCSSProperty the target CSS property of the webElement under test
     * @param locatorType        can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue       the value/expression of the desired element locator
     * @param expectedValue      the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} CSS property of the element found by {string}: {string}, contains {string}")
    public void assertElementCSSPropertyContains(String elementCSSProperty, String locatorType, String locatorValue, String expectedValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .cssProperty(elementCSSProperty)
                .contains(expectedValue)
                .withCustomReportMessage("I Assert that the [" + elementCSSProperty + "] CSS property of the element found by [" + locatorType + ": " + locatorValue + "], contains [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts webElement CSSProperty does not contain expectedValue.
     *
     * @param elementCSSProperty the target CSS property of the webElement under test
     * @param locatorType        can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue       the value/expression of the desired element locator
     * @param expectedValue      the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} CSS property of the element found by {string}: {string}, does not contain {string}")
    public void assertElementCSSPropertyDoesNotContain(String elementCSSProperty, String locatorType, String locatorValue, String expectedValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .cssProperty(elementCSSProperty)
                .doesNotContain(expectedValue)
                .withCustomReportMessage("I Assert that the [" + elementCSSProperty + "] CSS property of the element found by [" + locatorType + ": " + locatorValue + "], does not contain [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts webElement CSSProperty matches expectedValue.
     *
     * @param elementCSSProperty the target CSS property of the webElement under test
     * @param locatorType        can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue       the value/expression of the desired element locator
     * @param expectedValue      the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} CSS property of the element found by {string}: {string}, matches the regular expression {string}")
    public void assertElementCSSPropertyMatches(String elementCSSProperty, String locatorType, String locatorValue, String expectedValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .cssProperty(elementCSSProperty)
                .matchesRegex(expectedValue)
                .withCustomReportMessage("I Assert that the [" + elementCSSProperty + "] CSS property of the element found by [" + locatorType + ": " + locatorValue + "], matches the regular expression [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts webElement CSSProperty does not match expectedValue.
     *
     * @param elementCSSProperty the target CSS property of the webElement under test
     * @param locatorType        can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue       the value/expression of the desired element locator
     * @param expectedValue      the expected value (test data) of this assertion
     */
    @Then("I Assert that the {string} CSS property of the element found by {string}: {string}, does not match the regular expression {string}")
    public void assertElementCSSPropertyDoesNotMatch(String elementCSSProperty, String locatorType, String locatorValue, String expectedValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .cssProperty(elementCSSProperty)
                .doesNotMatchRegex(expectedValue)
                .withCustomReportMessage("I Assert that the [" + elementCSSProperty + "] CSS property of the element found by [" + locatorType + ": " + locatorValue + "], does not match the regular expression [" + expectedValue + "]")
                .perform();
    }

    /**
     * Asserts that the current image of the target element matches the expected reference image using the desired VisualValidationEngine if AssertionType is POSITIVE, or
     * doesn't match it if AssertionType is NEGATIVE. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @Then("I Assert that the element found by {string}: {string}, exactly matches with the expected reference image using Artificial Intelligence (OpenCV)")
    public void assertElementMatchesOpenCV(String locatorType, String locatorValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .matchesReferenceImage()
                .withCustomReportMessage("I Assert that the element found by [" + locatorType + ": " + locatorValue + "], exactly matches with the expected reference image using Artificial Intelligence (OpenCV)")
                .perform();
    }

    /**
     * Asserts that the current image of the target element matches the expected reference image using the desired VisualValidationEngine if AssertionType is POSITIVE, or
     * doesn't match it if AssertionType is NEGATIVE. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @Then("I Assert that the element found by {string}: {string}, does not exactly match with the expected reference image using Artificial Intelligence (OpenCV)")
    public void assertElementDoesNotMatchOpenCV(String locatorType, String locatorValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .doesNotMatchReferenceImage()
                .withCustomReportMessage("I Assert that the element found by [" + locatorType + ": " + locatorValue + "], does not exactly match with the expected reference image using Artificial Intelligence (OpenCV)")
                .perform();
    }

    /**
     * Asserts that the current image of the target element matches the expected reference image using the desired VisualValidationEngine if AssertionType is POSITIVE, or
     * doesn't match it if AssertionType is NEGATIVE. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @Then("I Assert that the element found by {string}: {string}, exactly matches with the expected reference image using Artificial Intelligence (Applitools Eyes)")
    public void assertElementMatchesExactEyes(String locatorType, String locatorValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .matchesReferenceImage(ValidationEnums.VisualValidationEngine.EXACT_EYES)
                .withCustomReportMessage("I Assert that the element found by [" + locatorType + ": " + locatorValue + "], exactly matches with the expected reference image using Artificial Intelligence (Applitools Eyes)")
                .perform();
    }

    /**
     * Asserts that the current image of the target element matches the expected reference image using the desired VisualValidationEngine if AssertionType is POSITIVE, or
     * doesn't match it if AssertionType is NEGATIVE. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @Then("I Assert that the element found by {string}: {string}, does not exactly match with the expected reference image using Artificial Intelligence (Applitools Eyes)")
    public void assertElementDoesNotMatchExactEyes(String locatorType, String locatorValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .doesNotMatchReferenceImage(ValidationEnums.VisualValidationEngine.EXACT_EYES)
                .withCustomReportMessage("I Assert that the element found by [" + locatorType + ": " + locatorValue + "], does not exactly match with the expected reference image using Artificial Intelligence (Applitools Eyes)")
                .perform();
    }

    /**
     * Asserts that the current image of the target element matches the expected reference image using the desired VisualValidationEngine if AssertionType is POSITIVE, or
     * doesn't match it if AssertionType is NEGATIVE. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @Then("I Assert that the element found by {string}: {string}, strictly matches with the expected reference image using Artificial Intelligence (Applitools Eyes)")
    public void assertElementMatchesStrictEyes(String locatorType, String locatorValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .matchesReferenceImage(ValidationEnums.VisualValidationEngine.STRICT_EYES)
                .withCustomReportMessage("I Assert that the element found by [" + locatorType + ": " + locatorValue + "], strictly matches with the expected reference image using Artificial Intelligence (Applitools Eyes)")
                .perform();
    }

    /**
     * Asserts that the current image of the target element matches the expected reference image using the desired VisualValidationEngine if AssertionType is POSITIVE, or
     * doesn't match it if AssertionType is NEGATIVE. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @Then("I Assert that the element found by {string}: {string}, does not strictly match with the expected reference image using Artificial Intelligence (Applitools Eyes)")
    public void assertElementDoesNotMatchStrictEyes(String locatorType, String locatorValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .doesNotMatchReferenceImage(ValidationEnums.VisualValidationEngine.STRICT_EYES)
                .withCustomReportMessage("I Assert that the element found by [" + locatorType + ": " + locatorValue + "], does not strictly match with the expected reference image using Artificial Intelligence (Applitools Eyes)")
                .perform();
    }

    /**
     * Asserts that the current image of the target element matches the expected reference image using the desired VisualValidationEngine if AssertionType is POSITIVE, or
     * doesn't match it if AssertionType is NEGATIVE. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @Then("I Assert that the element found by {string}: {string}, matches the content of the expected reference image using Artificial Intelligence (Applitools Eyes)")
    public void assertElementMatchesContentEyes(String locatorType, String locatorValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .matchesReferenceImage(ValidationEnums.VisualValidationEngine.CONTENT_EYES)
                .withCustomReportMessage("I Assert that the element found by [" + locatorType + ": " + locatorValue + "], matches the content of the expected reference image using Artificial Intelligence (Applitools Eyes)")
                .perform();
    }

    /**
     * Asserts that the current image of the target element matches the expected reference image using the desired VisualValidationEngine if AssertionType is POSITIVE, or
     * doesn't match it if AssertionType is NEGATIVE. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @Then("I Assert that the element found by {string}: {string}, does not match the content of the expected reference image using Artificial Intelligence (Applitools Eyes)")
    public void assertElementDoesNotMatchContentEyes(String locatorType, String locatorValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .doesNotMatchReferenceImage(ValidationEnums.VisualValidationEngine.CONTENT_EYES)
                .withCustomReportMessage("I Assert that the element found by [" + locatorType + ": " + locatorValue + "], does not match the content of the expected reference image using Artificial Intelligence (Applitools Eyes)")
                .perform();
    }

    /**
     * Asserts that the current image of the target element matches the expected reference image using the desired VisualValidationEngine if AssertionType is POSITIVE, or
     * doesn't match it if AssertionType is NEGATIVE. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @Then("I Assert that the element found by {string}: {string}, matches the layout of the expected reference image using Artificial Intelligence (Applitools Eyes)")
    public void assertElementMatchesLayoutEyes(String locatorType, String locatorValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .matchesReferenceImage(ValidationEnums.VisualValidationEngine.LAYOUT_EYES)
                .withCustomReportMessage("I Assert that the element found by [" + locatorType + ": " + locatorValue + "], matches the layout of the expected reference image using Artificial Intelligence (Applitools Eyes)")
                .perform();
    }

    /**
     * Asserts that the current image of the target element matches the expected reference image using the desired VisualValidationEngine if AssertionType is POSITIVE, or
     * doesn't match it if AssertionType is NEGATIVE. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @Then("I Assert that the element found by {string}: {string}, does not match the layout of the expected reference image using Artificial Intelligence (Applitools Eyes)")
    public void assertElementDoesNotMatchLayoutEyes(String locatorType, String locatorValue) {
        driver.get().assertThat()
                .element(ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue))
                .doesNotMatchReferenceImage(ValidationEnums.VisualValidationEngine.LAYOUT_EYES)
                .withCustomReportMessage("I Assert that the element found by [" + locatorType + ": " + locatorValue + "], does not match the layout of the expected reference image using Artificial Intelligence (Applitools Eyes)")
                .perform();
    }
}
