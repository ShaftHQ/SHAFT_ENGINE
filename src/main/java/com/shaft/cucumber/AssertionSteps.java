package com.shaft.cucumber;

import com.shaft.validation.Assertions;
import io.cucumber.java.en.Then;
import org.openqa.selenium.WebDriver;

import java.util.Objects;

public class AssertionSteps {
    private final ThreadLocal<WebDriver> driver;

    public AssertionSteps(ThreadLocal<WebDriver> driver) {
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
        Assertions.assertBrowserAttribute(driver.get(), browserAttribute, expectedValue);
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
        Assertions.assertBrowserAttribute(driver.get(), browserAttribute, expectedValue, Assertions.AssertionComparisonType.EQUALS, Assertions.AssertionType.NEGATIVE);
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
        Assertions.assertBrowserAttribute(driver.get(), browserAttribute, expectedValue, Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.POSITIVE);
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
        Assertions.assertBrowserAttribute(driver.get(), browserAttribute, expectedValue, Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.NEGATIVE);
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
        Assertions.assertBrowserAttribute(driver.get(), browserAttribute, expectedValue, Assertions.AssertionComparisonType.MATCHES, Assertions.AssertionType.POSITIVE);
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
        Assertions.assertBrowserAttribute(driver.get(), browserAttribute, expectedValue, Assertions.AssertionComparisonType.MATCHES, Assertions.AssertionType.NEGATIVE);
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
        Assertions.assertElementAttribute(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), elementAttribute, expectedValue);
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
        Assertions.assertElementAttribute(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), elementAttribute, expectedValue, Assertions.AssertionComparisonType.EQUALS, Assertions.AssertionType.NEGATIVE);
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
        Assertions.assertElementAttribute(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), elementAttribute, expectedValue, Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.POSITIVE);
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
        Assertions.assertElementAttribute(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), elementAttribute, expectedValue, Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.NEGATIVE);
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
        Assertions.assertElementAttribute(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), elementAttribute, expectedValue, Assertions.AssertionComparisonType.MATCHES, Assertions.AssertionType.POSITIVE);
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
        Assertions.assertElementAttribute(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), elementAttribute, expectedValue, Assertions.AssertionComparisonType.MATCHES, Assertions.AssertionType.NEGATIVE);
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
        Assertions.assertElementExists(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue));
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
        Assertions.assertElementExists(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), Assertions.AssertionType.NEGATIVE);
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
        Assertions.assertElementCSSProperty(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), elementCSSProperty, expectedValue);
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
        Assertions.assertElementCSSProperty(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), elementCSSProperty, expectedValue, Assertions.AssertionComparisonType.EQUALS, Assertions.AssertionType.NEGATIVE);
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
        Assertions.assertElementCSSProperty(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), elementCSSProperty, expectedValue, Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.POSITIVE);
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
        Assertions.assertElementCSSProperty(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), elementCSSProperty, expectedValue, Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.NEGATIVE);
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
        Assertions.assertElementCSSProperty(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), elementCSSProperty, expectedValue, Assertions.AssertionComparisonType.MATCHES, Assertions.AssertionType.POSITIVE);
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
        Assertions.assertElementCSSProperty(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), elementCSSProperty, expectedValue, Assertions.AssertionComparisonType.MATCHES, Assertions.AssertionType.NEGATIVE);
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
        Assertions.assertElementMatches(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue));
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
        Assertions.assertElementMatches(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), Assertions.VisualValidationEngine.EXACT_OPENCV, Assertions.AssertionType.NEGATIVE);
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
        Assertions.assertElementMatches(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), Assertions.VisualValidationEngine.EXACT_EYES, Assertions.AssertionType.POSITIVE);
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
        Assertions.assertElementMatches(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), Assertions.VisualValidationEngine.EXACT_EYES, Assertions.AssertionType.NEGATIVE);
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
        Assertions.assertElementMatches(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), Assertions.VisualValidationEngine.STRICT_EYES, Assertions.AssertionType.POSITIVE);
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
        Assertions.assertElementMatches(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), Assertions.VisualValidationEngine.STRICT_EYES, Assertions.AssertionType.NEGATIVE);
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
        Assertions.assertElementMatches(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), Assertions.VisualValidationEngine.CONTENT_EYES, Assertions.AssertionType.POSITIVE);
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
        Assertions.assertElementMatches(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), Assertions.VisualValidationEngine.CONTENT_EYES, Assertions.AssertionType.NEGATIVE);
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
        Assertions.assertElementMatches(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), Assertions.VisualValidationEngine.LAYOUT_EYES, Assertions.AssertionType.POSITIVE);
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
        Assertions.assertElementMatches(driver.get(), ElementSteps.getLocatorFromTypeAndValue(locatorType, locatorValue), Assertions.VisualValidationEngine.LAYOUT_EYES, Assertions.AssertionType.NEGATIVE);
    }
}
