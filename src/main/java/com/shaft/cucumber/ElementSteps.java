package com.shaft.cucumber;

import com.shaft.validation.Assertions;
import io.cucumber.java.ParameterType;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.util.Arrays;

public class ElementSteps {
    private final ThreadLocal<WebDriver> driver;

    public ElementSteps(ThreadLocal<WebDriver> driver) {
        if (driver == null) {
            driver = new ThreadLocal<>();
        }
        this.driver = driver;
    }

    @ParameterType(".*")
    public LocatorType locatorType(String locatorName) {
        return getLocatorTypeFromName(locatorName);
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element.
     *
     * @param text         the target text that needs to be typed into the target
     *                     webElement
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I Type {string} into the element found by {locatorType}: {string}")
    public void elementActionsType(String text, LocatorType locatorType, String locatorValue) {
        com.shaft.gui.element.ElementActions.type(driver.get(), getLocatorFromTypeAndValue(locatorType, locatorValue), text);
    }

    /**
     * Sends a keypress to the target element. Supported keys are: ENTER, RETURN, TAB.
     *
     * @param key          the key that should be pressed
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I Press the {word} key into the element found by {locatorType}: {string}")
    public void elementActionsKeyPress(String key, LocatorType locatorType, String locatorValue) {
        com.shaft.gui.element.ElementActions.keyPress(driver.get(), getLocatorFromTypeAndValue(locatorType, locatorValue), key);
    }

    /**
     * Clicks on a certain element using Selenium WebDriver, or JavaScript.
     *
     * @param locatorType  can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue the value/expression of the desired element locator
     */
    @When("I Click the element found by {locatorType}: {string}")
    public void elementActionsClick(LocatorType locatorType, String locatorValue) {
        com.shaft.gui.element.ElementActions.click(driver.get(), getLocatorFromTypeAndValue(locatorType, locatorValue));
    }

    /**
     * Asserts webElement attribute equals expectedValue.
     *
     * @param elementAttribute the desired attribute of the webElement under test
     * @param locatorType      can be {id, tagname, classname, name, linktext, partiallinktext, cssselector, xpath}
     * @param locatorValue     the value/expression of the desired element locator
     * @param expectedValue    the expected value (test data) of this assertion
     */
    @Then("I Assert that the {word} attribute of the element found by {locatorType}: {string}, should be {string}")
    public void assertElementAttribute(String elementAttribute, LocatorType locatorType, String locatorValue, String expectedValue) {
        Assertions.assertElementAttribute(driver.get(), getLocatorFromTypeAndValue(locatorType, locatorValue), elementAttribute, expectedValue);
    }

    private LocatorType getLocatorTypeFromName(String locatorType) {
        int values = LocatorType.values().length;
        for (int i = 0; i < values; i++) {
            if (Arrays.asList(LocatorType.values()).get(i).getValue()
                    .equalsIgnoreCase(locatorType.trim())) {
                return Arrays.asList(LocatorType.values()).get(i);
            }
        }
        return LocatorType.XPATH;
    }

    private By getLocatorFromTypeAndValue(LocatorType type, String locatorValue) {
        switch (type) {
            case ID -> {
                return By.id(locatorValue);
            }
            case TAG_NAME -> {
                return By.tagName(locatorValue);
            }
            case CLASS_NAME -> {
                return By.className(locatorValue);
            }
            case NAME -> {
                return By.name(locatorValue);
            }
            case LINK_TEXT -> {
                return By.linkText(locatorValue);
            }
            case PARTIAL_LINK_TEXT -> {
                return By.partialLinkText(locatorValue);
            }
            case CSS_SELECTOR -> {
                return By.cssSelector(locatorValue);
            }
            default -> {
                return By.xpath(locatorValue);
            }
        }
    }

    private enum LocatorType {
        ID("id"), TAG_NAME("tagname"), CLASS_NAME("classname"), NAME("name"), LINK_TEXT("linktext"),
        PARTIAL_LINK_TEXT("partiallinktext"), CSS_SELECTOR("cssselector"), XPATH("xpath");

        private final String value;

        LocatorType(String type) {
            this.value = type;
        }

        protected String getValue() {
            return value;
        }
    }
    //TODO: add element validations and assertions
}
