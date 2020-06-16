package com.shaft.cucumber;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Assertions;

import io.cucumber.java.Before;
import io.cucumber.java.ParameterType;
import io.cucumber.java.Scenario;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.util.Arrays;

public class GuiActionSteps {
    //TODO: handle shaft_engine listeners
    private ThreadLocal<WebDriver> driver = new ThreadLocal<>();

    @Before
    public void before(Scenario scenario){
        ReportManager.setCucumberScenario(scenario);
    }

    @Given("I Open the target browser")
    public void BrowserFactory_getBrowser(){
        driver.set(BrowserFactory.getBrowser());
    }

    @When("I Navigate to {string}")
    public void BrowserActions_navigateToURL(String url){
        BrowserActions.navigateToURL(driver.get(), url);
    }

    @When("I Navigate to {string} and get redirected to {string}")
    public void BrowserActions_navigateToURL(String url, String urlAfetrRedirection){
        BrowserActions.navigateToURL(driver.get(), url, urlAfetrRedirection);
    }

    @When("I Close the current window")
    public void BrowserActions_closeCurrentWindow(){
        BrowserActions.closeCurrentWindow(driver.get());
    }

    @ParameterType(".*")
    public LocatorType locatorType(String locatorName) {
        return getLocatorTypeFromName(locatorName);
    }

    @When("I Type {string} into the element found by {locatorType}: {string}")
    public void ElementActions_type(String text, LocatorType locatorType, String locatorValue){
        ElementActions.type(driver.get(), getLocatorFromTypeAndValue(locatorType, locatorValue), text);
    }

    /**
     * Sends a keypress to the target element. Supported keys are: ENTER, RETURN, TAB
     * @param key
     * @param locatorType
     * @param locatorValue
     */
    @When("I Press the {word} key into the element found by {locatorType}: {string}")
    public void ElementActions_keyPress(String key, LocatorType locatorType, String locatorValue){
        ElementActions.keyPress(driver.get(), getLocatorFromTypeAndValue(locatorType, locatorValue), key);
    }

    @When("I Click the element found by {locatorType}: {string}")
    public void ElementActions_click(LocatorType locatorType, String locatorValue){
        ElementActions.click(driver.get(), getLocatorFromTypeAndValue(locatorType, locatorValue));
    }

    @Then("I Assert that the {word} attribute of the element found by {locatorType}: {string}, should be {string}")
    public void Assertions_assertElementAttribute(String elementAttribute, LocatorType locatorType, String locatorValue, String expectedValue){
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

    private By getLocatorFromTypeAndValue(LocatorType type, String locatorValue){
        switch (type){
            case ID -> {return By.id(locatorValue);}
            case TAG_NAME -> {return By.tagName(locatorValue);}
            case CLASS_NAME -> {return By.className(locatorValue);}
            case NAME -> {return By.name(locatorValue);}
            case LINK_TEXT -> {return By.linkText(locatorValue);}
            case PARTIAL_LINK_TEXT -> {return By.partialLinkText(locatorValue);}
            case CSS_SELECTOR -> {return By.cssSelector(locatorValue);}
            default -> {return By.xpath(locatorValue);}
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
}
