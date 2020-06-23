package com.shaft.cucumber;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.validation.Assertions;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.openqa.selenium.WebDriver;

public class BrowserSteps {
    private final ThreadLocal<WebDriver> driver;

    public BrowserSteps(ThreadLocal<WebDriver> driver) {
        if (driver == null) {
            this.driver = new ThreadLocal<>();
        }else {
            this.driver = driver;
        }
    }

    /**
     * Read the target browser value from the execution.properties file
     */
    @Given("I Open the target browser")
    public void browserFactoryGetBrowser() {
        driver.set(BrowserFactory.getBrowser());
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes the current page
     *
     * @param targetUrl a string that represents the URL that you wish to navigate to
     */
    @When("I Navigate to {string}")
    public void browserActionsNavigateToURL(String targetUrl) {
        BrowserActions.navigateToURL(driver.get(), targetUrl);
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes
     * the current page. Waits for successfully navigating to the final url after
     * redirection.
     *
     * @param targetUrl                 a string that represents the URL that you wish to navigate to
     * @param targetUrlAfterRedirection a string that represents a part of the url that should be present after redirection, this string is used to confirm successful navigation
     */
    @When("I Navigate to {string} and get redirected to {string}")
    public void browserActionsNavigateToURL(String targetUrl, String targetUrlAfterRedirection) {
        BrowserActions.navigateToURL(driver.get(), targetUrl, targetUrlAfterRedirection);
    }

    /**
     * Navigates one step back from the browsers history
     */
    @When("I Navigate back")
    public void browserActionsNavigateBack() {
        BrowserActions.navigateBack(driver.get());
    }

    /**
     * Navigates one step forward from the browsers history
     */
    @When("I Navigate forward")
    public void browserActionsNavigateForward() {
        BrowserActions.navigateForward(driver.get());
    }

    @When("I Maximize the current window")
    public void browserActionsMaximizeWindow() {
        BrowserActions.maximizeWindow(driver.get());
    }

    @When("I Resize the current window size to {int} width * {int} height")
    public void browserActionsSetWindowSize(int width, int height) {
        BrowserActions.setWindowSize(driver.get(), width, height);
    }

    @When("I Full Screen the current window")
    public void browserActionsFullScreenWindow() {
        BrowserActions.fullScreenWindow(driver.get());
    }

    @When("I Refresh the current window")
    public void browserActionsRefreshCurrentPage() {
        BrowserActions.refreshCurrentPage(driver.get());
    }

    /**
     * Closes the current browser window
     */
    @When("I Close the current window")
    public void browserActionsCloseCurrentWindow() {
        BrowserActions.closeCurrentWindow(driver.get());
    }

    /**
     * Asserts browser attribute equals expectedValue. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize.
     *
     * @param browserAttribute the desired attribute of the browser window
     *                         under test
     * @param expectedValue    the expected value (test data) of this assertion
     */
    @Then("I Assert that the {word} attribute of the browser, should be {string}")
    public void assertBrowserAttribute(String browserAttribute, String expectedValue) {
        Assertions.assertBrowserAttribute(driver.get(), browserAttribute, expectedValue);
    }
    //TODO: add browser validations and assertions
}
