package com.shaft.cucumber;

import com.shaft.driver.SHAFT;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.When;

import java.util.Objects;

public class BrowserSteps {
    private final ThreadLocal<SHAFT.GUI.WebDriver> driver;

    public BrowserSteps(ThreadLocal<SHAFT.GUI.WebDriver> driver) {
        this.driver = Objects.requireNonNullElseGet(driver, ThreadLocal::new);
    }

    /**
     * Read the target browser value from the execution.properties file
     */
    @Given("I Open the target browser")
//    @بفرض("انى قمت بفتح المتصفح المطلوب")
    public void getBrowser() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes the current page
     *
     * @param targetUrl a string that represents the URL that you wish to navigate to
     */
    @When("I Navigate to {string}")
//    @عندما("اقوم بزيارة هذا الموقع {string}")
    public void navigateToURL(String targetUrl) {
        driver.get().browser().navigateToURL(targetUrl);
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
    public void navigateToURL(String targetUrl, String targetUrlAfterRedirection) {
        driver.get().browser().navigateToURL(targetUrl, targetUrlAfterRedirection);
    }

    /**
     * Navigates one step back from the browsers history
     */
    @When("I Navigate back")
    public void navigateBack() {
        driver.get().browser().navigateBack();
    }

    /**
     * Navigates one step forward from the browsers history
     */
    @When("I Navigate forward")
    public void navigateForward() {
        driver.get().browser().navigateForward();
    }

    @When("I Maximize the current window")
    public void maximizeWindow() {
        driver.get().browser().maximizeWindow();
    }

    @When("I Resize the current window size to {int} width * {int} height")
    public void setWindowSize(int width, int height) {
        driver.get().browser().setWindowSize(width, height);
    }

    @When("I Full Screen the current window")
    public void fullScreenWindow() {
        driver.get().browser().fullScreenWindow();
    }

    @When("I Refresh the current window")
    public void refreshCurrentPage() {
        driver.get().browser().refreshCurrentPage();
    }

    /**
     * Closes the current browser window
     */
    @When("I Close the current window")
//    @عندما("اقوم بغلق نافذة المتصفح الحالية")
    public void closeCurrentWindow() {
        driver.get().quit();
    }
}