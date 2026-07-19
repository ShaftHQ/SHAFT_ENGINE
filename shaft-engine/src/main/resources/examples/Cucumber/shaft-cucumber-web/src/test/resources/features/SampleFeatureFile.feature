Business Need: This Feature file demonstrates SHAFT_Engine's built in steps
Feature: Basic SHAFT_Engine Cucumber Web Steps

  Background: Open the browser
    Given I Open the target browser

  @SampleTests
  Scenario: Navigate to Wikipedia.org, and Assert that the browser title is displayed correctly
    When I Navigate to "https://en.wikipedia.org/wiki/Main_Page"
    Then I Assert that the "title" attribute of the browser, contains "Wikipedia"
    And I Close the current window

  @SampleTests
  Scenario: Navigate to Wikipedia.org, and Assert that the logo is displayed correctly
    When I Navigate to "https://en.wikipedia.org/wiki/Main_Page"
    Then I Assert that the element found by "xpath": "//img[@class='mw-logo-icon']", exactly matches with the expected reference image using AI OpenCV
    And I Close the current window

  @SampleTests
  Scenario: Navigate to Wikipedia.org, search for a query, and Assert that the first result does not contain unexpected text
    When I Navigate to "https://en.wikipedia.org/wiki/Main_Page"
    And I Type "Software testing framework" into the element found by "id": "searchInput"
    And I Press "Enter" into the element found by "id": "searchInput"
    And I Click the element found by "xpath": "(//div[contains(@class,'mw-search-result-heading')])[1]//a"
    Then I Assert that the "title" attribute of the browser, contains "Test automation"
    And I Assert that the "text" attribute of the browser, contains "Software testing"
    And I Close the current window
