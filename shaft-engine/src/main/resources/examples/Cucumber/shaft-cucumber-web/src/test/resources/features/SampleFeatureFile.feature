Business Need: This Feature file demonstrates SHAFT_Engine's built in steps
Feature: Basic SHAFT_Engine Cucumber Web Steps

  Background: Open the browser
    Given I Open the target browser

  @SampleTests
  Scenario: Navigate to DuckDuckGo.com, and Assert that the browser title is displayed correctly
    When I Navigate to "https://duckduckgo.com/"
    Then I Assert that the "title" attribute of the browser, contains "DuckDuckGo"
    And I Close the current window

  @SampleTests
  Scenario: Navigate to DuckDuckGo.com, search for a query, and Assert that the logo is displayed correctly
    When I Navigate to "https://duckduckgo.com/"
    Then I Assert that the element found by "xpath": "//div[contains(@class,'container_fullWidth__1H_L8')]//img", exactly matches with the expected reference image using AI OpenCV
    And I Close the current window

  @SampleTests
  Scenario: Navigate to DuckDuckGo.com, search for a query, and Assert that the first result does not contain unexpected text
    When I Navigate to "https://duckduckgo.com/"
    And I Type "SHAFT_Engine" into the element found by "name": "q"
    And I Press "Enter" into the element found by "name": "q"
    Then I Assert that the "text" attribute of the element found by "xpath": "(//article)[1]", does not equal "Nope"
    And I Close the current window
