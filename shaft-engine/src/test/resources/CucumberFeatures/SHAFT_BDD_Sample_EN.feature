Business Need: This Feature file demonstrates SHAFT_Engine's built in steps
Feature: Basic SHAFT_Engine BDD POC

  Scenario: Browser steps
    Given I Open the target browser
    When I Navigate to "https://www.selenium.dev/selenium/web/xhtmlTest.html"
    And I Navigate to "https://duckduckgo.com/"
    And I Navigate back
    And I Navigate forward
    And I Maximize the current window
    And I Resize the current window size to 1920 width * 1080 height
    And I Full Screen the current window
    And I Refresh the current window
    And I Close the current window

  @smoke
  Scenario: Browser attribute assertions
    Given I Open the target browser
    When I Navigate to "https://www.selenium.dev/selenium/web/xhtmlTest.html"
    Then I Assert that the "Title" attribute of the browser, equals "XHTML Test Page"
    And I Assert that the "Title" attribute of the browser, does not equal "Dummy"
    Then I Assert that the "Title" attribute of the browser, contains "XHTML"
    And I Assert that the "Title" attribute of the browser, does not contain "Dummy"
    Then I Assert that the "Title" attribute of the browser, matches the regular expression "XHTML.*"
    Then I Assert that the "Title" attribute of the browser, does not match the regular expression "Dummy.*"
    And I Close the current window

  Scenario: Element visual assertions
    Given I Open the target browser
    When I Navigate to "https://shafthq.github.io/"
    Then I Assert that the element found by "xpath": "(//img[contains(@src,'shaft.svg')])[1]", does exist
    And I Close the current window
