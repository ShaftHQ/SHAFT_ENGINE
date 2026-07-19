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
    Then I Assert that the element found by "xpath": "//a[@class='mw-logo']", exactly matches with the expected reference image using AI OpenCV
    And I Close the current window

  # Typing the exact title of an existing Wikipedia article and submitting the search form (Enter)
  # deterministically redirects straight to that article -- MediaWiki's "go to page" behavior for
  # an exact title match -- so this never depends on Wikipedia's live, ranked full-text search
  # results, which drift over time and are not a stable test oracle.
  @SampleTests
  Scenario: Navigate to Wikipedia.org, search for an exact article title, and Assert the article page loaded
    When I Navigate to "https://en.wikipedia.org/wiki/Main_Page"
    And I Type "Software testing" into the element found by "id": "searchInput"
    And I Press "Enter" into the element found by "id": "searchInput"
    Then I Assert that the "title" attribute of the browser, contains "Software testing"
    And I Assert that the "text" attribute of the browser, contains "Software testing"
    And I Close the current window
