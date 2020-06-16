Feature: Basic SHAFT_Engine BDD POC
  Scenario: TC001 - Open Browser, Navigate to Google, Search for query, Assert Result
    Given I Open the target browser
    When I Navigate to "https://www.google.com/ncr" and get redirected to "https://www.google.com/"
    And I Type "SHAFT_Engine" into the element found by name: "q"
    And I Press the Enter key into the element found by name: "q"
    Then I Assert that the href attribute of the element found by xpath: "(//h2[text()='Web result with site links']/following::a)[1]", should be "https://github.com/MohabMohie/SHAFT_ENGINE"
    And I Close the current window