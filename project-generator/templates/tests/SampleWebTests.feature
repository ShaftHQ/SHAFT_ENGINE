Feature: Sample Web Tests
  This feature file demonstrates basic web automation tests using SHAFT Engine

  Scenario: Navigate to SHAFT Engine user guide and click Upgrade Now
    Given I navigate to SHAFT Engine user guide
    When I click the Upgrade Now button
    Then I should be redirected to the SHAFT Engine GitHub page

  Scenario: Search for SHAFT_Engine on DuckDuckGo
    Given I navigate to DuckDuckGo
    When I search for "SHAFT_Engine"
    Then the first search result should contain "SHAFT_Engine"
