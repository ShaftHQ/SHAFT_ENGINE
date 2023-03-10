Feature: Sample Feature file to test custom cucumber steps

  Scenario: This is the first test scenario in the sample feature file
    Given I open the target browser
    When I navigate to "Google Home"
    And I search for "SHAFT_Engine"
    Then The first result text will contain "SHAFT"