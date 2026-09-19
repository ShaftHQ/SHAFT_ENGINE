Feature: Gap map mixed steps
  Scenario: Assertion and journey
    When the shopper completes checkout
    Then the order total should equal 42
