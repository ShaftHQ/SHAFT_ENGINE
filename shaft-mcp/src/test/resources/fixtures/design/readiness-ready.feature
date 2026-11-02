Feature: Checkout readiness

  @AC-01
  Scenario: valid payment
    Given a shopper with a saved card
    When the shopper places the order
    Then the order total should equal 42

  @AC-02
  Scenario: declined card
    Given a shopper with a declined card
    When the shopper places the order
    Then an error status is displayed

  @AC-03
  Scenario: duplicate submit
    Given a shopper who already submitted
    When the shopper places the order again within 30 seconds
    Then a single order exists
