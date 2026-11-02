Feature: Poisoned draft

  @AC-01
  Scenario: click the button
    Given a shopper
    When I click xpath=//button[@id='place-order']
    Then the order is placed
