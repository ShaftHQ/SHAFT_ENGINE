#Author: Mohab.MohieElDeen@outlook.com
#Keywords Summary :
#Feature: List of scenarios.
#Scenario: Business rule through list of steps with arguments.
#Given: Some precondition step
#When: Some key actions
#Then: To observe outcomes or validation
#And,But: To enumerate more Given,When,Then steps
#Scenario Outline: List of steps for data-driven as an Examples and <placeholder>
#Examples: Container for s table
#Background: List of steps run before each of the scenarios
#""" (Doc Strings)
#| (Data Tables)
#@ (Tags/Labels):To group Scenarios
#<> (placeholder)
#""
## (Comments)
#Sample Feature Definition Template
@basicCucumberPOC
Feature: Basic Google Search Feature
  The user will search Google and explore the results

  Scenario: TC001 - Navigate to URL and Verify page title
    Given Target browser is launched
    When I navigate to Google search page URL
    Then I validate that the correct page has been opened
    And I validate that page title is equal to expected title from test data

  Scenario: TC002 - Search for Query and Assert that the number of results is displayed
    Given Target browser is launched
    When I perform search for the query that is retrieved from test data
    Then I validate that search results counter exists
    And I validate that search results counter holds a value

  Scenario: TC003 - Clicks the next button thrice to make sure that the framework can scroll element into view before clicking it
    Given Target browser is launched
    When I click the next button to make sure that the framework can scroll element into view before clicking it
    Then I find that ten results are displayed in this page
