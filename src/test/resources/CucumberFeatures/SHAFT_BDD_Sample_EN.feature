Business Need: This Feature file demonstrates SHAFT_Engine's built in steps
Feature: Basic SHAFT_Engine BDD POC

  Scenario: Browser steps
    Given I Open the target browser
    When I Navigate to "https://www.google.com/ncr" and get redirected to "https://www.google.com/"
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
    When I Navigate to "https://www.google.com/ncr" and get redirected to "https://www.google.com/"
    Then I Assert that the "Title" attribute of the browser, equals "Google"
    And I Assert that the "Title" attribute of the browser, does not equal "Dummy"
    Then I Assert that the "Title" attribute of the browser, contains "G"
    And I Assert that the "Title" attribute of the browser, does not contain "D"
    Then I Assert that the "Title" attribute of the browser, matches the regular expression "G.*"
    Then I Assert that the "Title" attribute of the browser, does not match the regular expression "D.*"
    And I Close the current window

  Scenario: Element steps
    Given I Open the target browser
    When I Navigate to "https://www.google.com/ncr" and get redirected to "https://www.google.com/"
    And I Type "SHAFT_Engine" into the element found by "name": "q"
    And I Type "password" securely into the element found by "name": "q"
    And I Append the text "123" to the element found by "name": "q"
#    And I Upload the file {string} to the element found by {string}: {string}
    And I Click the element found by "name": "q"
    And I Click and hold the element found by "name": "q"
#    And I use the clipboard to perform "select all" on the element found by "name": "q"
    And I Double-click the element found by "name": "q"
#    And I Drag the element found by {string}: {string} and drop it on the element found by {string}: {string}
#    And I Drag the element found by {string}: {string} and drop it by offset x={int} and y={int}
    # And I Hover over the element found by "selector": ".lnXdpd:nth-child(2)"
    # And I Hover over the element found by "cssselector": ".lnXdpd:nth-child(2)"
    # And I Hover over the element found by "css": ".lnXdpd:nth-child(2)"
    # And I Hover over the element found by "css_selector": ".lnXdpd:nth-child(2)"
    # And I Hover over the element found by "css selector": ".lnXdpd:nth-child(2)"
    And I Hover over the element found by "classname": "gLFyf"
    And I Hover over the element found by "class_name": "gLFyf"
    And I Hover over the element found by "class name": "gLFyf"
#    And I Select {string} from the drop-down list element found by {string}: {string}
#    And I Submit the form found by "name": "q"
    And I Press the "Enter" key into the element found by "name": "q"
    And I Wait for the element found by "xpath": "(//h3[contains(@class,'LC20lb')])[1]/parent::a" to be present
    And I Navigate back
#    And I Wait for the element found by "xpath": "(//h3[contains(@class,'LC20lb')])[1]/parent::a" to be not present
    And I Set the value "SHAFT_Engine" into the element found by "name": "q"
    And I Press the "Enter" key into the element found by "name": "q"
#    And I Wait for the text inside the element found by {string}: {string} to change from the initial value {string}
    And I Close the current window

  Scenario: Element attribute assertions
    Given I Open the target browser
    When I Navigate to "https://www.google.com/ncr" and get redirected to "https://www.google.com/"
    And I Type "SHAFT_Engine" into the element found by "name": "q"
    And I Press the "Enter" key into the element found by "name": "q"
    Then I Assert that the "href" attribute of the element found by "xpath": "(//h3[contains(@class,'LC20lb')])[1]/parent::a", equals "https://github.com/ShaftHQ/SHAFT_ENGINE"
    And I Assert that the "href" attribute of the element found by "xpath": "(//h3[contains(@class,'LC20lb')])[1]/parent::a", does not equal "Dummy"
    And I Assert that the "href" attribute of the element found by "xpath": "(//h3[contains(@class,'LC20lb')])[1]/parent::a", contains "SHAFT"
    And I Assert that the "href" attribute of the element found by "xpath": "(//h3[contains(@class,'LC20lb')])[1]/parent::a", does not contain "Dummy"
    And I Assert that the "href" attribute of the element found by "xpath": "(//h3[contains(@class,'LC20lb')])[1]/parent::a", matches the regular expression ".*SHAFT.*"
    And I Assert that the "href" attribute of the element found by "xpath": "(//h3[contains(@class,'LC20lb')])[1]/parent::a", does not match the regular expression ".*Dummy.*"
    And I Close the current window

  Scenario: Element CSS property assertions
    Given I Open the target browser
    When I Navigate to "https://www.google.com/ncr" and get redirected to "https://www.google.com/"
    And I Type "SHAFT_Engine" into the element found by "name": "q"
    Then I Assert that the element found by "name": "q", does exist
    And I Assert that the element found by "xpath": "(//h3[contains(@class,'LC20lb')])[1]/parent::a", does not exist
#    And I Assert that the "appearance" CSS property of the element found by "name": "q", equals "auto"
    And I Assert that the "appearance" CSS property of the element found by "name": "q", does not equal "Dummy"
    And I Assert that the "appearance" CSS property of the element found by "name": "q", contains "t"
    And I Assert that the "appearance" CSS property of the element found by "name": "q", does not contain "mmy"
    And I Assert that the "appearance" CSS property of the element found by "name": "q", matches the regular expression ".*t.*"
    And I Assert that the "appearance" CSS property of the element found by "name": "q", does not match the regular expression ".*Dummy.*"
    And I Close the current window

  Scenario: Element visual assertions
    Given I Open the target browser
    When I Navigate to "https://shafthq.github.io/"
    Then I Assert that the element found by "xpath": "(//img[contains(@src,'shaft.svg')])[1]", exactly matches with the expected reference image using Artificial Intelligence OpenCV
#    Then I Assert that the element found by "xpath": "(//img[contains(@src,'shaft.svg')])[1]", does not exactly match with the expected reference image using Artificial Intelligence OpenCV
#    Then I Assert that the element found by "xpath": "(//img[contains(@src,'shaft.svg')])[1]", exactly matches with the expected reference image using Artificial Intelligence Applitools Eyes
#    Then I Assert that the element found by "xpath": "(//img[contains(@src,'shaft.svg')])[1]", does not exactly match with the expected reference image using Artificial Intelligence Applitools Eyes
#    Then I Assert that the element found by "xpath": "(//img[contains(@src,'shaft.svg')])[1]", strictly matches with the expected reference image using Artificial Intelligence Applitools Eyes
#    Then I Assert that the element found by "xpath": "(//img[contains(@src,'shaft.svg')])[1]", does not strictly match with the expected reference image using Artificial Intelligence Applitools Eyes
#    Then I Assert that the element found by "xpath": "(//img[contains(@src,'shaft.svg')])[1]", matches the content of the expected reference image using Artificial Intelligence Applitools Eyes
#    Then I Assert that the element found by "xpath": "(//img[contains(@src,'shaft.svg')])[1]", does not match the content of the expected reference image using Artificial Intelligence Applitools Eyes
#    Then I Assert that the element found by "xpath": "(//img[contains(@src,'shaft.svg')])[1]", matches the layout of the expected reference image using Artificial Intelligence Applitools Eyes
#    Then I Assert that the element found by "xpath": "(//img[contains(@src,'shaft.svg')])[1]", does not match the layout of the expected reference image using Artificial Intelligence Applitools Eyes
    And I Close the current window
