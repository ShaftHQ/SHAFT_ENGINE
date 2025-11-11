# 🏃 Quick Start Guide

Get started with SHAFT Engine in minutes! Choose the option that best fits your needs.

## Option 1: Project Generator (NEW! 🎉)

> [!TIP]
> Recommended for quickly creating new projects with a user-friendly interface.

- The fastest way to create a new SHAFT project with your preferred test runner and platform.
- Visit the [SHAFT Project Generator ➡️](https://shafthq.github.io/docs/Getting_Started/first_steps_5#option-1-interactive-project-generator-recommended) to generate your project in seconds.
- Choose your test runner (TestNG, JUnit, or Cucumber), select your platform (Web, Mobile, or API), and download a ready-to-use project.
- Optionally includes GitHub Actions workflow and Dependabot configuration.

## Option 2: Maven Archetype

> [!TIP]
> Recommended for new local sandbox projects using command line.

- The easiest and most straightforward way to create a new project that uses SHAFT.
- Just [follow the simple steps here ➡️](https://shafthq.github.io/docs/Getting_Started/first_steps_5#option-2-maven-archetype) to generate your new project with one command (all configurations included).

## Option 3: Start from Scratch

> [!TIP]
> Recommended if you're upgrading an existing project from Native Selenium WebDriver to SHAFT.

### Step 1: Initial Setup

- Create a new Java/Maven project using the latest version from IntelliJ IDEA, Eclipse, or your favorite IDE.
- Copy the highlighted contents of
  this [pom.xml](https://github.com/ShaftHQ/using_SHAFT_Engine/blob/main/GUI_Web/pom.xml#L11-L156) file into yours
  inside the ```<project>``` tag.

### Step 2: Creating Tests

#### 2.1. TestNG

- Create a new Package ```testPackage``` under ```src/test/java```
- Create a new Java class ```TestClass``` under your newly created `testPackage`.
- Copy the below imports into your newly created `TestClass` after the line that contains `package testPackage`.

```java
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
```

- Copy the below code snippet into the body of your `TestClass` after the line that contains `public class TestClass {`.

```java
SHAFT.GUI.WebDriver driver;
SHAFT.TestData.JSON testData;

String targetUrl = "https://duckduckgo.com/";

By logo = By.xpath("//div[contains(@class,'container_fullWidth__1H_L8')]//img");
By searchBox = Locator.hasAnyTagName().hasAttribute("name", "q").build(); // synonym to By.name("q");
By firstSearchResult = Locator.hasTagName("article").isFirst().build(); // synonym to By.xpath("(//article)[1]");

@Test
public void navigateToDuckDuckGoAndAssertBrowserTitleIsDisplayedCorrectly() {
  driver.browser().navigateToURL(targetUrl)
          .and().assertThat().browser().title().contains(testData.getTestData("expectedTitle"));
}

@Test
public void navigateToDuckDuckGoAndAssertLogoIsDisplayedCorrectly() {
  driver.browser().navigateToURL(targetUrl)
          .and().element().assertThat(logo).matchesReferenceImage();
}

@Test
public void searchForQueryAndAssert() {
  driver.browser().navigateToURL(targetUrl)
          .and().element().type(searchBox, testData.getTestData("searchQuery") + Keys.ENTER)
          .and().assertThat(firstSearchResult).text().doesNotEqual(testData.getTestData("unexpectedInFirstResult"));
}

@BeforeClass
public void beforeClass() {
  testData = new SHAFT.TestData.JSON("simpleJSON.json");
}

@BeforeMethod
public void beforeMethod() {
  driver = new SHAFT.GUI.WebDriver();
}

@AfterMethod
public void afterMethod(){
  driver.quit();
}
```

#### 2.2. JUnit5

- Create a new Package ```testPackage``` under ```src/test/java```
- Create a new Java class ```TestClass``` under your newly created `testPackage`.
- Copy the below imports into your newly created `TestClass` after the line that contains `package testPackage`.

```java
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
```

- Copy the below code snippet into the body of your `TestClass` after the line that contains `public class TestClass {`.

```java
private SHAFT.GUI.WebDriver driver;
private static SHAFT.TestData.JSON testData;

String targetUrl = "https://duckduckgo.com/";

By logo = By.xpath("//div[contains(@class,'container_fullWidth__1H_L8')]//img");
By searchBox = Locator.hasAnyTagName().hasAttribute("name", "q").build(); // synonym to By.name("q");
By firstSearchResult = Locator.hasTagName("article").isFirst().build(); // synonym to By.xpath("(//article)[1]");

@Test
public void navigateToDuckDuckGoAndAssertBrowserTitleIsDisplayedCorrectly() {
  driver.browser().navigateToURL(targetUrl)
          .and().assertThat().browser().title().contains(testData.getTestData("expectedTitle"));
}

@Test
public void navigateToDuckDuckGoAndAssertLogoIsDisplayedCorrectly() {
  driver.browser().navigateToURL(targetUrl)
          .and().element().assertThat(logo).matchesReferenceImage();
}

@Test
public void searchForQueryAndAssert() {
  driver.browser().navigateToURL(targetUrl)
          .and().element().type(searchBox, testData.getTestData("searchQuery") + Keys.ENTER)
          .and().assertThat(firstSearchResult).text().doesNotEqual(testData.getTestData("unexpectedInFirstResult"));
}

@BeforeAll
public static void beforeAll() {
  testData = new SHAFT.TestData.JSON("simpleJSON.json");
}

@BeforeEach
public void beforeEach() {
  driver = new SHAFT.GUI.WebDriver();
}

@AfterEach
public void afterEach(){
  driver.quit();
}
```

#### 2.3. Cucumber

- Create the following directory structure: ```src/test/java/cucumberTestRunner``` and ```src/test/java/customCucumberSteps```
- Create a new Java class ```CucumberTests.java``` under `cucumberTestRunner`.
- Copy the below code into your `CucumberTests.java`:

```java
package cucumberTestRunner;

import io.cucumber.testng.AbstractTestNGCucumberTests;
import org.testng.annotations.Listeners;

@Listeners({com.shaft.listeners.TestNGListener.class})
public class CucumberTests extends AbstractTestNGCucumberTests {
}
```

- Create a new Java class ```StepDefinitions.java``` under `customCucumberSteps`.
- Copy the below code into your `StepDefinitions.java`:

```java
package customCucumberSteps;

import com.shaft.driver.SHAFT;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;

public class StepDefinitions {
    private SHAFT.GUI.WebDriver driver;
    private SHAFT.TestData.JSON testData;
    
    @Given("I open the target browser")
    public void i_open_the_target_browser() {
        driver = new SHAFT.GUI.WebDriver();
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }
    
    @When("I navigate to {string}")
    public void i_navigate_to(String url) {
        driver.browser().navigateToURL(url);
    }
    
    @When("I search for {string}")
    public void i_search_for(String query) {
        By searchBox = By.name("q");
        driver.element().type(searchBox, query + Keys.ENTER);
    }
    
    @Then("I should see the page title contains {string}")
    public void i_should_see_the_page_title_contains(String expectedTitle) {
        driver.assertThat().browser().title().contains(expectedTitle).perform();
    }
    
    @Then("I close the browser")
    public void i_close_the_browser() {
        driver.quit();
    }
}
```

- Create the following directory: ```src/test/resources/features```
- Create a new file ```search.feature``` under `features` directory:

```gherkin
Feature: Search functionality

  Scenario: Verify DuckDuckGo search
    Given I open the target browser
    When I navigate to "https://duckduckgo.com/"
    Then I should see the page title contains "DuckDuckGo"
    When I search for "SHAFT_Engine"
    Then I close the browser
```

> [!TIP]
> In case you are planning to use Cucumber with IntelliJ IDEA, due to a known issue with IntelliJ you need to edit your run configuration template before running your tests by following these steps:
> <br/>- Open 'Edit Run/Debug Configurations' dialog > Edit Configurations... > Edit configuration templates...
> <br/>- Select <b>Cucumber Java</b> > Program Arguments > and add this argument:
> <br/>`--plugin com.shaft.listeners.CucumberFeatureListener`
> <br/>- After saving the changes, remember to delete any old runs you may have triggered by mistake before adding the needed config.


### Step 3: Managing Test Data
- Create the following file ```src/test/resources/testDataFiles/simpleJSON.json```.
- Copy the below code snippet into your newly created json file.
```json
{
  "searchQuery": "SHAFT_Engine",
  "expectedTitle": "DuckDuckGo",
  "unexpectedInFirstResult": "Nope"
}
```

### Step 4: Running Tests
- Run your ```TestClass.java``` either from the side menu or by pressing the run button.
- On the first test run: 
  - SHAFT will create a new folder ```src/main/resources/properties``` and generate some default properties files.
  - SHAFT will run in `minimalistic test run` mode and will self-configure its listeners under the `src/test/resources/META-INF/services` directory.
> [!NOTE]
> In case you get the following error message when trying to execute your first run:
> 
> ![image](https://github.com/user-attachments/assets/6b894234-e365-4fdd-a1d2-abd06ead7e98)
> 
> And you don't see the option ```Shorten the command line and rerun```:
>  - From Intellij IDEA main menu, go to Help/Edit Custom VM Options
>  - Add the following line and click save ```-Didea.dynamic.classpath=true```
>  - Restart IntelliJ to apply the changes

> [!TIP]
> You can visit the [user guide ➡️](https://shafthq.github.io/docs/Properties/PropertiesList) to learn how to configure all SHAFT's properties.
- On all following test runs:
  - After the run is complete, the Allure execution report will open automatically in your default web browser.
- <b>Join</b> our ![GitHub Repo stars](https://img.shields.io/github/stars/shafthq/shaft_engine?logoColor=black&style=social) to get notified by email when a new release is pushed out.
> [!NOTE]
> After upgrading your Engine to a new major release it is sometimes recommended to delete the properties
folder ```src\main\resources\properties``` and allow SHAFT to regenerate the defaults by running any test method.

---

[← Back to README](../README.md) | [Features →](FEATURES.md)
