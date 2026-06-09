package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.Tests;

import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

public class TestClass extends Tests {
    SHAFT.TestData.JSON testData;

    String targetUrl;

    By searchBox = Locator.hasAnyTagName().hasAttribute("id", "searchbox_input").build();
    By firstSearchResult = Locator.hasTagName("article").isFirst().build();

    @Test
    public void navigateToSearchFixtureAndAssertBrowserTitleIsDisplayedCorrectly() {
        driver.get().browser().navigateToURL(targetUrl)
                .and().assertThat().title().isEqualTo("SHAFT Search Fixture");
    }

    @Test
    public void navigateToSearchFixtureAndAssertSearchBoxIsVisible() {
        driver.get().browser().navigateToURL(targetUrl)
                .and().element().assertThat(searchBox).isVisible();
    }

    @Test
    public void searchForQueryAndAssert() {
        driver.get().browser().navigateToURL(targetUrl)
                .and().element().type(searchBox, testData.getTestData("searchQuery"))
                .and().assertThat(firstSearchResult).text().doesNotEqual(testData.getTestData("unexpectedInFirstResult"));
    }

    @BeforeClass
    public void beforeClass() throws IOException {
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
        String fixtureHtml = Files.readString(Path.of(SHAFT.Properties.paths.testData(), "searchFixture.html"));
        targetUrl = "data:text/html;charset=utf-8," + URLEncoder.encode(fixtureHtml, StandardCharsets.UTF_8).replace("+", "%20");
    }

    @BeforeMethod(alwaysRun = true)
    @Override
    public void init() {
        Properties.clearForCurrentThread();
        setThreadLocalPropertyFromSystemProperty("executionAddress");
        setThreadLocalPropertyFromSystemProperty("targetOperatingSystem");
        setThreadLocalPropertyFromSystemProperty("targetBrowserName");
        setThreadLocalPropertyFromSystemProperty("headlessExecution");
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    @Override
    public void tear() {
        if (driver.get() != null) {
            driver.get().quit();
            driver.remove();
        }
        Properties.clearForCurrentThread();
    }

    private void setThreadLocalPropertyFromSystemProperty(String propertyName) {
        String propertyValue = System.getProperty(propertyName);
        if (propertyValue != null && !propertyValue.isBlank()) {
            ThreadLocalPropertiesManager.setProperty(propertyName, propertyValue);
        }
    }
}
