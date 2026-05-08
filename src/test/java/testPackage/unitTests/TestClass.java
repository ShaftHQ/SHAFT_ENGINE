package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import org.openqa.selenium.By;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import testPackage.Tests;

public class TestClass extends Tests {
    SHAFT.TestData.JSON testData;

    String targetUrl = SHAFT.Properties.paths.testData() + "searchFixture.html";

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
    public void beforeClass() {
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }
}
