package testPackage;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import poms.GoogleSearch;

public class ChainableElementActionsTests {
    SHAFT.GUI.WebDriver driver;

    @BeforeMethod
    public void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @Test
    public void chainElementActions() {
        driver.browser().navigateToURL("https://www.google.com/ncr", "https://www.google.com");

        By searchBox = GoogleSearch.getSearchBox_textField();

        driver.element().type(searchBox, "chained type 1")
                .type(searchBox, "chained type 2")
                .typeAppend(searchBox, "345");

        driver.assertThat().element(searchBox).text().isEqualTo("chained type 2345").perform();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod(){
        driver.quit();
    }
}
