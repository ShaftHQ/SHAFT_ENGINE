package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import poms.GoogleSearch;

public class ChainableElementActionsTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @Test
    public void chainElementActions() {
        driver.get().browser().navigateToURL("https://www.google.com/ncr", "https://www.google.com");

        By searchBox = GoogleSearch.getSearchBox_textField();

        driver.get().element().type(searchBox, "chained type 1")
                .type(searchBox, "chained type 2")
                .typeAppend(searchBox, "345");

        driver.get().assertThat().element(searchBox).text().isEqualTo("chained type 2345");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod(){
        driver.get().quit();
    }
}
