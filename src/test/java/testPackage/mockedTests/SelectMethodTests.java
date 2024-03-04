package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class SelectMethodTests {
    private SHAFT.GUI.WebDriver driver;
    private final By dropDownList = By.className("dropdown");

    @BeforeMethod
    protected void setUp() {
        driver= new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL(SHAFT.Properties.paths.testData() + "selectDemo.html");
    }

    @Test
    public void testValidSelect() {
        clickDropDownList("Div 1");
        clickDropDownList("Div 2");
        clickDropDownList("Div 3");
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void testInvalidSelect() {
        clickDropDownList("Div 1000");

    }

    @AfterMethod
    protected void tearDown() {
        driver.quit();
    }

    private void clickDropDownList(String text) {
        driver.element().select(dropDownList, text);

    }
}
