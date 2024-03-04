package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class SelectMethodTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private final By dropDownList = By.className("dropdown");

    @BeforeMethod
    protected void setUp() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL(SHAFT.Properties.paths.testData() + "selectDemo.html");
    }

    @Test
    public void testValidSelect() {
        driver.get().assertThat().element(dropDownList).isVisible();
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
        driver.get().quit();
    }

    private void clickDropDownList(String text) {
        driver.get().element().select(dropDownList, text);

    }
}
