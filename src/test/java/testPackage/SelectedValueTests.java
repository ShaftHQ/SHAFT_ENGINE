package testPackage;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class SelectedValueTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    String baseURL = "https://yari-demos.prod.mdn.mozit.cloud/en-US/docs/Web/HTML/Element/select/_sample_.";
    By select = By.tagName("select");

    @Test
    public void simpleSelect() {
        String textToSelect = "Third Value";
        driver.get().browser().navigateToURL(baseURL + "Basic_select.html")
                .performElementAction().select(select, textToSelect);
        driver.get().assertThat().element(select).attribute("selectedText").isEqualTo(textToSelect).perform();
    }

    @Test
    public void multipleSelect() {
        driver.get().browser().navigateToURL(baseURL + "Advanced_select_with_multiple_features.html")
                .performElementAction().select(select, "Dog")
                .select(select, "Cat");
        driver.get().assertThat().element(select).attribute("selectedText").isEqualTo("DogCat").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }

}
