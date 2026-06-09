package fixture;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.Test;

public class TestNgWebConsumer {
    @Test
    public void compilesTestNgWebUsage() {
        SHAFT.GUI.WebDriver driver = null;
        if (driver != null) {
            driver.element().click(By.id("submit"));
        }
    }
}
