package fixture;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;

class JUnitWebConsumer {
    @Test
    void compilesJUnitWebUsage() {
        SHAFT.GUI.WebDriver driver = null;
        if (driver != null) {
            driver.element().type(By.name("query"), "SHAFT");
        }
    }
}
