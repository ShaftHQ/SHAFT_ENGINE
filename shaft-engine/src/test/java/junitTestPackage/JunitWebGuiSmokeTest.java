package junitTestPackage;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

class JunitWebGuiSmokeTest {
    private SHAFT.GUI.WebDriver driver;

    @BeforeEach
    void setup() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterEach
    void teardown() {
        try {
            if (driver != null) {
                driver.quit();
            }
        } finally {
            Properties.clearForCurrentThread();
        }
    }

    @Test
    void shaftGuiShouldOpenDataPageAndValidateElementText() {
        assertDoesNotThrow(() -> {
            driver.browser().navigateToURL("data:text/html;charset=utf-8,%3Cmain%3E%3Ch1%20id%3D%22ready%22%3EJUnit%20GUI%20Smoke%3C%2Fh1%3E%3C%2Fmain%3E");

            driver.element().assertThat(By.id("ready")).text().isEqualTo("JUnit GUI Smoke").perform();
        });
    }
}
