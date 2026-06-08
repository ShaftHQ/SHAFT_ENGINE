package junitTestPackage;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.RepeatedTest;
import org.openqa.selenium.By;

public class JunitParallelizationTests {
    SHAFT.GUI.WebDriver driver;
    By pageTitleLabel = SHAFT.GUI.Locator.hasTagName("h1").build();

    @BeforeEach
    void setup() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterEach
    void tearDown() {
        driver.quit();
    }

    @RepeatedTest(20)
    void test() {
        System.out.println("Test " + Thread.currentThread().threadId());
        driver.browser().navigateToURL("https://shafthq.github.io/")
                .and().element().assertThat(pageTitleLabel).isVisible();
    }
}
