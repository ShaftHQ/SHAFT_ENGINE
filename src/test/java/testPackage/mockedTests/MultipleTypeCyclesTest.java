package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class MultipleTypeCyclesTest {
    ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    String testElement = "data:text/html,<input type=\"text\"><br><br>";
    By locator = SHAFT.GUI.Locator.hasTagName("input").build();

    @Test
    public void _01typeClearTypeTypeAppend() {
        driver.get().browser().navigateToURL(testElement);
        driver.get().element().type(locator, "first string")
                .type(locator, "second ")
                .typeAppend(locator, "string")
                .and().assertThat(locator).text().isEqualTo("second string")
                .perform();
    }



@Test
    public void _02clearUsingBackSpace(){
        SHAFT.Properties.flags.set().attemptClearBeforeTypingUsingBackspace(true);
        driver.get().browser().navigateToURL(testElement);
        driver.get().element().type(locator, "first string")
                              .type(locator, "Second string")
            .and().assertThat(locator).text().isEqualTo("Second string")
                  .perform();
    }

    @Test
    public void _03NoClearBeforeTypingTest(){
        SHAFT.Properties.flags.set().attemptClearBeforeTyping(false);
        SHAFT.Properties.flags.set().attemptClearBeforeTypingUsingBackspace(true);
        driver.get().browser().navigateToURL(testElement);
        driver.get().element().type(locator, "first string")
                .type(locator, " + Second string")
                .and().assertThat(locator).text().isEqualTo("first string + Second string")
                .perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());

    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
        driver.remove();
        //Resetting flags to default values after each method
        SHAFT.Properties.flags.set().attemptClearBeforeTyping(true);
        SHAFT.Properties.flags.set().attemptClearBeforeTypingUsingBackspace(false);
    }
}
