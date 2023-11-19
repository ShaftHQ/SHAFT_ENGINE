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
    public void _02clearUsingNativeClearOnly(){
        driver.get().browser().navigateToURL("https://testkru.com/Elements/TextFields");
        driver.get().element().type(By.xpath("(//input)[7]"),"text entered By Test")
                .and().assertThat(By.xpath("(//input)[7]")).text().isEqualTo("text entered By Test")
                .perform();
                }


@Test
    public void _03clearUsingBackSpaceOnly(){
        SHAFT.Properties.flags.set().attemptClearBeforeTyping(true);
        SHAFT.Properties.flags.set().attemptClearBeforeTypingUsingBackspace(true);
        driver.get().browser().navigateToURL("https://testkru.com/Elements/TextFields");
        driver.get().element().type(By.xpath("(//input)[7]"),"text entered By me")
                .and().assertThat(By.xpath("(//input)[7]")).text().isEqualTo("text entered By me")
                .perform();
    }

    @Test
    public void _04clearUsingBackSpaceOnly2(){
        SHAFT.Properties.flags.set().attemptClearBeforeTyping(false);
        SHAFT.Properties.flags.set().attemptClearBeforeTypingUsingBackspace(true);
        driver.get().browser().navigateToURL("https://testkru.com/Elements/TextFields");
        driver.get().element().type(By.xpath("(//input)[7]"),"text entered By me")
                .and().assertThat(By.xpath("(//input)[7]")).text().isEqualTo("text entered By me")
                .perform();
    }
@Test
    public void _05noClearBeforeTypingNoForceCheckAfterTyping(){
        SHAFT.Properties.flags.set().attemptClearBeforeTyping(false);
        SHAFT.Properties.flags.set().attemptClearBeforeTypingUsingBackspace(false);
        SHAFT.Properties.flags.set().forceCheckTextWasTypedCorrectly(false);
        driver.get().browser().navigateToURL("https://testkru.com/Elements/TextFields");
        driver.get().element().type(By.xpath("(//input)[7]")," text entered By me")
                .and().assertThat(By.xpath("(//input)[7]")).text().isEqualTo("Codekru text entered By me")
                .perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(SHAFT.GUI.WebDriver.getInstance());

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
