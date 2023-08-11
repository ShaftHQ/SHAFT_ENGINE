package testPackage;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class BigPageActionsTest {
    SHAFT.GUI.WebDriver driver;

    @Test
    public void test() {
        driver.element().type(By.id("et_pb_contact_name_0"), "TEST_NAME")
                .clear(By.id("et_pb_contact_name_0"))
                .type(By.id("et_pb_contact_name_0"), "TEST_NAME")
                .type(By.id("et_pb_contact_email_0"), "email@email.email")
                .type(By.id("et_pb_contact_message_0"), """
                        This is a long message
                        it will have line breaks
                        and special characters ...######$%^&&*!!""")
                .type(By.id("et_pb_contact_name_1"), "TEST_NAME")
                .type(By.id("et_pb_contact_email_1"), "email@email.email")
                .type(By.id("et_pb_contact_message_1"), """
                        This is a long message
                        it will have line breaks
                        and special characters ...######$%^&&*!!""")
                .type(By.id("et_pb_contact_name_2"), "TEST_NAME")
                .type(By.id("et_pb_contact_email_2"), "email@email.email")
                .type(By.id("et_pb_contact_message_2"), """
                        This is a long message
                        it will have line breaks
                        and special characters ...######$%^&&*!!""")
                .captureScreenshot(By.id("et_pb_contact_message_2"))
                .and().browser().captureScreenshot();
        driver.assertThat().element(By.id("et_pb_contact_message_2")).text().isEqualTo("""
                This is a long message
                it will have line breaks
                and special characters ...######$%^&&*!!""").perform();
    }

    @Test
    public void test2() {
        driver.element().type(By.id("et_pb_contact_name_0"), "TEST_NAME")
                .type(By.id("et_pb_contact_email_0"), "email@email.email")
                .type(By.id("et_pb_contact_message_0"), """
                        This is a long message
                        it will have line breaks
                        and special characters ...######$%^&&*!!""")
                .type(By.id("et_pb_contact_name_1"), "TEST_NAME")
                .type(By.id("et_pb_contact_email_1"), "email@email.email")
                .type(By.id("et_pb_contact_message_1"), """
                        This is a long message
                        it will have line breaks
                        and special characters ...######$%^&&*!!""")
                .type(By.id("et_pb_contact_name_2"), "TEST_NAME")
                .type(By.id("et_pb_contact_email_2"), "email@email.email")
                .type(By.id("et_pb_contact_message_2"), """
                        This is a long message
                        it will have line breaks
                        and special characters ...######$%^&&*!!""")
                .captureScreenshot(By.id("et_pb_contact_message_2"))
                .and().browser().captureScreenshot();
        driver.assertThat().element(By.id("et_pb_contact_message_2")).text().isEqualTo("""
                This is a long message
                it will have line breaks
                and special characters ...######$%^&&*!!""").perform();
    }

    @Test
    public void testSettingVisualProprtitesDuringRuntime() {
        driver.element().type(By.id("et_pb_contact_name_0"), "TEST_NAME");
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Always");
        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("Regular");
                driver.element().clear(By.id("et_pb_contact_name_0"))
                .type(By.id("et_pb_contact_name_0"), "TEST_NAME")
                .type(By.id("et_pb_contact_email_0"), "email@email.email")
                .type(By.id("et_pb_contact_message_0"), """
                        This is a long message
                        it will have line breaks
                        and special characters ...######$%^&&*!!""");
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("Regular");
        driver.element().type(By.id("et_pb_contact_name_1"), "TEST_NAME")
                .type(By.id("et_pb_contact_email_1"), "email@email.email")
                .type(By.id("et_pb_contact_message_1"), """
                        This is a long message
                        it will have line breaks
                        and special characters ...######$%^&&*!!""");
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("ValidationPointsOnly");
        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("element");
        driver.element().type(By.id("et_pb_contact_name_2"), "TEST_NAME")
                .type(By.id("et_pb_contact_email_2"), "email@email.email")
                .type(By.id("et_pb_contact_message_2"), """
                        This is a long message
                        it will have line breaks
                        and special characters ...######$%^&&*!!""")
                .captureScreenshot(By.id("et_pb_contact_message_2"))
                .and().browser().captureScreenshot();
        driver.assertThat().element(By.id("et_pb_contact_message_2")).text().isEqualTo("""
                This is a long message
                it will have line breaks
                and special characters ...######$%^&&*!!""").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://ultimateqa.com/complicated-page");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        if (driver != null)
            driver.quit();
    }
}
