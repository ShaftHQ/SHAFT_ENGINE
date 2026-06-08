package junitTestPackage;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;

public class JunitTest {

    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    void testMethod() {
        driver.get().element().type(By.id("et_pb_contact_name_0"), "TEST_NAME")
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
                .and().browser().captureScreenshot()
                .and().element().assertThat(By.id("et_pb_contact_email_2")).text().isEqualTo("email@email.email").perform();
    }

    @Test
    void testMethod2() {
        driver.get().element().type(By.id("et_pb_contact_name_0"), "TEST_NAME")
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
                .and().browser().captureScreenshot()
                .and().element().assertThat(By.id("et_pb_contact_email_2")).text().isEqualTo("email@email.email").perform();
    }

    @BeforeEach
    void beforeEach() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("https://ultimateqa.com/complicated-page");
    }

    @AfterEach
    void afterEach() {
        driver.get().quit();
    }
}
