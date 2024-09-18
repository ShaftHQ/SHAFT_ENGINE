package testPackage.coreUpgradeScenarios;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class CoreUpgradeTests {
    SHAFT.GUI.WebDriver driver;

//    @Test
//    public void newCore(){
//        driver.newActions().type(By.id("et_pb_contact_name_0"), "TEST_NAME")
//                .type(By.id("et_pb_contact_email_0"), "email@email.email")
//                .type(By.id("et_pb_contact_message_0"), """
//                        This is a long message
//                        it will have line breaks
//                        and special characters ...######$%^&&*!!""")
//                .type(By.id("et_pb_contact_name_1"), "TEST_NAME")
//                .type(By.id("et_pb_contact_email_1"), "email@email.email")
//                .type(By.id("et_pb_contact_message_1"), """
//                        This is a long message
//                        it will have line breaks
//                        and special characters ...######$%^&&*!!""")
//                .type(By.id("et_pb_contact_name_2"), "TEST_NAME")
//                .type(By.id("et_pb_contact_email_2"), "email@email.email")
//                .type(By.id("et_pb_contact_message_2"), """
//                        This is a long message
//                        it will have line breaks
//                        and special characters ...######$%^&&*!!""");
//
//        driver.assertThat().element(By.id("et_pb_contact_message_2")).text().isEqualTo("""
//                This is a long message
//                it will have line breaks
//                and special characters ...######$%^&&*!!""").perform();
//    }

    @Test
    public void oldCore(){
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
                        and special characters ...######$%^&&*!!""");

        driver.assertThat().element(By.id("et_pb_contact_message_2")).text().isEqualTo("""
                This is a long message
                it will have line breaks
                and special characters ...######$%^&&*!!""").perform();
    }

    @BeforeMethod
    public void setUp() {
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://ultimateqa.com/complicated-page");
    }

    @AfterMethod
    public void tearDown() {
        driver.quit();
    }
}
