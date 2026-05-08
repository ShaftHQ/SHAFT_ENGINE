package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class BigPageActionsTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void virtualThreads_1_sequential() {
        driver.get().element().type(By.id("et_pb_contact_name_0"), "TEST_NAME");
        driver.get().element().type(By.id("et_pb_contact_email_0"), "email@email.email");
        driver.get().element().type(By.id("et_pb_contact_message_0"), """
                This is a long message
                it will have line breaks
                and special characters ...######$%^&&*!!""");

        driver.get().assertThat().element(By.id("et_pb_contact_message_0")).text().isEqualTo("""
                This is a long message
                it will have line breaks
                and special characters ...######$%^&&*!!""").perform();
    }

//    @Test
//    public void virtualThreads_2_oneService() throws InterruptedException {
//        try (ExecutorService myExecutor = Executors.newVirtualThreadPerTaskExecutor()) {
//            myExecutor.submit(() -> {
//                driver.get().element().type(By.id("et_pb_contact_name_0"), "TEST_NAME");
//                driver.get().element().type(By.id("et_pb_contact_email_0"), "email@email.email");
//                driver.get().element().type(By.id("et_pb_contact_message_0"), """
//                        This is a long message
//                        it will have line breaks
//                        and special characters ...######$%^&&*!!""");
//            }).get();
//        } catch (ExecutionException e) {
//            throw new RuntimeException(e);
//        }
//        driver.get().assertThat().element(By.id("et_pb_contact_message_0")).text().isEqualTo("""
//                This is a long message
//                it will have line breaks
//                and special characters ...######$%^&&*!!""").perform();
//    }
//
//    @Test
//    public void virtualThreads_3_multiServices() {
//        try (ExecutorService myExecutor = Executors.newVirtualThreadPerTaskExecutor()) {
//            var action1 = myExecutor.submit(() -> driver.get().element().type(By.id("et_pb_contact_name_0"), "TEST_NAME"));
//            var action2 = myExecutor.submit(() -> driver.get().element().type(By.id("et_pb_contact_email_0"), "email@email.email"));
//            var action3 = myExecutor.submit(() -> driver.get().element().type(By.id("et_pb_contact_message_0"), """
//                    This is a long message
//                    it will have line breaks
//                    and special characters ...######$%^&&*!!"""));
//
//            //synchronization point
//            action1.get();
//            action2.get();
//            action3.get();
//        } catch (ExecutionException | InterruptedException e) {
//            throw new RuntimeException(e);
//        }
//
//        driver.get().assertThat().element(By.id("et_pb_contact_message_0")).text().isEqualTo("""
//                This is a long message
//                it will have line breaks
//                and special characters ...######$%^&&*!!""").perform();
//    }
//
//    @Test
//    public void virtualThreads_4_multiServicesBuiltInAsync() {
//        driver.get().async().element().type(By.id("et_pb_contact_name_0"), "TEST_NAME")
//                .type(By.id("et_pb_contact_email_0"), "email@email.email")
//                .type(By.id("et_pb_contact_message_0"), """
//                        This is a long message
//                        it will have line breaks
//                        and special characters ...######$%^&&*!!""")
//                .perform();
//
//        driver.get().assertThat().element(By.id("et_pb_contact_message_0")).text().isEqualTo("""
//                This is a long message
//                it will have line breaks
//                and special characters ...######$%^&&*!!""").perform();
//    }


    @Test
    public void bigTest_1_Sequential() {
        this.testSteps();
        driver.get().assertThat().element(By.id("et_pb_contact_message_2")).text().isEqualTo("""
                This is a long message
                it will have line breaks
                and special characters ...######$%^&&*!!""").perform();
    }

//    @Test
//    public void bigTest_2_VirtualThreads_OneService() {
//        try (ExecutorService myExecutor = Executors.newVirtualThreadPerTaskExecutor()) {
//            myExecutor.submit(this::testSteps).get();
//        } catch (ExecutionException | InterruptedException e) {
//            throw new RuntimeException(e);
//        }
//        driver.get().assertThat().element(By.id("et_pb_contact_message_2")).text().isEqualTo("""
//                This is a long message
//                it will have line breaks
//                and special characters ...######$%^&&*!!""").perform();
//    }
//
//    @Test
//    public void bigTest_3_VirtualThreads_multiServicesBuiltInAsync() {
//        driver.get().async().element().type(By.id("et_pb_contact_name_0"), "TEST_NAME")
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
//                        and special characters ...######$%^&&*!!""")
//                .perform();
//
//        driver.get().element()
//                .captureScreenshot(By.id("et_pb_contact_message_2"))
//                .and().browser().captureScreenshot();
//
//        driver.get().assertThat().element(By.id("et_pb_contact_message_2")).text().isEqualTo("""
//                This is a long message
//                it will have line breaks
//                and special characters ...######$%^&&*!!""").perform();
//    }

    private void testSteps() {
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
                .and().browser().captureScreenshot();
    }


    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("https://ultimateqa.com/complicated-page");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        if (driver.get() != null)
            driver.get().quit();
    }
}
