package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class ThreadSafeGuiWizardTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private static final ThreadLocal<SHAFT.TestData.JSON> testData = new ThreadLocal<>();
    private final By locator = SHAFT.GUI.Locator.hasTagName("input").build();
    private double defaultElementIdentificationTimeout;

    @Test
    public void test() {
        steps(1);
    }

    @Test
    public void test2() {
        steps(2);
    }

    @Test
    public void test3() {
        steps(3);
    }

    @Test
    public void test4() {
        steps(4);
    }

    @Test
    public void test5() {
        steps(5);
    }

    @Test
    public void test6() {
        steps(6);
    }

    @Test
    public void test7() {
        steps(7);
    }

    @Test
    public void test8() {
        steps(8);
    }

    @Test
    public void test9() {
        steps(9);
    }

    @Test
    public void test10() {
        steps(10);
    }

    private void steps(int id) {
        String testElement = """
                data:text/html,<html>
                               <head>
                                 <title>SHAFT Mock</title>
                               </head>
                               <body>
                               <input type="text">
                               </body>
                               </html>""";
        driver.get().browser().navigateToURL(testElement)
                .and().verifyThat().title().isEqualTo("SHAFT Mock").perform();

        driver.get().element().type(locator, testData.get().getTestData("searchQuery") + id)
                .and().assertThat(locator).text().isEqualTo(testData.get().getTestData("searchQuery") + id).perform();
    }

    @SuppressWarnings("CommentedOutCode")
    @BeforeMethod
    public void beforeMethod() {
        //BrowserStack Web
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.platform.set().targetPlatform(Platform.MAC.name());
//        SHAFT.Properties.web.set().targetBrowserName(Browser.SAFARI.browserName());
//        SHAFT.Properties.browserStack.set().browserVersion("15.3");
//        SHAFT.Properties.browserStack.set().osVersion("Monterey");

        //Grid Web
//        SHAFT.Properties.platform.set().executionAddress("localhost:4444");
//        SHAFT.Properties.platform.set().targetPlatform(Platform.LINUX.name());
//        SHAFT.Properties.web.set().targetBrowserName(Browser.CHROME.browserName());

        driver.set(new SHAFT.GUI.WebDriver());
        testData.set(new SHAFT.TestData.JSON("simpleJSON.json"));
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
        driver.remove();
    }
}
