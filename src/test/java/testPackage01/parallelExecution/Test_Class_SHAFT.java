//package testPackage01.parallelExecution;
//
//import com.shaft.driver.SHAFT;
//import com.shaft.gui.browser.BrowserActions;
//import com.shaft.gui.element.ElementActions;
//import com.shaft.validation.Validations;
//import org.openqa.selenium.By;
//import org.testng.annotations.*;
//
//import java.nio.file.Paths;
//
//public class Test_Class_SHAFT {
//    private static final String html = Paths.get(System.getProperty("testDataFolderPath")).toUri().toString();
//    private static ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
//
//    private static final By firstName_input = By.id("fname");
//    private static final By lastName_input = By.id("lname");
//    private static final By submit_button = By.cssSelector("input[type='submit']");
//    private static final By output_label = By.cssSelector("div[style='word-wrap:break-word']");
//
//    @Test
//    public void test1() {
//        commonSteps("SHAFT", "Engine_1");
//    }
//    @Test
//    public void test2() {
//        commonSteps("SHAFT", "Engine_2");
//    }
//    @Test
//    public void test3() {
//        commonSteps("SHAFT", "Engine_3");
//    }
//    @Test
//    public void test4() {
//        commonSteps("SHAFT", "Engine_4");
//    }
//    @Test
//    public void test5() {
//        commonSteps("SHAFT", "Engine_5");
//    }
//    @Test
//    public void test6() {
//        commonSteps("SHAFT", "Engine_6");
//    }
//    @Test
//    public void test7() {
//        commonSteps("SHAFT", "Engine_7");
//    }
//    @Test
//    public void test8() {
//        commonSteps("SHAFT", "Engine_8");
//    }
//    @Test
//    public void test9() {
//        commonSteps("SHAFT", "Engine_9");
//    }
//    @Test
//    public void test10() {
//        commonSteps("SHAFT", "Engine_10");
//    }
//
//    private void commonSteps(String firstName, String lastName){
//
//        BrowserActions.navigateToURL(driver.get().getDriver(), html);
//        ElementActions.type(driver.get().getDriver(), firstName_input, firstName);
//        ElementActions.type(driver.get().getDriver(), lastName_input, lastName);
//        ElementActions.click(driver.get().getDriver(), submit_button);
//        Validations.assertThat().element(driver.get().getDriver(), output_label).text().contains("fname="+firstName+"&lname="+lastName).perform();
//
////        driver.get().browser().navigateToURL(html);
////        driver.get().element().type(firstName_input, firstName)
////                .type(lastName_input, lastName)
////                .click(submit_button);
////        driver.get().assertThat().element(output_label).text().contains("fname="+firstName+"&lname="+lastName).perform();
//    }
//
//    @BeforeMethod
//    public void beforeMethod() {
//        driver.set(new SHAFT.GUI.WebDriver());
//    }
//
//    @AfterMethod
//    public void afterMethod() {
//        driver.get().quit();
//    }
//}