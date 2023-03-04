//package testPackage01.parallelExecution;
//
//import com.shaft.tools.io.ReportManager;
//import io.github.bonigarcia.wdm.WebDriverManager;
//import org.openqa.selenium.By;
//import org.openqa.selenium.WebDriver;
//import org.testng.Assert;
//import org.testng.annotations.*;
//
//import java.nio.file.Paths;
//
//public class Test_Class_NativeSelenium {
//    private static final String html = Paths.get("src/test/resources/testDataFiles/test.html").toUri().toString();
////    private static ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
//    private static ThreadLocal<WebDriver> driver = new ThreadLocal<>();
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
////        driver.get().browser().navigateToURL(html);
////        driver.get().element().type(firstName_input, firstName)
////                .type(lastName_input, lastName)
////                .click(submit_button);
////        driver.get().assertThat().element(output_label).text().contains("fname="+firstName+"&lname="+lastName).perform();
//
//        driver.get().navigate().to(html);
//        driver.get().findElement(firstName_input).clear();
//        driver.get().findElement(firstName_input).sendKeys(firstName);
//        driver.get().findElement(lastName_input).clear();
//        driver.get().findElement(lastName_input).sendKeys(lastName);
//        driver.get().findElement(submit_button).click();
//        var actualText = driver.get().findElement(output_label).getText();
//        var expectedText = "fname="+firstName+"&lname="+lastName;
//        ReportManager.log("Actual Text: "+ actualText);
//        ReportManager.log("Expected Text: "+ expectedText);
//        Assert.assertTrue(actualText.contains(expectedText));
//    }
//
//    @BeforeClass
//    public void beforeClass(){
////        System.setProperty("generateExtentReports", "false");
////        System.setProperty("setVerbose", "10");
////        System.setProperty("setParallel", "METHODS");
////        System.setProperty("setThreadCount", "4");
////        System.setProperty("executionAddress", "dockerized");
//
//
//    }
//
//    @AfterClass(alwaysRun = true)
//    public void afterClass(){
////        System.setProperty("generateExtentReports", "true");
////        System.setProperty("setVerbose", "1");
////        System.setProperty("setParallel", "NONE");
////        System.setProperty("setThreadCount", "1");
////        System.setProperty("executionAddress", "local");
//    }
//
//    @BeforeMethod
//    public void beforeMethod() {
////        driver.set(new SHAFT.GUI.WebDriver());
//        driver.set(WebDriverManager.chromedriver().create());
//    }
//
//    @AfterMethod(alwaysRun = true)
//    public void afterMethod() {
//        driver.get().quit();
//        driver.remove();
//    }
//}