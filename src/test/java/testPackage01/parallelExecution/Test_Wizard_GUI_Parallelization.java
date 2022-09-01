//package testPackage01.parallelExecution;
//
//import com.shaft.driver.SHAFT;
//import com.shaft.gui.browser.BrowserActions;
//import com.shaft.gui.element.ElementActions;
//import com.shaft.validation.Validations;
//import org.openqa.selenium.By;
//import org.openqa.selenium.Keys;
//import org.testng.annotations.AfterMethod;
//import org.testng.annotations.BeforeClass;
//import org.testng.annotations.BeforeMethod;
//import org.testng.annotations.Test;
//
//public class Test_Wizard_GUI_Parallelization {
////    SHAFT.GUI.WebDriver driver;
//    ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
//    SHAFT.TestData.JSON testData;
//
//    By searchBox = By.name("q");
//    By resultStats = By.id("result-stats");
//
//    @Test
//    public void test1() {
//        commonSteps(1);
//    }
//
//    @Test
//    public void test2() {
//        commonSteps(2);
//    }
//
//    @Test
//    public void test3() {
//        commonSteps(3);
//    }
//
//    @Test
//    public void test4() {
//        commonSteps(4);
//    }
//
//    @Test
//    public void test5() {
//        commonSteps(5);
//    }
//
//    @Test
//    public void test6() {
//        commonSteps(6);
//    }
//
//    @Test
//    public void test7() {
//        commonSteps(7);
//    }
//
//    @Test
//    public void test8() {
//        commonSteps(8);
//    }
//
//    @Test
//    public void test9() {
//        commonSteps(9);
//    }
//
//    @Test
//    public void test10() {
//        commonSteps(10);
//    }
//
//    private void commonSteps(int queryPostfix){
//        //TODO: legacy syntax is working as expected with 100% passing rate, wizard syntax is still inconsistent
//
////        driver.get().browser().navigateToURL("https://www.google.com/");
////        driver.get().verifyThat().browser().title().isEqualTo("Google").perform();
////        driver.get().element().type(searchBox, testData.getTestData("searchQuery")+"_"+queryPostfix)
////                .keyPress(searchBox, Keys.ENTER);
////        driver.get().assertThat().element(resultStats).text().doesNotEqual("").withCustomReportMessage("Check that result stats is not empty").perform();
//
//        BrowserActions.navigateToURL(driver.get().getDriver(), "https://www.google.com/");
//        Validations.verifyThat().browser(driver.get().getDriver()).title().isEqualTo("Google").perform();
//        ElementActions.type(driver.get().getDriver(), searchBox, testData.getTestData("searchQuery")+"_"+queryPostfix);
//        ElementActions.keyPress(driver.get().getDriver(), searchBox, Keys.ENTER);
//        Validations.assertThat().element(driver.get().getDriver(), resultStats).text().doesNotEqual("").withCustomReportMessage("Check that result stats is not empty").perform();
//    }
//
//    @BeforeClass
//    public void beforeClass() {
//        testData = new SHAFT.TestData.JSON("simpleJSON.json");
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
