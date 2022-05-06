//package testPackage01;
//
//import com.shaft.driver.SHAFT;
//import org.openqa.selenium.By;
//import org.testng.annotations.AfterMethod;
//import org.testng.annotations.BeforeMethod;
//import org.testng.annotations.Test;
//
//public class Test_shutterbug {
//    SHAFT.GUI.WebDriver driver;
//
//    @Test
//    public void test(){
//        driver.browser().navigateToURL("https://www.google.com/");
//        driver.assertThat().element(By.xpath("//img[@alt='Google']"))
//                .matchesReferenceImage()
//                .perform();
//    }
//    @BeforeMethod
//    public void beforeMethod(){
//        driver = new SHAFT.GUI.WebDriver();
//    }
//    @AfterMethod
//    public void afterMethod(){
//        driver.quit();
//    }
//}
