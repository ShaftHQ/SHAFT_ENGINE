//package testPackage01.appium;
//
//import com.shaft.driver.DriverFactory;
//import com.shaft.driver.SHAFT;
//import io.appium.java_client.AppiumDriver;
//import io.appium.java_client.remote.AndroidMobileCapabilityType;
//import io.appium.java_client.remote.MobileCapabilityType;
//import org.openqa.selenium.By;
//import org.openqa.selenium.WebDriver;
//import org.openqa.selenium.remote.DesiredCapabilities;
//import org.testng.annotations.AfterMethod;
//import org.testng.annotations.BeforeMethod;
//import org.testng.annotations.Test;
//
//import java.net.MalformedURLException;
//import java.net.URL;
//
//public class SeeTestCloud_Test {
//    SHAFT.GUI.WebDriver driver;
//    @BeforeMethod
//    public void beforeMethod(){
//        driver = new SHAFT.GUI.WebDriver();
//    }
//    @Test
//    public void quickStartAndroidNativeDemo() {
//        driver.element().type(By.xpath("//*[@id='usernameTextField']"), "company")
//                        .type(By.xpath("//*[@id='passwordTextField']"), "company")
//                        .click(By.xpath("//*[@id='loginButton']"));
//    }
//    @AfterMethod
//    public void afterMethod(){
//        driver.quit();
//    }
//
//    @Test
//    public void nativeAppiumSetup() throws MalformedURLException {
//        String accessKey = "";
//        WebDriver driver;
//        DesiredCapabilities dc = new DesiredCapabilities();
//        dc.setCapability("testName", "Quick Start Android Native Demo");
//        dc.setCapability("accessKey", accessKey);
//        dc.setCapability("deviceQuery", "@os='android' and @category='PHONE'");
//        dc.setCapability(MobileCapabilityType.APP, "cloud:com.experitest.ExperiBank/.LoginActivity");
//        dc.setCapability(AndroidMobileCapabilityType.APP_PACKAGE, "com.experitest.ExperiBank");
//        dc.setCapability(AndroidMobileCapabilityType.APP_ACTIVITY, ".LoginActivity");
//        driver = new AppiumDriver(new URL("https://server.company.com/wd/hub"), dc);
//        driver.findElement(By.xpath("//*[@id='usernameTextField']")).sendKeys("company");
//        driver.findElement(By.xpath("//*[@id='passwordTextField']")).sendKeys("company");
//        driver.findElement(By.xpath("//*[@id='loginButton']")).click();
//        driver.quit();
//    }
//}
