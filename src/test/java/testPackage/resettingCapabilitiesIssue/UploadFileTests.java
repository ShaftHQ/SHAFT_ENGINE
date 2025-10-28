package testPackage.resettingCapabilitiesIssue;

import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class UploadFileTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void uploadFile_visibleUploadInput() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")) {
            driver.get().browser().navigateToURL("https://demo.guru99.com/test/upload/");
            driver.get().element().typeFileLocationForUpload(By.id("uploadfile_0"), "src/main/resources/images/shaft.png");
            driver.get().element().click(By.id("terms")).click(By.id("submitbutton"));
            Validations.assertThat().element(driver.get().getDriver(), By.id("res")).attribute("Text").contains("1 file").perform();
        }
    }


    @Test
    public void uploadFile_invisibleUploadInput() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")) {
            driver.get().browser().navigateToURL("https://demo.guru99.com/test/upload/");
            WebDriver nativeDriver = driver.get().getDriver();
            ((JavascriptExecutor) nativeDriver).executeScript("arguments[0].setAttribute('hidden', 'true')", nativeDriver.findElement(By.id("uploadfile_0")));

            driver.get().element().typeFileLocationForUpload(By.id("uploadfile_0"), "src/main/resources/images/shaft.png");
            driver.get().element().click(By.id("terms")).click(By.id("submitbutton"));
            Validations.assertThat().element(driver.get().getDriver(), By.id("res")).attribute("Text").contains("1 file").perform();
        }
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().setWindowSize(1920, 1080);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
