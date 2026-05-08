package testPackage.resettingCapabilitiesIssue;

import com.shaft.driver.SHAFT;
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
            driver.get().browser().navigateToURL("https://the-internet.herokuapp.com/upload");
            driver.get().element().typeFileLocationForUpload(By.cssSelector("input[id='file-upload']"), "src/main/resources/images/shaft.png");
            driver.get().element().click(By.id("file-submit"));
            driver.get().assertThat().element(By.tagName("h3")).text().contains("File Uploaded!");
        }
    }


    @Test
    public void uploadFile_invisibleUploadInput() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")) {
            driver.get().browser().navigateToURL("https://the-internet.herokuapp.com/upload");
            WebDriver nativeDriver = driver.get().getDriver();
            ((JavascriptExecutor) nativeDriver).executeScript("arguments[0].setAttribute('hidden', 'true')", nativeDriver.findElement(By.cssSelector("input[id='file-upload']")));

            driver.get().element().typeFileLocationForUpload(By.cssSelector("input[id='file-upload']"), "src/main/resources/images/shaft.png");
            driver.get().element().click(By.id("file-submit"));
            driver.get().assertThat().element(By.tagName("h3")).text().contains("File Uploaded!");
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
