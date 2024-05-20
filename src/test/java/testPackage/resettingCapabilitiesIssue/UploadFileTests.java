package testPackage.resettingCapabilitiesIssue;

import com.shaft.cli.FileActions;
import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class UploadFileTests {
    private static final ThreadLocal<WebDriver> driver = new ThreadLocal<>();

    //    @Test
    public void uploadFile_visibleUploadInput() {
        new BrowserActions(driver.get()).navigateToURL("https://demo.guru99.com/test/upload/");
        new ElementActions(driver.get()).typeFileLocationForUpload(By.id("uploadfile_0"), "src/main/resources/images/shaft.png");
        new ElementActions(driver.get()).click(By.id("terms")).click(By.id("submitbutton"));
        Validations.assertThat().element(driver.get(), By.id("res")).attribute("Text").contains("1 file").perform();
    }

    @Test
    public void uploadFile_invisibleUploadInput() {
        new BrowserActions(driver.get()).navigateToURL("https://fineuploader.com/demos.html#gallery-view");
        new ElementActions(driver.get()).typeFileLocationForUpload(By.xpath("//div[@id='fine-uploader-gallery']//input[@type='file']"), FileActions.getInstance().getAbsolutePath("src/main/resources/images/", "shaft.png"));
        Validations.assertThat().element(driver.get(), By.xpath("//div[@id='fine-uploader-gallery']//div[@class='qq-thumbnail-wrapper']/img")).attribute("src").contains("data:image/png;base64").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new DriverFactory().getDriver());
        new BrowserActions(driver.get()).setWindowSize(1920, 1080);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        new BrowserActions(driver.get()).closeCurrentWindow();
    }
}
