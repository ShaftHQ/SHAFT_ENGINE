package testPackage01;

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

public class Test_uploadFile {
    ThreadLocal<WebDriver> driver = new ThreadLocal<>();

    @Test
    public void uploadFile_visibleUploadInput() {
        BrowserActions.navigateToURL(driver.get(), "http://demo.guru99.com/test/upload/");
        ElementActions.typeFileLocationForUpload(driver.get(), By.id("uploadfile_0"), "src/main/resources/images/shaft.png");
        new ElementActions(driver.get()).click(By.id("terms")).click(By.id("submitbutton"));
        Validations.assertThat().element(driver.get(), By.id("res")).attribute("Text").contains("1 file").perform();
    }

    @Test
    public void uploadFile_invisibleUploadInput() {
        BrowserActions.navigateToURL(driver.get(), "https://fineuploader.com/demos.html#gallery-view");
        ElementActions.typeFileLocationForUpload(driver.get(), By.xpath("//div[@id='fine-uploader-gallery']//input[@type='file']"), FileActions.getAbsolutePath("src/main/resources/images/", "shaft.png"));
        Validations.assertThat().element(driver.get(), By.xpath("//div[@id='fine-uploader-gallery']//div[@class='qq-thumbnail-wrapper']/img")).attribute("src").contains("data:image/png;base64").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(DriverFactory.getDriver());
    }

    @AfterMethod
    public void afterMethod() {
        BrowserActions.closeCurrentWindow(driver.get());
        driver.remove();
    }
}
