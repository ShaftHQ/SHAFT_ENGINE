package testPackage01;

import com.shaft.cli.FileActions;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
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
        ElementActions.typeFileLocationForUpload(driver.get(), By.id("uploadfile_0"), FileActions.getAbsolutePath("src/main/resources/images/", "shaft.png"));
        new ElementActions(driver.get()).click(By.id("terms")).click(By.id("submitbutton"));
        Assertions.assertElementAttribute(driver.get(), By.id("res"), "Text", "1 file", Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.POSITIVE);
    }

    @Test
    public void uploadFile_invisibleUploadInput() {
        BrowserActions.navigateToURL(driver.get(), "https://fineuploader.com/demos.html#gallery-view");
        ElementActions.typeFileLocationForUpload(driver.get(), By.xpath("//div[@id='fine-uploader-gallery']//input[@type='file']"), FileActions.getAbsolutePath("src/main/resources/images/", "shaft.png"));
        Assertions.assertElementAttribute(driver.get(), By.xpath("//div[@id='fine-uploader-gallery']//div[@class='qq-thumbnail-wrapper']/img"), "src", "data:image/png;base64", Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.POSITIVE);
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(BrowserFactory.getBrowser());
    }

    @AfterMethod
    public void afterMethod() {
        BrowserActions.closeCurrentWindow(driver.get());
        driver.remove();
    }
}
