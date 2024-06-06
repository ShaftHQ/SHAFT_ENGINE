package testPackage.resettingCapabilitiesIssue;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class UploadFileTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    //    @Test
    public void uploadFile_visibleUploadInput() {
        driver.get().browser().navigateToURL("https://demo.guru99.com/test/upload/");
        driver.get().element().typeFileLocationForUpload(By.id("uploadfile_0"), "src/main/resources/images/shaft.png");
        driver.get().element().click(By.id("terms")).click(By.id("submitbutton"));
        Validations.assertThat().element(driver.get().getDriver(), By.id("res")).attribute("Text").contains("1 file").perform();
    }

    @Test
    public void uploadFile_invisibleUploadInput() {
        driver.get().browser().navigateToURL("https://fineuploader.com/demos.html#gallery-view");
        driver.get().element().typeFileLocationForUpload(By.xpath("//div[@id='fine-uploader-gallery']//input[@type='file']"), FileActions.getInstance().getAbsolutePath("src/main/resources/images/", "shaft.png"));
        Validations.assertThat().element(driver.get().getDriver(), By.xpath("//div[@id='fine-uploader-gallery']//div[@class='qq-thumbnail-wrapper']/img")).attribute("src").contains("data:image/png;base64").perform();
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
