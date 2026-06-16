package testPackage.legacy;

import org.openqa.selenium.By;
import testPackage.TestPageServer;
import org.testng.annotations.Test;
import testPackage.Tests;

public class UploadFileTests extends Tests {
    @Test
    public void uploadFileViaVisibleInput() {
        driver.get().browser()
                .navigateToURL(uploadFixture())
                .element()
                .typeFileLocationForUpload(By.cssSelector("input[id='file-upload']"), "src/test/resources/testDataFiles/sample.pdf")
                .click(By.id("file-submit"))
                .assertThat(By.tagName("h3")).text().contains("File Uploaded!");
    }
    @Test
    public void uploadFileViaHiddenInput(){
        driver.get().browser()
                .navigateToURL(uploadFixture())
                .element()
                .typeFileLocationForUpload(By.id("upload-file"), "src/test/resources/testDataFiles/sample.pdf")
                .assertThat(By.tagName("body")).text().contains("1 selected file");
    }
    @Test
    public void uploadFileViaDragAndDrop() {
        driver.get().browser()
                .navigateToURL(uploadFixture())
                .element()
                .dropFileToUpload(By.cssSelector("label[for='upload-file']"), "src/test/resources/testDataFiles/sample.pdf")
                .assertThat(By.tagName("body")).text().contains("1 selected file");
    }

    /**
     * This case is disabled because it fails due to the presence of other hidden inputs on the page.
     */
    @Test(enabled = false)
    public void uploadFileViaDragAndDropWithOtherHiddenInputsPresent() {
        driver.get().browser()
                .navigateToURL("https://the-internet.herokuapp.com/upload")
                .element()
                .dropFileToUpload(By.id("drag-drop-upload"), "src/test/resources/testDataFiles/sample.pdf")
                .assertThat(By.id("content")).text().contains("✔");
    }

    @Test
    public void uploadFileViaAjax(){
        driver.get().browser()
                .navigateToURL(uploadFixture())
                .element()
                .typeFileLocationForUpload(By.id("ajaxUpload"), "src/test/resources/testDataFiles/youtube.png")
                .assertThat(By.cssSelector("div[id='ImageList'] > img")).domAttribute("src").contains("youtube.png");
    }

    private static String uploadFixture() {
        return TestPageServer.url("uploadFixture.html");
    }
}
