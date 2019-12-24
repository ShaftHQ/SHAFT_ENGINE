package testPackage01;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionType;

public class Test_downloadFileToCustomDirectory {
    WebDriver driver;

    @Test
    public void downloadFile() {
	BrowserActions.navigateToURL(driver, "https://sample-videos.com/download-sample-doc-file.php");

//	ElementActions.click(driver, By.xpath("//a[@href='doc/Sample-doc-file-100kb.doc']"));
//	Assertions.assertFileExists(System.getProperty("downloadsFolderPath"), "SampleDOCFile_100kb.doc", 10, true);

	ElementActions.click(driver, By.xpath("//a[@href='doc/Sample-doc-file-5000kb.doc']"));
	Assertions.assertFileExists(System.getProperty("downloadsFolderPath"), "SampleDOCFile_5000kb.doc", 100,
		AssertionType.POSITIVE);

    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
	driver = BrowserFactory.getBrowser();
    }

    @AfterClass
    public void afterClass() {
	BrowserActions.closeCurrentWindow(driver);
    }
}