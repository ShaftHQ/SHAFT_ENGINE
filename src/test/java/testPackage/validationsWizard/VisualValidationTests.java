package testPackage.validationsWizard;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.ScreenshotHelper;
import com.shaft.validation.ValidationEnums;
import org.openqa.selenium.By;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

@Test public class VisualValidationTests {
    protected static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    String firstRunElementReferenceFilePath;
    By firstRunElement = By.cssSelector("img[alt='SHAFT_Engine']");

    public void visualValidation_1stRun(){
        var fileActions = SHAFT.CLI.file();
        firstRunElementReferenceFilePath = ScreenshotHelper.getAiAidedElementIdentificationFolderPath()
                + ImageProcessingActions.formatElementLocatorToImagePath(firstRunElement)
                + ".png";
        fileActions.deleteFile(firstRunElementReferenceFilePath);
        driver.get().element().assertThat(firstRunElement).matchesReferenceImage(ValidationEnums.VisualValidationEngine.EXACT_OPENCV);
    }

    @Test (dependsOnMethods = {"visualValidation_1stRun"})
    public void visualValidation_2ndRun_expectedToPass(){
        driver.get().element().assertThat(firstRunElement).matchesReferenceImage(ValidationEnums.VisualValidationEngine.EXACT_OPENCV);
    }

    @Test (dependsOnMethods = {"visualValidation_2ndRun_expectedToPass"}, expectedExceptions = {AssertionError.class})
    public void visualValidation_2ndRun_expectedToFail(){
        SHAFT.CLI.file().writeToFile(firstRunElementReferenceFilePath, new byte[0]);
        driver.get().element().assertThat(firstRunElement).matchesReferenceImage(ValidationEnums.VisualValidationEngine.EXACT_OPENCV);
    }

    @BeforeMethod
    public void navigate(){
        driver.get().browser().navigateToURL("https://shafthq.github.io/");
    }

    @BeforeClass
    public void init() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterClass(alwaysRun = true)
    public void tear() {
        driver.get().quit();
    }
}
