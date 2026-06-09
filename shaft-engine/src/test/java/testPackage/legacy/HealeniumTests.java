package testPackage.legacy;

import com.epam.healenium.SelfHealingDriver;
import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class HealeniumTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    /*
    TODO:
. optimize and reduce number of findElement calls done by SHAFT to increase overall performance
. research if healenium report can be generated manually and attached to the allure report
. capture healenium logs in shaft logs
. create user guide or video on how to use healenium with shaft and the needed manual step to initialize healenium backend
     */

    /*
    Steps to use Healinium with SHAFT
    1. Run the docker-compose file under src/main/resources/docker-compose/healenium-backend.yml
    2. Ensure that the 'heal-enabled' property is set to 'true'
    3. Run this test class to ensure that everything is working as expected
     */

    @Test(enabled = false)
    public void test(){
        //confirm that the self healing driver has been initialized successfully
        if (driver.get().getDriver() instanceof SelfHealingDriver) {
            ReportManager.log("Healenium's Self Healing Driver initialized successfully.");
        } else{
            ReportManager.log("This is a normal WebDriver instance.");
        }

        //navigate to target url
        new BrowserActions(driver.get().getDriver()).navigateToURL("https://www.google.com/ncr", "https://www.google.com");

        //define element locator
        By googleLogo_image = By.xpath("//*[@alt='Google']");

        //confirm that the locator is working
        try {
            Validations.assertThat().element(driver.get().getDriver(), googleLogo_image).exists().perform();
            ReportManager.log("Successfully Found Element on initial check");
        }catch (AssertionError e){
            Validations.assertThat().forceFail()
                    .withCustomReportMessage("Failed To Find Element on initial check").perform();
        }

        //break the locator
        ((JavascriptExecutor) driver.get().getDriver()).executeScript("arguments[0].setAttribute('alt', 'NotGoogle')", driver.get().getDriver().findElement(googleLogo_image));

        //confirm that self healing is working
        try {
            Validations.assertThat().element(driver.get().getDriver(), googleLogo_image).exists().perform();
            ReportManager.log("Successfully Healed the locator and Found Element.");
        }catch (AssertionError e){
            Validations.assertThat().forceFail()
                    .withCustomReportMessage("Failed To Heal locator and Find Element").perform();
        }
    }

    @BeforeMethod
    public void beforeMethod(){
//        SHAFT.Properties.healenium.set().healEnabled(true);
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod(){
        driver.get().quit();
    }
}
