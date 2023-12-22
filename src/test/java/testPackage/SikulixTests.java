package testPackage;

import com.shaft.cli.TerminalActions;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.SikuliActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.WebDriver;
import org.sikuli.script.App;
import org.sikuli.script.Key;
import org.testng.annotations.Test;

public class SikulixTests {
    App calculator;
    String pathToCalculatorElementsFolder = "src/test/resources/DynamicObjectRepository/calculator/";

    //@Test
    @SuppressWarnings("CommentedOutCode")
    public void sampleWithSeleniumWebDriver() {
        new BrowserActions().navigateToURL("https://www.google.com/ncr", "https://www.google.com");
//        byte[] searchTextBox = ScreenshotManager.takeElementScreenshot(driver, By.xpath("//input[@name='q']"));
//        ElementActions.performSikuliAction(searchTextBox).type("SHAFT_Engine trial using SikuliX1" + Key.ENTER);
        String pathToTargetElementImage = "src/test/resources/DynamicObjectRepository/" + "sikuli_googleHome_searchBox_text.PNG";
        new SikuliActions().click(pathToTargetElementImage).type(pathToTargetElementImage, "SHAFT_Engine trial using SikuliX1" + Key.ENTER);
        new BrowserActions().closeCurrentWindow();
    }

    @Test
    public void sampleWithSeleniumAndYoutube() {
        WebDriver driver = new DriverFactory().getDriver();
        new BrowserActions(driver).navigateToURL("https://www.youtube.com/watch?v=6FbpNgZ8fZ8&t=2s");
        String pathToTargetElementImage = SHAFT.Properties.paths.testData() + "sikulixElements/youtube.png";
        new ElementActions(driver).performSikuliAction().click(pathToTargetElementImage);
        Validations.assertThat().browser(driver).url().isEqualTo("https://www.youtube.com/").perform();
    }

    @Test
    public void sampleWithDesktopApplication() {
        String result = new SikuliActions(calculator).click(pathToCalculatorElementsFolder + "1.png")
                .click(pathToCalculatorElementsFolder + "+.png")
                .click(pathToCalculatorElementsFolder + "3.png")
                .click(pathToCalculatorElementsFolder + "=.png")
                .getText(pathToCalculatorElementsFolder + "result.png");
        Validations.assertThat().object(result).isEqualTo("4").perform();
    }

    //@BeforeClass
    public void openApplication() {
        new TerminalActions().performTerminalCommand("calc");
        calculator = DriverFactory.getSikuliApp("Calculator");
    }

    //@AfterClass(alwaysRun = true)
    public void closeApplication() {
        DriverFactory.closeSikuliApp(calculator);
    }

}
