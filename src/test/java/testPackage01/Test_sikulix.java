package testPackage01;

import com.shaft.cli.TerminalActions;
import com.shaft.driver.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import org.openqa.selenium.WebDriver;
import org.sikuli.script.App;
import org.sikuli.script.Key;
import org.testng.annotations.Test;

public class Test_sikulix {
    App calculator;
    String pathToCalculatorElementsFolder = "src/test/resources/DynamicObjectRepository/calculator/";

    //@Test
    public void sampleWithSeleniumWebDriver() {
        WebDriver driver = BrowserFactory.getBrowser();
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
//        byte[] searchTextBox = ScreenshotManager.takeElementScreenshot(driver, By.xpath("//input[@name='q']"));
//        ElementActions.performSikuliAction(searchTextBox).type("SHAFT_Engine trial using SikuliX1" + Key.ENTER);
        String pathToTargetElementImage = "src/test/resources/DynamicObjectRepository/" + "sikuli_googleHome_searchBox_text.PNG";
        ElementActions.performSikuliAction().click(pathToTargetElementImage).type(pathToTargetElementImage, "SHAFT_Engine trial using SikuliX1" + Key.ENTER);
        BrowserActions.closeCurrentWindow(driver);
    }

    @Test
    public void sampleWithSeleniumAndYoutube() {
        WebDriver driver = BrowserFactory.getBrowser();
        BrowserActions.navigateToURL(driver, "https://www.youtube.com/watch?v=6FbpNgZ8fZ8&t=2s");
        String pathToTargetElementImage = System.getProperty("testDataFolderPath") + "sikulixElements/youtube.png";
        ElementActions.performSikuliAction().click(pathToTargetElementImage);
        Assertions.assertBrowserAttribute(driver, "url", "https://www.youtube.com/");
    }

    @Test
    public void sampleWithDesktopApplication() {
        String result = ElementActions.performSikuliAction(calculator).click(pathToCalculatorElementsFolder + "1.png")
                .click(pathToCalculatorElementsFolder + "+.png")
                .click(pathToCalculatorElementsFolder + "3.png")
                .click(pathToCalculatorElementsFolder + "=.png")
                .getText(pathToCalculatorElementsFolder + "result.png");
        Assertions.assertEquals("4", result);
    }

    //@BeforeClass
    public void openApplication() {
        new TerminalActions().performTerminalCommand("calc");
        calculator = DriverFactoryHelper.getSikuliApp("Calculator");
    }

    //@AfterClass
    public void closeApplication() {
    	DriverFactoryHelper.closeSikuliApp(calculator);
    }

}
