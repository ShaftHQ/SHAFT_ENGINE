package testPackage01;

import com.shaft.cli.TerminalActions;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.WebDriver;
import org.sikuli.script.Key;
import org.testng.annotations.Test;

public class Test_sikulix {
    @Test
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
    public void sampleWithDesktopApplication() {
        String pathToCalculatorElementsFolder = "src/test/resources/DynamicObjectRepository/calculator/";
        ReportManager.log("Launching Calculator App...");
        new TerminalActions().performTerminalCommand("calc \n");
        ElementActions.performSikuliAction().click(pathToCalculatorElementsFolder + "1.png")
                .click(pathToCalculatorElementsFolder + "+.png")
                .click(pathToCalculatorElementsFolder + "2.png")
                .click(pathToCalculatorElementsFolder + "=.png");
        //how will I do the assertion?
    }

}
