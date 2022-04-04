package testPackage01.dsl;

import com.shaft.driver.DriverFactory;
import com.shaft.dsl.gui.Button;
import com.shaft.dsl.gui.CheckBox;
import com.shaft.dsl.gui.Element;
import com.shaft.dsl.gui.Label;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_Dsl {
    WebDriver driver;

    @BeforeMethod
    void beforeMethod() {
        driver = DriverFactory.getDriver();
    }

    @AfterMethod
    void afterMethod() {
        DriverFactory.closeAllDrivers();
    }

    @Test
    void testLabel() {
        driver.navigate().to("http://the-internet.herokuapp.com/");
        Element.setDriver(driver);
        Label label = new Label(By.xpath("//*[contains(text(),'Welcome to the-internet')]"));
        label.shouldHaveText("Welcome to the-internet");
    }

    @Test
    void testButton()
    {
        BrowserActions.navigateToURL(driver,"http://the-internet.herokuapp.com/");
        ElementActions.click(driver,By.linkText("Add/Remove Elements"));
        Element.setDriver(driver);
        Button btn= new Button( By.xpath("//button[text()='Add Element']"));
        btn.shouldHaveText("Add Element");
        btn.click();
        btn.isEnabled();
        Button btn1= new Button(By.xpath("//button[text()='Delete']"));
        btn1.shouldHaveText("Delete");
    }
    @Test
    void testCheckBox()
    {
        Element.setDriver(driver);
        BrowserActions.navigateToURL(driver,"http://the-internet.herokuapp.com/");
        ElementActions.click(driver,By.linkText("Checkboxes"));
        CheckBox cb1 = new CheckBox(By.xpath("(//INPUT[@type='checkbox'])[1]"),By.xpath("(//INPUT[@type='checkbox'])[1]"));
        cb1.click();
        CheckBox cb2 = new CheckBox(By.xpath("(//INPUT[@type='checkbox'])[2]"),By.xpath("(//INPUT[@type='checkbox'])[2]"));
        cb2.click();
    }
}
