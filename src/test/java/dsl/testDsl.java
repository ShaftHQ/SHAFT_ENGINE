package dsl;

import com.shaft.driver.DriverFactory;
import com.shaft.dsl.webElements.Button;
import com.shaft.dsl.webElements.CheckBox;
import com.shaft.dsl.webElements.Label;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;

import java.sql.DriverAction;

public class testDsl {
    @Test
    void testLabel()
    {
        WebDriver driver = DriverFactory.getDriver();
        driver.navigate().to("http://the-internet.herokuapp.com/");
        Label label= new Label(driver, By.xpath("//*[contains(text(),'Welcome to the-internet')]"));
        label.shouldHaveText("Welcome to the-internet");
    }
    @Test
    void testButton()
    {
        WebDriver driver = DriverFactory.getDriver();
        BrowserActions.navigateToURL(driver,"http://the-internet.herokuapp.com/");
        ElementActions.click(driver,By.linkText("Add/Remove Elements"));
        Button btn= new Button(driver, By.xpath("//button[text()='Add Element']"));
        btn.shouldHaveText("Add Element");
        btn.click();
        btn.isEnabled()
        Button btn1= new Button(driver, By.xpath("//button[text()='Delete']"));
        btn1.shouldHaveText("Delete");
        driver.quit();
    }
    @Test
    void testCheckBox()
    {
        WebDriver driver = DriverFactory.getDriver();
        BrowserActions.navigateToURL(driver,"http://the-internet.herokuapp.com/");
        ElementActions.click(driver,By.linkText("Checkboxes"));
        CheckBox cb1 = new CheckBox(driver,By.xpath("(//INPUT[@type='checkbox'])[1]"),By.xpath("(//INPUT[@type='checkbox'])[1]"));
        cb1.click();
        CheckBox cb2 = new CheckBox(driver,By.xpath("(//INPUT[@type='checkbox'])[2]"),By.xpath("(//INPUT[@type='checkbox'])[2]"));
        cb2.click();
        driver.quit();
    }
}
