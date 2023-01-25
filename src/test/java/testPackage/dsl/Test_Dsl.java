package testPackage.dsl;

import com.shaft.driver.SHAFT;
import com.shaft.dsl.gui.Button;
import com.shaft.dsl.gui.CheckBox;
import com.shaft.dsl.gui.Element;
import com.shaft.dsl.gui.Label;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_Dsl {
    SHAFT.GUI.WebDriver driver;

    @BeforeMethod
    void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("http://the-internet.herokuapp.com/");
    }

    @AfterMethod
    void afterMethod() {
        driver.quit();
    }

    @Test
    void testLabel() {
        Element.setDriver(driver.getDriver());
        Label label = new Label(By.xpath("//*[contains(text(),'Welcome to the-internet')]"));
        label.shouldHaveText("Welcome to the-internet");
    }

    @Test
    void testButton() {
        driver.element().click(By.linkText("Add/Remove Elements"));
        Element.setDriver(driver.getDriver());
        Button btn = new Button(By.xpath("//button[text()='Add Element']"));
        btn.shouldHaveText("Add Element");
        btn.click();
        btn.isEnabled();
        Button btn1 = new Button(By.xpath("//button[text()='Delete']"));
        btn1.shouldHaveText("Delete");
    }

    @Test
    void testCheckBox() {
        Element.setDriver(driver.getDriver());
        driver.element().click(By.linkText("Checkboxes"));
        CheckBox cb1 = new CheckBox(By.xpath("(//INPUT[@type='checkbox'])[1]"), By.xpath("(//INPUT[@type='checkbox'])[1]"));
        cb1.click();
        CheckBox cb2 = new CheckBox(By.xpath("(//INPUT[@type='checkbox'])[2]"), By.xpath("(//INPUT[@type='checkbox'])[2]"));
        cb2.click();
    }
}
