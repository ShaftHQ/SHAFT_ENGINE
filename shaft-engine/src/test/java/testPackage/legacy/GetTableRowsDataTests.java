package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.Map;

public class GetTableRowsDataTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    By tableLocator = By.id("example");

    @Test
    public void getFirstRow(){
        Map<String, String> firstRow = driver.get().element().getTableRowsData(tableLocator).get(0);
        Assert.assertEquals(firstRow.get("Name"),"Airi Satou");
        Assert.assertEquals(firstRow.get("Age"), "33");
        Assert.assertEquals(firstRow.get("Start date"), "2008-11-28");

    }
    @BeforeMethod(description = "Setup Browser instance.")
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("https://datatables.net/examples/basic_init/zero_configuration.html");
    }

    @AfterMethod(description = "Teardown Browser instance.", alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
