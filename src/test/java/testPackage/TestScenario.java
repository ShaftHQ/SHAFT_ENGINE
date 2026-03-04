package testPackage;

import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;

public abstract class TestScenario {
    protected SHAFT.GUI.WebDriver driver;

    @BeforeClass
    public void init() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterClass(alwaysRun = true)
    public void tear() {
        if (driver != null) driver.quit();
    }
}
