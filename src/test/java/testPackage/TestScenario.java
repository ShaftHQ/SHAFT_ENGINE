package testPackage;

import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;

public abstract class TestScenario {
    protected static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @BeforeClass
    public void init() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterClass(alwaysRun = true)
    public void tear() {
        driver.get().quit();
    }
}
