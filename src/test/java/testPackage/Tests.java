package testPackage;

import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;

public abstract class Tests {
    static final ThreadLocal<SHAFT.GUI.WebDriver> driverThreadLocal = new ThreadLocal<>();

    @BeforeMethod
    public void init() {
        driverThreadLocal.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod
    public void tear() {
        driverThreadLocal.get().quit();
    }
}
