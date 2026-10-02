package testPackage.unitTests;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * Verifies the end-of-run leaked WebDriver session inventory (#6397).
 */
public class LeakedDriverSessionUnitTest {
    @Test
    public void unclosedSessionIsReportedAndQuitButClosedSessionIsNot() {
        WebDriver leaked = mock(WebDriver.class);
        WebDriver closed = mock(WebDriver.class);
        DriverFactoryHelper leakedHelper = new DriverFactoryHelper(leaked);
        DriverFactoryHelper closedHelper = new DriverFactoryHelper(closed);
        closedHelper.setDriver(null);
        try {
            Assert.assertTrue(DriverFactoryHelper.closeLeakedDrivers() >= 1, "The leaked session should be counted.");
            verify(leaked).quit();
            verify(closed, never()).quit();
        } finally {
            leakedHelper.setDriver(null);
        }
    }
}
