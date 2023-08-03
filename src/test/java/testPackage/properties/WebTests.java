package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class WebTests {
    String targetBrowserName;
    boolean headlessExecution;
    boolean isMobileEmulation;
    boolean mobileEmulationIsCustomDevice;
    String mobileEmulationDeviceName;
    int mobileEmulationWidth;
    int mobileEmulationHeight;
    double mobileEmulationPixelRatio;
    String mobileEmulationUserAgent;
    String baseURL;


    @BeforeClass
    public void beforeClass() {
        targetBrowserName = SHAFT.Properties.web.targetBrowserName();
        headlessExecution = SHAFT.Properties.web.headlessExecution();
        isMobileEmulation = SHAFT.Properties.web.isMobileEmulation();
        mobileEmulationIsCustomDevice = SHAFT.Properties.web.mobileEmulationIsCustomDevice();
        mobileEmulationDeviceName = SHAFT.Properties.web.mobileEmulationDeviceName();
        mobileEmulationWidth = Integer.parseInt("0");
        mobileEmulationHeight = Integer.parseInt("0");
        mobileEmulationPixelRatio = Double.parseDouble("0");
        mobileEmulationUserAgent = SHAFT.Properties.web.mobileEmulationUserAgent();
        baseURL = SHAFT.Properties.web.baseURL();


    }

    @Test
    public void test() {
        SHAFT.Properties.web.set().targetBrowserName(Browser.CHROME.browserName())
                .headlessExecution(headlessExecution)
                .isMobileEmulation(isMobileEmulation)
                .mobileEmulationIsCustomDevice(mobileEmulationIsCustomDevice)
                .mobileEmulationDeviceName(mobileEmulationDeviceName)
                .mobileEmulationWidth(mobileEmulationWidth)
                .mobileEmulationHeight(mobileEmulationHeight)
                .mobileEmulationPixelRatio(mobileEmulationPixelRatio)
                .mobileEmulationUserAgent(mobileEmulationUserAgent)
                .baseURL(baseURL);
    }
}
