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
    int browserWindowWidth;
    int browserWindowHeight;


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
        browserWindowWidth = Integer.parseInt("1920");
        browserWindowHeight = Integer.parseInt("1080");
    }

    @Test
    public void test() {
        SHAFT.Properties.web.set().targetBrowserName(targetBrowserName)
                .headlessExecution(headlessExecution)
                .isMobileEmulation(isMobileEmulation)
                .mobileEmulationIsCustomDevice(mobileEmulationIsCustomDevice)
                .mobileEmulationDeviceName(mobileEmulationDeviceName)
                .mobileEmulationWidth(mobileEmulationWidth)
                .mobileEmulationHeight(mobileEmulationHeight)
                .mobileEmulationPixelRatio(mobileEmulationPixelRatio)
                .mobileEmulationUserAgent(mobileEmulationUserAgent)
                .baseURL(baseURL)
                .browserWindowWidth(browserWindowWidth)
                .browserWindowHeight(browserWindowHeight);
    }
}
