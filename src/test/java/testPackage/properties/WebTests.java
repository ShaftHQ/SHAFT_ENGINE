package testPackage.properties;

import com.shaft.driver.SHAFT;
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
    boolean incognitoMode;

    @BeforeClass
    public void beforeClass() {
        targetBrowserName = SHAFT.Properties.web.targetBrowserName();
        headlessExecution = SHAFT.Properties.web.headlessExecution();
        isMobileEmulation = SHAFT.Properties.web.isMobileEmulation();
        mobileEmulationIsCustomDevice = SHAFT.Properties.web.mobileEmulationIsCustomDevice();
        mobileEmulationDeviceName = SHAFT.Properties.web.mobileEmulationDeviceName();
        mobileEmulationWidth = 0;
        mobileEmulationHeight = 0;
        mobileEmulationPixelRatio = Double.parseDouble("0");
        mobileEmulationUserAgent = SHAFT.Properties.web.mobileEmulationUserAgent();
        baseURL = SHAFT.Properties.web.baseURL();
        browserWindowWidth = 1920;
        browserWindowHeight = 1080;
        incognitoMode = SHAFT.Properties.web.incognitoMode();
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
                .browserWindowHeight(browserWindowHeight)
                .incognitoMode(incognitoMode);
    }
}
