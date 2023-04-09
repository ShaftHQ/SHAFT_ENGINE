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
        SHAFT.Properties.web.set().targetBrowserName(targetBrowserName);
        SHAFT.Properties.web.set().headlessExecution(headlessExecution);
        SHAFT.Properties.web.set().isMobileEmulation(isMobileEmulation);
        SHAFT.Properties.web.set().mobileEmulationIsCustomDevice(mobileEmulationIsCustomDevice);
        SHAFT.Properties.web.set().mobileEmulationDeviceName(mobileEmulationDeviceName);
        SHAFT.Properties.web.set().mobileEmulationWidth(mobileEmulationWidth);
        SHAFT.Properties.web.set().mobileEmulationHeight(mobileEmulationHeight);
        SHAFT.Properties.web.set().mobileEmulationPixelRatio(mobileEmulationPixelRatio);
        SHAFT.Properties.web.set().mobileEmulationUserAgent(mobileEmulationUserAgent);
        SHAFT.Properties.web.set().baseURL(baseURL);
    }
}
