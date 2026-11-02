package junitTestPackage;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInfo;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.Augmenter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.shaft.driver.DriverFactory.DriverType.CHROME;

public class MoonTests {
    private static final String LOCAL_MOON_URL = "http://localhost:4444/wd/hub";
    private static final String TEST_PAGE_URL = "data:text/html;charset=utf-8,%3Chtml%3E%3Chead%3E%3Ctitle%3EMoon%20Test%20Page%3C%2Ftitle%3E%3C%2Fhead%3E%3Cbody%3E%3Cmain%3E%3Ch1%20id%3D%22moon-ready%22%3EMoon%20is%20running%3C%2Fh1%3E%3C%2Fmain%3E%3C%2Fbody%3E%3C%2Fhtml%3E";
    private static final By MOON_READY_HEADER = By.id("moon-ready");
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @BeforeEach
    void beforeMethod(TestInfo testInfo) {
        configureMoonExecution();
        driver.set(new SHAFT.GUI.WebDriver(CHROME, getMoonCapabilities(testInfo.getDisplayName())));
    }

    private static void configureMoonExecution() {
        var executionAddress = SHAFT.Properties.platform.executionAddress();
        if (executionAddress == null || executionAddress.isBlank() || "local".equalsIgnoreCase(executionAddress)) {
            executionAddress = LOCAL_MOON_URL;
        }
        SHAFT.Properties.platform.set()
                .executionAddress(executionAddress)
                .targetPlatform("LINUX")
                .enableBiDi(false);
        SHAFT.Properties.web.set()
                .targetBrowserName("chrome")
                .headlessExecution(true)
                .forceBrowserDownload(false);
        SHAFT.Properties.timeouts.set()
                .waitForRemoteServerToBeUp(false)
                .timeoutForRemoteServerToBeUp(2);
    }

    private static ChromeOptions getMoonCapabilities(String testName) {
        var options = new ChromeOptions();
        var moonOptions = new HashMap<String, Object>();
        moonOptions.put("name", testName);
        moonOptions.put("sessionTimeout", "15m");
        moonOptions.put("env", List.of("TZ=UTC"));
        moonOptions.put("labels", Map.of("manual", "true"));
        options.setCapability("moon:options", moonOptions);
        return options;
    }

    @Test
    void regularDriverShouldOpenTestPage() {
        navigateToTestPageAndAssertHeader(driver.get().getDriver());
    }

    @Test
    void augmentedDriverShouldOpenTestPageWithDevTools() {
        var nativeDriver = driver.get().getDriver();
        nativeDriver = new Augmenter().augment(driver.get().getDriver());
        DevTools devTools = ((HasDevTools) nativeDriver).getDevTools();
        ReportManager.logDiscrete(devTools.getDomains().toString());
        navigateToTestPageAndAssertHeader(nativeDriver);
    }

    private static void navigateToTestPageAndAssertHeader(WebDriver nativeDriver) {
        new BrowserActions(nativeDriver).navigateToURL(TEST_PAGE_URL);
        Validations.assertThat()
                .element(nativeDriver, MOON_READY_HEADER)
                .text()
                .isEqualTo("Moon is running")
                .perform();
    }

    @AfterEach
    void afterMethod() {
        try {
            if (driver.get() != null) {
                driver.get().quit();
            }
        } finally {
            driver.remove();
            Properties.clearForCurrentThread();
        }
    }
}
