package junitTestPackage;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.Augmenter;
import poms.GoogleSearch;

import java.util.ArrayList;
import java.util.HashMap;

import static com.shaft.driver.DriverFactory.DriverType.CHROME;

public class MoonTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    //    @BeforeAll
    public static void beforeAll() {
        SHAFT.Properties.platform.set().executionAddress("http://moon.aerokube.local/wd/hub");
        SHAFT.Properties.web.set().headlessExecution(true);
        SHAFT.Properties.timeouts.set().remoteServerInstanceCreationTimeout(1);
        SHAFT.Properties.platform.set().enableBiDi(false);
    }

    private static ChromeOptions getMoonCapabilities(String testName) {
        var options = new ChromeOptions();
        options.setCapability("moon:options", new HashMap<String, Object>() {{
            /* How to add test badge */
            put("name", testName);

            /* How to set session timeout */
            put("sessionTimeout", "15m");

            /* How to set timezone */
            put("env", new ArrayList<String>() {{
                add("TZ=UTC");
            }});

            /* How to add "trash" button */
            put("labels", new HashMap<String, Object>() {{
                put("manual", "true");
            }});

            /* How to enable video recording */
            put("enableVideo", true);
        }});
        return options;
    }

    //    @Test
    public void regularDriver() {
        var searchPage = new GoogleSearch(driver.get().getDriver());
        searchPage.navigateToURL();
        searchPage.assertPageIsOpen();
    }

    //    @Test
    public void augmentedDriver() {
        var nativeDriver = driver.get().getDriver();
        nativeDriver = new Augmenter().augment(driver.get().getDriver());
        DevTools devTools = ((HasDevTools) nativeDriver).getDevTools();
        ReportManager.logDiscrete(devTools.getDomains().toString());
        var searchPage = new GoogleSearch(nativeDriver);
        searchPage.navigateToURL();
        searchPage.assertPageIsOpen();
    }

    //    @BeforeEach
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver(CHROME, getMoonCapabilities("test")));
    }

    //    @AfterEach
    public void afterMethod() {
        driver.get().quit();
    }
}
