package testPackage.appium;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

/**
 * Flutter Integration Driver E2E against the CI-built
 * AppiumTestDistribution/appium-flutter-server demo-app (pinned in e2eTests.yml).
 *
 * <p>Acceptance coverage (issue #5638):
 * <ul>
 *   <li>Tap a Flutter widget via {@link SHAFT.GUI.Locator#flutterKey(String)}</li>
 *   <li>Type into a Flutter {@code TextField}</li>
 *   <li>Assert element text after interaction via SHAFT fluent validations</li>
 * </ul>
 *
 * <p>Skipped unless {@code -Dshaft.enableFlutterE2E=true} so PR-gate unit jobs stay green
 * without an emulator. Local emulator CI passes {@code -DexecutionAddress=127.0.0.1:4723}
 * and {@code -Dmobile_app=.../flutter-demo.apk}; omitting those keeps the BrowserStack path.
 *
 * <p>Runnable sample (matches the Flutter user guide):
 * <pre>{@code
 * mvn -pl shaft-engine -Dtest=FlutterTest -Dshaft.enableFlutterE2E=true \
 *   -DexecutionAddress=127.0.0.1:4723 \
 *   -Dmobile_app=src/test/resources/testDataFiles/apps/flutter-demo.apk test
 * }</pre>
 */
public class FlutterTest {
    private static final String ENABLE_FLUTTER_E2E_PROPERTY = "shaft.enableFlutterE2E";
    private static final String DEMO_APK = "src/test/resources/testDataFiles/apps/flutter-demo.apk";

    // Locators match demo-app/lib/screens/login_screen.dart + home_screen.dart at pin
    // 78ba97a6146091b944335d0db3330f74416f3ac7 (AppiumTestDistribution/appium-flutter-server).
    private static final By PLEASE_LOGIN_TEXT = SHAFT.GUI.Locator.flutterText("Please Login");
    private static final By PLEASE_LOGIN_SEMANTICS = SHAFT.GUI.Locator.flutterSemanticsLabel("please_login_text");
    private static final By USERNAME_FIELD = SHAFT.GUI.Locator.flutterKey("username_text_field");
    private static final By PASSWORD_FIELD = SHAFT.GUI.Locator.flutterKey("password");
    private static final By LOGIN_BUTTON = SHAFT.GUI.Locator.flutterKey("LoginButton");
    private static final By LOGIN_BUTTON_TEXT = SHAFT.GUI.Locator.flutterText("Login");
    private static final By SAMPLES_LIST_TITLE = SHAFT.GUI.Locator.flutterText("Samples List");
    private static final By TEXT_FIELD_TYPE = SHAFT.GUI.Locator.flutterType("TextField");

    public static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @BeforeMethod(onlyForGroups = {"flutter"})
    public void setupFlutterDriver() {
        if (!Boolean.getBoolean(ENABLE_FLUTTER_E2E_PROPERTY)) {
            throw new SkipException("Flutter E2E is disabled. Set -D"
                    + ENABLE_FLUTTER_E2E_PROPERTY
                    + "=true with a debug/profile Integration-Server APK (never release).");
        }

        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().automationName(AutomationName.FLUTTER_INTEGRATION);
        SHAFT.Properties.mobile.set().browserName("");

        String executionAddress = System.getProperty("executionAddress", "");
        boolean localEmulator = isLocalAppiumAddress(executionAddress);

        if (localEmulator) {
            SHAFT.Properties.platform.set().executionAddress(executionAddress);
            String appPath = System.getProperty("mobile_app", DEMO_APK);
            SHAFT.Properties.mobile.set().app(appPath);
        } else {
            // BrowserStack path (optional; emulator job is the primary CI proof for #5638).
            SHAFT.Properties.platform.set().executionAddress("browserstack");
            SHAFT.Properties.browserStack.set().platformVersion("13.0");
            SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
            SHAFT.Properties.browserStack.set().appName("flutter-demo.apk");
            SHAFT.Properties.browserStack.set().appRelativeFilePath(DEMO_APK);
            SHAFT.Properties.browserStack.set().appUrl("");
        }

        driver.set(new SHAFT.GUI.WebDriver());
    }

    /**
     * Tap + type + assert text against the demo-app Login screen, then verify Home opens.
     * Uses {@link SHAFT.GUI.Locator} Flutter factories exclusively (not raw AppiumBy).
     */
    @Test(groups = {"flutter"}, description = "Flutter tap/type/text via SHAFT.GUI.Locator.flutter*")
    public void testFlutterTapTypeAndAssertText() {
        var d = driver.get();

        d.assertThat().element(PLEASE_LOGIN_TEXT).exists().perform();
        d.assertThat().element(PLEASE_LOGIN_SEMANTICS).exists().perform();
        d.assertThat().element(TEXT_FIELD_TYPE).exists().perform();
        d.assertThat().element(LOGIN_BUTTON_TEXT).exists().perform();

        // Type (demo pre-fills admin/1234; clear+retype proves TextField interaction).
        d.element().type(USERNAME_FIELD, "admin");
        d.element().type(PASSWORD_FIELD, "1234");
        d.element().assertThat(USERNAME_FIELD).text().isEqualTo("admin").perform();

        // Tap Login via ValueKey.
        d.element().click(LOGIN_BUTTON);

        // Assert home screen title after navigation.
        d.assertThat().element(SAMPLES_LIST_TITLE).exists().perform();
        d.element().assertThat(SAMPLES_LIST_TITLE).text().contains("Samples").perform();
    }

    /**
     * Partial-text finder smoke (same session prerequisites as the main flow).
     */
    @Test(groups = {"flutter"}, description = "Flutter flutterTextContaining finder")
    public void testFlutterTextContaining() {
        driver.get().assertThat()
                .element(SHAFT.GUI.Locator.flutterTextContaining("Please"))
                .exists()
                .perform();
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        if (driver.get() != null) {
            driver.get().quit();
            driver.remove();
        }
        Properties.clearForCurrentThread();
    }

    private static boolean isLocalAppiumAddress(String executionAddress) {
        if (executionAddress == null || executionAddress.isBlank()) {
            return false;
        }
        String normalized = executionAddress.toLowerCase();
        return normalized.contains("127.0.0.1") || normalized.contains("localhost");
    }
}
