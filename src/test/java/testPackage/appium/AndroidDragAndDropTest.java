package testPackage.appium;

import com.shaft.driver.SHAFT;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class AndroidDragAndDropTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private static final By dragAndDropScreen = AppiumBy.accessibilityId("Drag-drop-screen");

    @Test
    public void wizard_scrollInExpandableLists_verticalScrolling_insideScreen() {
        By dragButton = By.xpath("//android.widget.Button[@content-desc='Drag']");
        By draggableRobotEyes = By.xpath("//android.view.ViewGroup[@content-desc='drag-c1']/android.widget.ImageView");
        By dropRobotEyes = By.xpath("//android.view.ViewGroup[@content-desc='drop-c1']/android.view.ViewGroup");

        // WebDriver code -> really fails to drag
//        driver.get().element().click(dragButton)
//                .dragAndDrop(draggableRobotEyes, dropRobotEyes)
//                .and().assertThat(dragAndDropScreen).matchesReferenceImage().perform();

        // Appium code -> drag and drop happens but shows up as failed
        driver.get().touch().tap(dragButton)
                .swipeToElement(draggableRobotEyes, dropRobotEyes)
                .and().assertThat(dragAndDropScreen).matchesReferenceImage().perform();
    }

    @Test
    public void multipleDragAndDrop() {
        driver.get().touch().tap(AppiumBy.accessibilityId("Drag"));
        driver.get().element()
                .dragAndDrop(AppiumBy.accessibilityId("drag-l2"), AppiumBy.accessibilityId("drop-l2"))
                .dragAndDrop(AppiumBy.accessibilityId("drag-r3"), AppiumBy.accessibilityId("drop-r3"))
                .dragAndDrop(AppiumBy.accessibilityId("drag-c3"), AppiumBy.accessibilityId("drop-c3"))
                .dragAndDrop(AppiumBy.accessibilityId("drag-r1"), AppiumBy.accessibilityId("drop-r1"))
                .and().assertThat(dragAndDropScreen).matchesReferenceImage().perform();
    }

    @SuppressWarnings("CommentedOutCode")
    @BeforeMethod
    public void setup() {
        System.setProperty("mobile_autoGrantPermissions", "true");
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().automationName(AutomationName.ANDROID_UIAUTOMATOR2);

        // self-managed execution
//        SHAFT.Properties.mobile.set().selfManaged(true);
//        SHAFT.Properties.mobile.set().selfManagedAndroidSDKVersion(31);

        // local appium server (for local and GitHub actions execution)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4723");
//        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/Android-NativeDemoApp-0.4.0.apk");

        // local appium server (android-emulator docker-compose)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4725");
//        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/Android-NativeDemoApp-0.4.0.apk");

        // remote browserstack server (new app version)
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.browserStack.set().platformVersion("13.0");
//        SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
//        SHAFT.Properties.browserStack.set().appName("Android-NativeDemoApp-0.4.0.apk");
//        SHAFT.Properties.browserStack.set().appRelativeFilePath("src/test/resources/testDataFiles/apps/Android-NativeDemoApp-0.4.0.apk");
//        SHAFT.Properties.browserStack.set().appUrl("");

//        // remote browserstack server (existing app version)
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.browserStack.set().platformVersion("13.0");
//        SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
//        SHAFT.Properties.browserStack.set().appName("Android-NativeDemoApp-0.4.0.apk");
//        SHAFT.Properties.browserStack.set().appRelativeFilePath("");
//        SHAFT.Properties.browserStack.set().appUrl("bs://ea453f6afaac69563817f4ae41311e2de98b260d");
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        driver.get().quit();
    }
}