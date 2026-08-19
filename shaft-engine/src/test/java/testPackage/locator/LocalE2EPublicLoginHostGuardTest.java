package testPackage.locator;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

/**
 * Inventory guard for #5207: Local E2E GUI tests must not depend on the
 * public static-markup hosts that were migrated off live sites (#5216).
 */
public class LocalE2EPublicLoginHostGuardTest {
    private static final List<String> FORBIDDEN_HOSTS = List.of(
            "practicetestautomation" + ".com",
            "saucedemo.com" + "/v1",
            "selenium.dev" + "/selenium/web/login.html",
            "moatazeldebsy" + ".github.io"
    );

    @Test
    public void localE2EGuiTestsMustNotUseMigratedPublicLoginHosts() throws IOException {
        Path testRoot = Path.of("src/test/java");
        Assert.assertTrue(Files.isDirectory(testRoot),
                "expected shaft-engine test sources at " + testRoot.toAbsolutePath());

        List<String> hits = new ArrayList<>();
        try (Stream<Path> files = Files.walk(testRoot)) {
            files.filter(path -> path.toString().endsWith(".java"))
                    .filter(LocalE2EPublicLoginHostGuardTest::isLocalE2EGuiTest)
                    .forEach(path -> addHitIfForbidden(testRoot, path, hits));
        }

        Assert.assertTrue(hits.isEmpty(),
                "Local E2E GUI tests still use a migrated public login host: " + hits);
    }

    @Test
    public void guardDetectsHostsMigratedBy5207() {
        Assert.assertTrue(containsForbiddenHost("https://www.saucedemo.com" + "/v1"),
                "saucedemo v1 login was migrated and must stay forbidden");
        Assert.assertTrue(containsForbiddenHost("https://www.selenium.dev" + "/selenium/web/login.html"),
                "selenium.dev login.html was migrated and must stay forbidden");
        Assert.assertTrue(containsForbiddenHost("https://moatazeldebsy" + ".github.io/test-automation-practices/#/login"),
                "moatazeldebsy forms were migrated and must stay forbidden");
        Assert.assertTrue(containsForbiddenHost("https://practicetestautomation" + ".com/practice-test-login/"),
                "practice-test-automation host remains forbidden");
    }

    @Test
    public void guardAllowsIntentionalLeftoverAndCloudHosts() {
        Assert.assertFalse(containsForbiddenHost("https://www.saucedemo.com/inventory.html"),
                "saucedemo.com without /v1 is an intentional leftover live URL");
        Assert.assertFalse(containsForbiddenHost("https://www.selenium.dev/selenium/web/web-form.html"),
                "selenium web-form stays on cloud/grid jobs");
        Assert.assertFalse(containsForbiddenHost("https://hub.browserstack.com/wd/hub"),
                "BrowserStack/cloud hosts must not be forbidden");
        Assert.assertFalse(containsForbiddenHost("https://the-internet.herokuapp.com/dynamic_loading/2"),
                "leftover live URLs kept on purpose must not be forbidden");
    }

    private static boolean isLocalE2EGuiTest(Path path) {
        String normalized = path.toString().replace('\\', '/');
        return !normalized.contains("/unitTests/")
                && !normalized.contains("/LambdaTest/")
                && !normalized.contains("/appium/");
    }

    private static void addHitIfForbidden(Path testRoot, Path path, List<String> hits) {
        try {
            if (containsForbiddenHost(Files.readString(path))) {
                hits.add(testRoot.relativize(path).toString().replace('\\', '/'));
            }
        } catch (IOException exception) {
            Assert.fail(exception.getMessage(), exception);
        }
    }

    static boolean containsForbiddenHost(String source) {
        for (String host : FORBIDDEN_HOSTS) {
            if (source.contains(host)) {
                return true;
            }
        }
        return false;
    }
}
