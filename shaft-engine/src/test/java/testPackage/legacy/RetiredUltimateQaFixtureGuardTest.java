package testPackage.legacy;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RetiredUltimateQaFixtureGuardTest {
    private static final List<Path> NIGHTLY_FIXTURES = List.of(
            Path.of("src/test/java/testPackage/legacy/BigPageActionsTest.java"),
            Path.of("src/test/java/junitTestPackage/JunitTest.java"),
            Path.of("src/test/java/testPackage/appium/MobileWebTest.java")
    );

    @Test
    void nightlyFixturesUseTheSeleniumOwnedForm() throws IOException {
        Path localForm = Path.of("src/test/java/testPackage/legacy/BigPageActionsTest.java");
        Path junitForm = Path.of("src/test/java/junitTestPackage/JunitTest.java");
        Path mobileForm = Path.of("src/test/java/testPackage/appium/MobileWebTest.java");
        for (Path fixture : NIGHTLY_FIXTURES) {
            String source = Files.readString(fixture);
            assertFalse(source.contains("ultimateqa.com/complicated-page"), fixture + " still uses UltimateQA");
            assertFalse(source.contains("et_pb_contact_name_0"), fixture + " still uses the retired form");
            assertFalse(source.contains("saucedemo.com"), fixture + " still uses Sauce Demo");
        }
        String localSource = Files.readString(localForm);
        String junitSource = Files.readString(junitForm);
        String mobileSource = Files.readString(mobileForm);
        assertTrue(localSource.contains("TestPageServer.url(\"smartWebFormFixture.html\")"),
                localForm + " must use the repo-owned web form fixture");
        assertTrue(junitSource.contains("TestPageServer.url(\"smartWebFormFixture.html\")"),
                junitForm + " must use the repo-owned web form fixture");
        assertTrue(mobileSource.contains("https://www.selenium.dev/selenium/web/web-form.html"),
                mobileForm + " stays on Selenium's owned web form (not Local E2E GUI)");
    }
}
