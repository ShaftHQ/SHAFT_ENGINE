package testPackage.unitTests;

import com.shaft.tools.io.internal.RealtimeReporter;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.testng.Assert.*;

/**
 * Unit tests for {@link RealtimeReporter}: verifies CI detection, launch guards,
 * data model helpers, and core server lifecycle (without actually starting a browser).
 */
public class RealtimeReporterUnitTest {
    private String dashboardHtml;

    @BeforeMethod(alwaysRun = true)
    public void ensureServerStopped() {
        RealtimeReporter.stopServer();
        dashboardHtml = readDashboardHtml();
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        RealtimeReporter.stopServer();
    }

    // ─── CI detection ─────────────────────────────────────────────────────

    @Test
    public void isRunningInCIMatchesDirectEnvVarCheck() {
        // We cannot set/unset env vars in Java tests, but we can verify that
        // the method's result is consistent with a direct check of the same variables.
        boolean expectedInCI = System.getenv("CI") != null
                || System.getenv("GITHUB_ACTIONS") != null
                || System.getenv("JENKINS_URL") != null
                || System.getenv("GITLAB_CI") != null
                || System.getenv("TRAVIS") != null
                || System.getenv("CIRCLECI") != null
                || System.getenv("TF_BUILD") != null
                || System.getenv("BAMBOO_BUILDNUMBER") != null
                || System.getenv("TEAMCITY_VERSION") != null
                || System.getenv("BUILD_ID") != null;
        assertEquals(RealtimeReporter.isRunningInCI(), expectedInCI,
                "isRunningInCI() result must match the direct check of CI environment variables");
    }

    @Test
    public void shouldLaunchReturnsFalseByDefault() {
        // The property defaults to false, so shouldLaunch() must return false
        // regardless of the environment.
        assertFalse(RealtimeReporter.shouldLaunch(),
                "Real-time report should be disabled by default");
    }

    @Test
    public void currentTestIdThreadLocalCanBeSetAndCleared() {
        RealtimeReporter.setCurrentTestId("com.example.Test#method");
        assertEquals(RealtimeReporter.getCurrentTestId(), "com.example.Test#method");
        RealtimeReporter.clearCurrentTestId();
        assertNull(RealtimeReporter.getCurrentTestId());
    }

    @Test
    public void currentTestIdIsClearedWhenSetWithBlankValue() {
        RealtimeReporter.setCurrentTestId("com.example.Test#method");
        RealtimeReporter.setCurrentTestId("   ");
        assertNull(RealtimeReporter.getCurrentTestId());
    }

    // ─── Helper utilities ─────────────────────────────────────────────────

    @Test
    public void buildTestIdCombinesClassAndMethod() {
        String id = RealtimeReporter.buildTestId("com.example.MyTest", "loginShouldSucceed");
        assertEquals(id, "com.example.MyTest#loginShouldSucceed");
    }

    @Test
    public void classNameToFilePathConvertsCorrectly() {
        String path = RealtimeReporter.classNameToFilePath("com.example.tests.LoginTest");
        assertEquals(path, "src/test/java/com/example/tests/LoginTest.java");
    }

    @Test
    public void classNameToFilePathHandlesSingleSegmentClass() {
        String path = RealtimeReporter.classNameToFilePath("MyTest");
        assertEquals(path, "src/test/java/MyTest.java");
    }

    @Test
    public void classNameToFilePathHandlesBlankInput() {
        assertEquals(RealtimeReporter.classNameToFilePath(""), "");
        assertEquals(RealtimeReporter.classNameToFilePath("   "), "");
        assertEquals(RealtimeReporter.classNameToFilePath(null), "");
    }

    // ─── JSON serialisation ────────────────────────────────────────────────

    @Test
    public void testCardHasExpectedDefaultValues() {
        RealtimeReporter.TestCard card = new RealtimeReporter.TestCard(
                "myClass#myMethod", "myClass", "myMethod", "src/test/java/myClass.java");
        assertEquals(card.id, "myClass#myMethod");
        assertEquals(card.className, "myClass");
        assertEquals(card.methodName, "myMethod");
        assertEquals(card.filePath, "src/test/java/myClass.java");
        assertEquals(card.startTime, 0);
        assertEquals(card.endTime, 0);
        assertNull(card.errorMessage);
    }

    @Test
    public void buildTestIdProducesConsistentResult() {
        String id1 = RealtimeReporter.buildTestId("com.example.Test", "method1");
        String id2 = RealtimeReporter.buildTestId("com.example.Test", "method1");
        assertEquals(id1, id2, "Same class+method must produce the same test ID");
    }

    @Test
    public void buildTestIdDifferentiatesTestMethods() {
        String id1 = RealtimeReporter.buildTestId("com.example.Test", "method1");
        String id2 = RealtimeReporter.buildTestId("com.example.Test", "method2");
        assertNotEquals(id1, id2, "Different methods on the same class must produce different IDs");
    }

    // ─── Server lifecycle (no browser launch) ─────────────────────────────

    @Test
    public void serverIsNotRunningInitially() {
        assertFalse(RealtimeReporter.isRunning(),
                "Server must not be running before initialize() is called");
    }

    @Test
    public void stopServerIsIdempotentWhenNotStarted() {
        // Calling stopServer when the server has never been started should not throw
        RealtimeReporter.stopServer();
        RealtimeReporter.stopServer();
        assertFalse(RealtimeReporter.isRunning());
    }

    // ─── Test data model ──────────────────────────────────────────────────

    @Test
    public void testCardDefaultStatusIsTodo() {
        RealtimeReporter.TestCard card = new RealtimeReporter.TestCard(
                "id", "ClassName", "methodName", "src/test/.../ClassName.java");
        assertEquals(card.status, RealtimeReporter.TestStatus.TODO);
    }

    @Test
    public void testCardStepsListIsInitiallyEmpty() {
        RealtimeReporter.TestCard card = new RealtimeReporter.TestCard(
                "id", "ClassName", "methodName", "src/test/.../ClassName.java");
        assertNotNull(card.steps);
        assertEquals(card.steps.size(), 0);
    }

    @Test
    public void testCardHasAttachmentsIsFalseByDefault() {
        RealtimeReporter.TestCard card = new RealtimeReporter.TestCard(
                "id", "ClassName", "methodName", "src/test/.../ClassName.java");
        assertFalse(card.hasAttachments,
                "New test card should not have attachments by default");
    }

    @Test
    public void attachmentInfoHasExpectedFields() {
        RealtimeReporter.AttachmentInfo att = new RealtimeReporter.AttachmentInfo();
        att.id = "att-1";
        att.type = "Screenshot";
        att.name = "login-page.png";
        att.contentType = "image/png";
        att.size = 12345;
        att.timestamp = System.currentTimeMillis();
        assertEquals(att.id, "att-1");
        assertEquals(att.type, "Screenshot");
        assertEquals(att.name, "login-page.png");
        assertEquals(att.contentType, "image/png");
        assertEquals(att.size, 12345);
        assertTrue(att.timestamp > 0);
    }

    // ─── Planned tests (server must be running) ───────────────────────────
    // NOTE: These tests skip if the flag is disabled (default) to avoid
    //       binding to port 1111 in CI.  They exercise the server when the
    //       feature is explicitly enabled via a system property.

    @Test
    public void appendConsoleLogIsNoOpWhenServerNotRunning() {
        // Server is stopped; this must not throw
        RealtimeReporter.appendConsoleLog("com.example.Foo#bar", "some log line");
    }

    @Test
    public void appendStepIsNoOpWhenServerNotRunning() {
        // Server is stopped; this must not throw
        RealtimeReporter.appendStep("com.example.Foo#bar", "I click the button", "PASSED");
    }

    @Test
    public void onTestStartedIsNoOpWhenServerNotRunning() {
        RealtimeReporter.onTestStarted("com.example.Foo#bar");
    }

    @Test
    public void onTestFinishedIsNoOpWhenServerNotRunning() {
        RealtimeReporter.onTestFinished("com.example.Foo#bar",
                RealtimeReporter.TestStatus.PASSED, null);
    }

    @Test
    public void onTestsPlannedIsNoOpWhenServerNotRunning() {
        List<RealtimeReporter.TestCard> cards = List.of(
                new RealtimeReporter.TestCard("id", "C", "m", "path"));
        RealtimeReporter.onTestsPlanned(cards); // must not throw
    }

    @Test
    public void appendAttachmentIsNoOpWhenServerNotRunning() {
        // Server is stopped; this must not throw
        RealtimeReporter.appendAttachment("com.example.Foo#bar",
                "Screenshot", "screenshot.png", "image/png", new byte[]{1, 2, 3});
    }

    @Test
    public void dashboardHasLightAndDarkLogoElements() {
        assertTrue(dashboardHtml.contains("https://shaftengine.netlify.app/img/shaft.svg"),
                "Dashboard should use official SHAFT logo URL.");
        assertTrue(dashboardHtml.contains("class=\"logo-light\""),
                "Dashboard should include explicit light logo element.");
        assertTrue(dashboardHtml.contains("class=\"logo-dark\""),
                "Dashboard should include explicit dark logo element.");
    }

    @Test
    public void dashboardLightModeIsDefaultAndDarkModeIsOptIn() {
        assertTrue(dashboardHtml.contains("/* Light mode (default) */"),
                "Light mode should be documented and configured as default.");
        assertTrue(dashboardHtml.contains("document.documentElement.classList.add('light');"),
                "Dashboard should initialize in light mode by default.");
        assertTrue(dashboardHtml.contains("if (savedTheme === 'dark')"),
                "Dashboard should restore dark mode only when explicitly saved.");
    }

    @Test
    public void dashboardHasThemePersistenceAndToggleHooks() {
        assertTrue(dashboardHtml.contains("localStorage.getItem('shaft-theme')"),
                "Dashboard should restore saved theme from localStorage.");
        assertTrue(dashboardHtml.contains("localStorage.setItem('shaft-theme'"),
                "Dashboard should persist theme changes in localStorage.");
        assertTrue(dashboardHtml.contains("theme-btn"),
                "Dashboard should expose a theme toggle control.");
    }

    private String readDashboardHtml() {
        try (InputStream is = RealtimeReporter.class.getResourceAsStream("/realtime/dashboard.html")) {
            if (is != null) {
                return new String(is.readAllBytes(), StandardCharsets.UTF_8);
            }
            Path sourcePath = Path.of(System.getProperty("user.dir"),
                    "src", "main", "resources", "realtime", "dashboard.html");
            if (Files.exists(sourcePath)) {
                return Files.readString(sourcePath, StandardCharsets.UTF_8);
            }
            fail("Could not load dashboard.html from classpath or source resources path");
        } catch (Exception e) {
            fail("Failed to read dashboard HTML resource: " + e.getMessage());
        }
        return "";
    }
}
