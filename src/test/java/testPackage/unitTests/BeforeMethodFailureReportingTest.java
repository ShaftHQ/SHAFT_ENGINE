package testPackage.unitTests;

import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

/**
 * Integration-style test that verifies a test skipped due to {@code @BeforeMethod} failure
 * appears as BROKEN (not SKIPPED) in the Allure report.
 *
 * <p>The {@code @BeforeMethod} intentionally throws a {@link RuntimeException} to simulate
 * the scenario that occurs in CI when BrowserStack session creation fails. The test method
 * itself should never run; it should appear as BROKEN in the Allure HTML report with the
 * setup exception's stacktrace attached.
 *
 * <p><b>How to verify manually:</b> run this test and open the generated
 * {@code target/allure-report} HTML. The test should show with status BROKEN (red), not
 * SKIPPED (yellow), and the "Exception Stacktrace" attachment should be readable.
 */
public class BeforeMethodFailureReportingTest {

    /**
     * This {@code @BeforeMethod} simulates a driver-setup failure (e.g., BrowserStack
     * session creation failure). It will cause the test method below to be SKIPPED by TestNG.
     * The SHAFT framework should promote the Allure status to BROKEN and attach the trace.
     */
    @BeforeMethod(alwaysRun = true)
    public void setupThatAlwaysFails() {
        throw new RuntimeException(
                "Simulated @BeforeMethod failure: remote driver could not be created " +
                        "(e.g., BrowserStack session creation error in CI)");
    }

    /**
     * This test should never execute because the {@code @BeforeMethod} always throws. In the
     * Allure report it must appear as BROKEN (not SKIPPED), and the fixture's exception
     * stacktrace must be attached as a readable text file.
     */
    @Test
    public void testThatShouldShowAsBrokenNotSkipped() {
        // Intentionally empty – this body should never be reached
    }
}
