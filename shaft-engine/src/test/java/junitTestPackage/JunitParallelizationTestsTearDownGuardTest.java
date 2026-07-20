package junitTestPackage;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

/**
 * Regression coverage for the tearDown() NPE masking bug described in issue #3812:
 * when {@code setup()} never completes (e.g. the {@code SHAFT.GUI.WebDriver} constructor
 * throws), {@code driver} stays null, and an unguarded {@code driver.quit()} in
 * {@code tearDown()} throws a NullPointerException that hides the real setup failure.
 */
class JunitParallelizationTestsTearDownGuardTest {

    @Test
    void tearDownShouldNotThrowWhenSetupNeverCompleted() {
        var testInstance = new JunitParallelizationTests();

        assertDoesNotThrow(testInstance::tearDown);
    }
}
