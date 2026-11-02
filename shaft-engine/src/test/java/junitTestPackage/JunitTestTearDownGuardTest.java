package junitTestPackage;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

/**
 * Sibling regression coverage for the same tearDown() NPE masking bug fixed in
 * {@link JunitParallelizationTestsTearDownGuardTest} (issue #3812): {@link JunitTest}
 * keeps its WebDriver in a {@code ThreadLocal} and calls {@code driver.get().quit()}
 * unconditionally in {@code afterEach()}. When {@code beforeEach()} never completes,
 * the ThreadLocal is never set, {@code driver.get()} returns null, and the resulting
 * NullPointerException hides the real setup failure.
 */
class JunitTestTearDownGuardTest {

    @Test
    void afterEachShouldNotThrowWhenBeforeEachNeverCompleted() {
        var testInstance = new JunitTest();

        assertDoesNotThrow(testInstance::afterEach);
    }
}
