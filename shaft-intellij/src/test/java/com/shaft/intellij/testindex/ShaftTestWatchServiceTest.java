package com.shaft.intellij.testindex;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftTestWatchServiceTest {
    private static final String PROJECT_ROOT = "C:/dev/shop-tests";

    @Test
    void recognizesTestSourceFileUnderProject() {
        assertTrue(ShaftTestWatchService.isTestSourceChange(
                "C:/dev/shop-tests/src/test/java/SignInTest.java", PROJECT_ROOT));
    }

    @Test
    void normalizesWindowsBackslashesBeforeMatching() {
        assertTrue(ShaftTestWatchService.isTestSourceChange(
                "C:\\dev\\shop-tests\\src\\test\\java\\SignInTest.java", "C:\\dev\\shop-tests"));
    }

    @Test
    void ignoresMainSourceFiles() {
        assertFalse(ShaftTestWatchService.isTestSourceChange(
                "C:/dev/shop-tests/src/main/java/App.java", PROJECT_ROOT));
    }

    @Test
    void ignoresFilesOutsideTheProject() {
        assertFalse(ShaftTestWatchService.isTestSourceChange(
                "C:/other/src/test/java/SignInTest.java", PROJECT_ROOT));
    }

    @Test
    void returnsFalseForNullInputs() {
        assertFalse(ShaftTestWatchService.isTestSourceChange(null, PROJECT_ROOT));
        assertFalse(ShaftTestWatchService.isTestSourceChange(
                "C:/dev/shop-tests/src/test/java/SignInTest.java", null));
    }

    @Test
    void ignoresTargetOrBuildOutputEvenIfPathContainsTestSubstring() {
        assertFalse(ShaftTestWatchService.isTestSourceChange(
                "C:/dev/shop-tests/target/test-classes/SignInTest.class", PROJECT_ROOT));
    }
}
