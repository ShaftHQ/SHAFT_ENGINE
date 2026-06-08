package com.shaft.tools.io.internal;

import com.shaft.properties.internal.Properties;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

public class ProgressBarLoggerCoverageUnitTest {

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
        System.clearProperty("cucumber.ansi-colors.disabled");
        Thread.interrupted();
    }

    @Test
    public void shouldUseAnsiColorsWhenCucumberAnsiDisabledShouldReturnFalse() {
        System.setProperty("cucumber.ansi-colors.disabled", "true");

        Assert.assertFalse(ProgressBarLogger.shouldUseAnsiColors());
    }

    @Test
    public void shouldUseAnsiColorsWhenNotDisabledShouldMatchConsoleAvailability() {
        System.setProperty("cucumber.ansi-colors.disabled", "false");

        Assert.assertEquals(ProgressBarLogger.shouldUseAnsiColors(), System.console() != null);
    }

    @Test
    public void constructorTaskAndCloseShouldCoverNormalProgressPath() {
        ProgressBarLogger logger = new ProgressBarLogger("normal-progress-path", 1);
        try {
            logger.task.run();
            Assert.assertNotNull(logger.pb);
        } finally {
            logger.close();
        }
    }

    @Test
    public void constructorTaskAndCloseShouldCoverInterruptedProgressPath() {
        ProgressBarLogger logger = new ProgressBarLogger("interrupted-progress-path", 2);
        logger.close();

        Thread.currentThread().interrupt();
        logger.task.run();
        Assert.assertNotNull(logger.pb);
    }

    @Test
    public void defaultConstructorShouldInitializeAndClose() {
        ProgressBarLogger logger = new ProgressBarLogger("default-constructor-coverage");
        logger.close();
    }
}
