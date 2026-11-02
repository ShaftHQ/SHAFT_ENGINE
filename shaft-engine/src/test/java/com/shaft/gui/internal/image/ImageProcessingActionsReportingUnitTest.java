package com.shaft.gui.internal.image;

import com.shaft.tools.io.ReportManager;
import org.apache.logging.log4j.Level;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.testng.annotations.Test;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;

public class ImageProcessingActionsReportingUnitTest {
    @Test
    public void formatElementLocatorToImagePathShouldLogAsDebugOnly() {
        By locator = By.id("visual-" + System.nanoTime());

        try (MockedStatic<ReportManager> reportManager = Mockito.mockStatic(ReportManager.class)) {
            ImageProcessingActions.formatElementLocatorToImagePath(locator);

            reportManager.verify(() -> ReportManager.logDiscrete(
                    contains("Element Locator \"By.id: visual-"), eq(Level.DEBUG)));
            reportManager.verify(() -> ReportManager.log(anyString(), any(Level.class)), never());
        }
    }
}
