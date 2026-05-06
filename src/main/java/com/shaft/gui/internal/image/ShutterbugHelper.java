package com.shaft.gui.internal.image;

import com.assertthat.selenium_shutterbug.core.CaptureElement;
import com.assertthat.selenium_shutterbug.core.Shutterbug;
import com.assertthat.selenium_shutterbug.utils.image.UnableToCompareImagesException;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.openqa.selenium.By;
import org.openqa.selenium.UnsupportedCommandException;
import org.openqa.selenium.WebDriver;

import java.io.IOException;

class ShutterbugHelper {

    private ShutterbugHelper() {}

    static boolean compareWithDiff(WebDriver driver, By elementLocator,
                                   String referenceImagePath, String resultingImagePath,
                                   double threshold) {
        try {
            var snapshot = Shutterbug.shootElement(driver, elementLocator, CaptureElement.VIEWPORT, true);
            return snapshot.equalsWithDiff(referenceImagePath, resultingImagePath, threshold);
        } catch (IOException e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
        } catch (UnableToCompareImagesException | UnsupportedCommandException e) {
            ReportManager.logDiscrete("Shutterbug comparison failed, falling back: " + e.getMessage());
            throw new ShutterbugFallbackException(e);
        }
    }

    static class ShutterbugFallbackException extends RuntimeException {
        ShutterbugFallbackException(Throwable cause) { super(cause); }
    }
}
