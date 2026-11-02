package com.shaft.gui.element.internal;

import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.appium.java_client.android.AndroidDriver;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

/**
 * On Android an element can be in the view tree yet covered by the soft keyboard, so the tap lands on
 * the keyboard (#7283). Before a native click, hide the keyboard when the target's centre lies in the
 * lower area the IME occupies.
 */
final class KeyboardCoverGuard {
    /** Fraction of the window height above which a shown soft keyboard covers the screen. */
    static final double KEYBOARD_TOP = 0.55;

    private KeyboardCoverGuard() {
    }

    static boolean centreUnderKeyboard(Rectangle target, int windowHeight) {
        return windowHeight > 0 && target.getY() + target.getHeight() / 2.0 > windowHeight * KEYBOARD_TOP;
    }

    static void hideIfCovering(WebDriver driver, WebElement target, boolean nativeExecution) {
        if (!nativeExecution || !(driver instanceof AndroidDriver android)) {
            return;
        }
        try {
            if (!android.isKeyboardShown()) {
                return;
            }
            int height = android.manage().window().getSize().getHeight();
            if (centreUnderKeyboard(target.getRect(), height)) {
                android.hideKeyboard();
                ReportManager.logDiscrete("Hid the soft keyboard: it covered the click target.");
            }
        } catch (RuntimeException e) {
            ReportManagerHelper.logDiscrete("Could not check the soft keyboard before click: " + e.getMessage(), Level.DEBUG);
        }
    }
}
