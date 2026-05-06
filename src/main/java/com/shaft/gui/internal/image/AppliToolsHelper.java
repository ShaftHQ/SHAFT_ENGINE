package com.shaft.gui.internal.image;

import com.applitools.eyes.LogHandler;
import com.applitools.eyes.MatchLevel;
import com.applitools.eyes.TestResults;
import com.applitools.eyes.exceptions.DiffsFoundException;
import com.applitools.eyes.images.Eyes;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.ValidationEnums;

class AppliToolsHelper {

    private AppliToolsHelper() {}

    static boolean compare(byte[] elementScreenshot, String hashedLocatorName,
                           ValidationEnums.VisualValidationEngine engine) {
        Eyes eyes = new Eyes();
        eyes.setLogHandler(new LogHandler() {
            public void open() {}
            public void onMessage(boolean b, String s) { ReportManager.logDiscrete(s); }
            public void close() {}
        });
        eyes.setApiKey(SHAFT.Properties.paths.applitoolsApiKey());
        MatchLevel targetMatchLevel = switch (engine) {
            case EXACT_EYES -> MatchLevel.EXACT;
            case CONTENT_EYES -> MatchLevel.CONTENT;
            case LAYOUT_EYES -> MatchLevel.LAYOUT;
            default -> MatchLevel.STRICT;
        };
        eyes.setMatchLevel(targetMatchLevel);
        if (DriverFactoryHelper.isMobileNativeExecution()) {
            eyes.setHostOS(SHAFT.Properties.mobile.platformName() + "_" + SHAFT.Properties.mobile.platformVersion());
            eyes.setHostApp("NativeMobileExecution");
        } else if (DriverFactoryHelper.isMobileWebExecution()) {
            eyes.setHostOS(SHAFT.Properties.mobile.platformName() + "_" + SHAFT.Properties.mobile.platformVersion());
            eyes.setHostApp(SHAFT.Properties.mobile.browserName());
        } else {
            eyes.setHostOS(SHAFT.Properties.platform.targetPlatform());
            eyes.setHostApp(SHAFT.Properties.web.targetBrowserName());
        }
        try {
            eyes.open("SHAFT_Engine", ReportManagerHelper.getCallingMethodFullName());
            eyes.checkImage(elementScreenshot, hashedLocatorName);
            TestResults result = eyes.close();
            ReportManager.logDiscrete("Successfully validated the element using AI; Applitools Eyes.");
            return result.isNew() || result.isPassed();
        } catch (DiffsFoundException e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
        } finally {
            eyes.abortIfNotClosed();
        }
    }
}
