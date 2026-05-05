package testPackage.properties;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.PropertiesHelper;
import org.testng.Assert;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import java.util.Locale;

public class Log4jTests {
    String name;
    String appenderConsoleType;
    String appenderConsoleName;
    String appenderConsoleLayoutType;
    boolean appenderConsoleLayoutDisableAnsi;
    boolean appenderConsoleLayoutNoConsoleNoAnsi;
    String appenderConsoleLayoutCharset;
    String appenderConsoleLayoutPattern;
    String appenderConsoleFilterThresholdType;
    String appenderConsoleFilterThresholdLevel;
    String appenderFileType;
    String appenderFileName;
    String appenderFile_FileName;
    String appenderFileLayoutType;
    String appenderFileLayoutPattern;
    String appenderFileLayoutCharset;
    String appenderFileFilterThresholdType;
    String appenderFileFilterThresholdLevel;
    String rootLogger;
    String loggerAppName;
    String loggerAppLevel;

    @BeforeClass(alwaysRun = true)
    public void beforeClass() {
        PropertiesHelper.initialize();
    }

    @Test
    public void test() {
        name = SHAFT.Properties.log4j.name();
        appenderConsoleType = SHAFT.Properties.log4j.appenderConsoleType();
        appenderConsoleName = SHAFT.Properties.log4j.appenderConsoleName();
        appenderConsoleLayoutType = SHAFT.Properties.log4j.appenderConsoleLayoutType();
        appenderConsoleLayoutDisableAnsi = SHAFT.Properties.log4j.appenderConsoleLayoutDisableAnsi();
        appenderConsoleLayoutNoConsoleNoAnsi = SHAFT.Properties.log4j.appenderConsoleLayoutNoConsoleNoAnsi();
        appenderConsoleLayoutCharset = SHAFT.Properties.log4j.appenderConsoleLayoutCharset();
        appenderConsoleLayoutPattern = SHAFT.Properties.log4j.appenderConsoleLayoutPattern();
        appenderConsoleFilterThresholdType = SHAFT.Properties.log4j.appenderConsoleFilterThresholdType();
        appenderConsoleFilterThresholdLevel = SHAFT.Properties.log4j.appenderConsoleFilterThresholdLevel();
        appenderFileType = SHAFT.Properties.log4j.appenderFileType();
        appenderFileName = SHAFT.Properties.log4j.appenderFileName();
        appenderFile_FileName = SHAFT.Properties.log4j.appenderFile_FileName();
        appenderFileLayoutType = SHAFT.Properties.log4j.appenderFileLayoutType();
        appenderFileLayoutPattern = SHAFT.Properties.log4j.appenderFileLayoutPattern();
        appenderFileLayoutCharset = SHAFT.Properties.log4j.appenderFileLayoutCharset();
        appenderFileFilterThresholdType = SHAFT.Properties.log4j.appenderFileFilterThresholdType();
        appenderFileFilterThresholdLevel = SHAFT.Properties.log4j.appenderFileFilterThresholdLevel();
        rootLogger = SHAFT.Properties.log4j.rootLogger();
        loggerAppName = SHAFT.Properties.log4j.loggerAppName();
        loggerAppLevel = SHAFT.Properties.log4j.loggerAppLevel();

        Assert.assertTrue(rootLogger.toLowerCase(Locale.ROOT).startsWith("info"),
                "Default root logger level should be info");
        Assert.assertFalse(rootLogger.contains("LOGFILE"),
                "Default root logger should keep file logging disabled until retry diagnostics enable it");
        Assert.assertTrue(rootLogger.contains("ReportPortalAppender"),
                "Default root logger should stay aligned with the bundled default log4j2.properties appenders");
        Assert.assertFalse(appenderConsoleLayoutNoConsoleNoAnsi,
                "Console ANSI output should remain enabled for styled logs even in non-console contexts");
        Assert.assertEquals(appenderConsoleLayoutCharset.toUpperCase(Locale.ROOT), "UTF-8",
                "Console layout charset should be UTF-8 for proper Unicode and Arabic rendering");
        Assert.assertEquals(appenderFileLayoutCharset.toUpperCase(Locale.ROOT), "UTF-8",
                "File layout charset should be UTF-8 for proper Unicode and Arabic rendering");
    }

}
