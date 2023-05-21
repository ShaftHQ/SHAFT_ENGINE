package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

public class Log4jTests {
    String name;
    String appenderConsoleType;
    String appenderConsoleName;
    String appenderConsoleLayoutType;
    boolean appenderConsoleLayoutDisableAnsi;
    String appenderConsoleLayoutPattern;
    String appenderConsoleFilterThresholdType;
    String appenderConsoleFilterThresholdLevel;
    String appenderFileType;
    String appenderFileName;
    String appenderFile_FileName;
    String appenderFileLayoutType;
    String appenderFileLayoutPattern;
    String appenderFileFilterThresholdType;
    String appenderFileFilterThresholdLevel;
    String rootLogger;
    String loggerAppName;
    String loggerAppLevel;

    @Test
    public void test() {
        name = SHAFT.Properties.log4j.name();
        appenderConsoleType = SHAFT.Properties.log4j.appenderConsoleType();
        appenderConsoleName = SHAFT.Properties.log4j.appenderConsoleName();
        appenderConsoleLayoutType = SHAFT.Properties.log4j.appenderConsoleLayoutType();
        appenderConsoleLayoutDisableAnsi = SHAFT.Properties.log4j.appenderConsoleLayoutDisableAnsi();
        appenderConsoleLayoutPattern = SHAFT.Properties.log4j.appenderConsoleLayoutPattern();
        appenderConsoleFilterThresholdType = SHAFT.Properties.log4j.appenderConsoleFilterThresholdType();
        appenderConsoleFilterThresholdLevel = SHAFT.Properties.log4j.appenderConsoleFilterThresholdLevel();
        appenderFileType = SHAFT.Properties.log4j.appenderFileType();
        appenderFileName = SHAFT.Properties.log4j.appenderFileName();
        appenderFile_FileName = SHAFT.Properties.log4j.appenderFile_FileName();
        appenderFileLayoutType = SHAFT.Properties.log4j.appenderFileLayoutType();
        appenderFileLayoutPattern = SHAFT.Properties.log4j.appenderFileLayoutPattern();
        appenderFileFilterThresholdType = SHAFT.Properties.log4j.appenderFileFilterThresholdType();
        appenderFileFilterThresholdLevel = SHAFT.Properties.log4j.appenderFileFilterThresholdLevel();
        rootLogger = SHAFT.Properties.log4j.rootLogger();
        loggerAppName = SHAFT.Properties.log4j.loggerAppName();
        loggerAppLevel = SHAFT.Properties.log4j.loggerAppLevel();
    }

}
