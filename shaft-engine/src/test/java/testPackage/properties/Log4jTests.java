package testPackage.properties;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.PropertiesHelper;
import org.testng.Assert;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import java.util.Locale;

public class Log4jTests {

    @BeforeClass(alwaysRun = true)
    public void beforeClass() {
        PropertiesHelper.initialize();
    }

    @Test
    public void testLog4jConfigurationDefaults() {
        boolean appenderConsoleLayoutNoConsoleNoAnsi = SHAFT.Properties.log4j.appenderConsoleLayoutNoConsoleNoAnsi();
        String appenderConsoleLayoutCharset = SHAFT.Properties.log4j.appenderConsoleLayoutCharset();
        String appenderFileLayoutCharset = SHAFT.Properties.log4j.appenderFileLayoutCharset();
        String rootLogger = SHAFT.Properties.log4j.rootLogger();
        String jvmFileEncoding = System.getProperty("file.encoding", "");
        String jvmStdoutEncoding = System.getProperty("stdout.encoding", "");
        String jvmStderrEncoding = System.getProperty("stderr.encoding", "");

        Assert.assertTrue(rootLogger.toLowerCase(Locale.ROOT).startsWith("info"),
                "Default root logger level should be info");
        Assert.assertTrue(rootLogger.contains("ASYNC_STDOUT"),
                "Default root logger should write console output through the async appender");
        Assert.assertTrue(rootLogger.contains("ASYNC_LOGFILE"),
                "Default root logger should write the engine log through the async file appender");
        Assert.assertTrue(rootLogger.contains("ASYNC_REPORT_PORTAL"),
                "Default root logger should stay aligned with the bundled default log4j2.properties appenders");
        Assert.assertFalse(appenderConsoleLayoutNoConsoleNoAnsi,
                "Console ANSI output should remain enabled for styled logs even in non-console contexts");
        Assert.assertEquals(appenderConsoleLayoutCharset.toUpperCase(Locale.ROOT), "UTF-8",
                "Console layout charset should be UTF-8 for proper Unicode and Arabic rendering");
        Assert.assertEquals(appenderFileLayoutCharset.toUpperCase(Locale.ROOT), "UTF-8",
                "File layout charset should be UTF-8 for proper Unicode and Arabic rendering");
        Assert.assertEquals(jvmFileEncoding.toUpperCase(Locale.ROOT), "UTF-8",
                "JVM file.encoding should be UTF-8 to keep Unicode test data stable across environments");
        Assert.assertEquals(jvmStdoutEncoding.toUpperCase(Locale.ROOT), "UTF-8",
                "JVM stdout.encoding should be UTF-8 to avoid garbled console output");
        Assert.assertEquals(jvmStderrEncoding.toUpperCase(Locale.ROOT), "UTF-8",
                "JVM stderr.encoding should be UTF-8 to avoid garbled error output");
    }

}
