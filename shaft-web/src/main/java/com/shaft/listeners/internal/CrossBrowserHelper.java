package com.shaft.listeners.internal;

import com.shaft.enums.internal.Screenshots;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.Platform;
import org.openqa.selenium.remote.Browser;
import org.testng.xml.XmlSuite;
import org.testng.xml.XmlTest;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * Selenium-aware cross-browser suite configurator.
 * Stays in shaft-engine (not shaft-core) so it can reference Selenium Browser/Platform types.
 * TestNGListenerHelper delegates here via reflection when shaft-web is on the classpath.
 */
public class CrossBrowserHelper {

    private CrossBrowserHelper() {}

    public static void configure(List<XmlSuite> suites) {
        String crossBrowserMode = Properties.platform.crossBrowserMode();
        List<Browser> supportedBrowsers = Arrays.asList(Browser.CHROME, Browser.SAFARI, Browser.FIREFOX, Browser.EDGE);
        if (!"off".equals(crossBrowserMode)) {
            suites.forEach(suite -> {
                supportedBrowsers.forEach(supportedBrowser ->
                        createTestSuite((XmlTest) suite.getTests().getFirst().clone(), supportedBrowser));
                suite.getTests().removeFirst();
                if (crossBrowserMode.equals("parallelized")) {
                    suite.setParallel(XmlSuite.ParallelMode.TESTS);
                    suite.setThreadCount(supportedBrowsers.size());
                    Properties.visuals.set().videoParamsRecordVideo(true);
                    Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
                }
            });
        }
    }

    private static void createTestSuite(XmlTest xmlTest, Browser browser) {
        xmlTest.setParameters(Map.of(
                "executionAddress", "dockerized",
                "targetOperatingSystem", Platform.LINUX.name(),
                "targetBrowserName", browser.browserName()
        ));
        double threadCount = Properties.testNG.threadCount();
        if ("DYNAMIC".equals(Properties.testNG.parallelMode())) {
            threadCount = threadCount * Runtime.getRuntime().availableProcessors();
            xmlTest.setThreadCount((int) Math.floor(threadCount));
        } else {
            xmlTest.setThreadCount((int) Math.floor(threadCount));
        }
        xmlTest.setParallel(XmlSuite.ParallelMode.valueOf(Properties.testNG.parallel()));
        xmlTest.setName(xmlTest.getName() + " - " + browser);
    }
}
