package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class PathsTests {
    String properties;
    String defaultProperties;
    String dynamicObjectRepository;
    String testData;
    String downloads;
    String allureResults;
    String extentReports;
    String executionSummaryReport;
    String video;
    String applitoolsApiKey;


    @BeforeClass
    public void beforeClass() {
        properties = SHAFT.Properties.paths.properties();
        defaultProperties = SHAFT.Properties.paths.defaultProperties();
        dynamicObjectRepository = SHAFT.Properties.paths.dynamicObjectRepository();
        testData = SHAFT.Properties.paths.testData();
        downloads = SHAFT.Properties.paths.downloads();
        allureResults = SHAFT.Properties.paths.allureResults();
        extentReports = SHAFT.Properties.paths.extentReports();
        executionSummaryReport = SHAFT.Properties.paths.executionSummaryReport();
        video = SHAFT.Properties.paths.video();
        applitoolsApiKey = SHAFT.Properties.paths.applitoolsApiKey();

    }

    @Test
    public void test() {
        SHAFT.Properties.paths.set().properties(properties);
        SHAFT.Properties.paths.set().dynamicObjectRepository(dynamicObjectRepository);
        SHAFT.Properties.paths.set().testData(testData);
        SHAFT.Properties.paths.set().downloads(downloads);
        SHAFT.Properties.paths.set().allureResults(allureResults);
        SHAFT.Properties.paths.set().extentReports(extentReports);
        SHAFT.Properties.paths.set().executionSummaryReport(executionSummaryReport);
        SHAFT.Properties.paths.set().video(video);
        SHAFT.Properties.paths.set().applitoolsApiKey(applitoolsApiKey);


    }
}
