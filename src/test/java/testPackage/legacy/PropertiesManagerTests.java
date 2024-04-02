package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;

public class PropertiesManagerTests {

    //@BeforeMethod
    public void setup() {
        Properties.web.set().targetBrowserName("firefox");
        Properties.browserStack.set().local(true);
        Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(true);
        Properties.healenium.set().imitatePort(8080);
        Properties.jira.set().jiraInteraction(true);
        Properties.mobile.set().selfManaged(true);
        Properties.paths.set().properties("src/main/resources/properties/");
        Properties.pattern.set().testDataColumnNamePrefix("Data");
        Properties.platform.set().executionAddress("0.0.0.0:4723");
        Properties.reporting.set().captureElementName(true);
        Properties.timeouts.set().waitForLazyLoading(true);
        Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("ValidationPointsOnly");


    }

    //@Test
    public void testProperties() {
        SHAFT.Validations.assertThat().object(Properties.web.targetBrowserName()).equals("firefox");
        SHAFT.Validations.assertThat().object(Properties.browserStack.local()).equals(true);
        SHAFT.Validations.assertThat().object(Properties.cucumber.cucumberFeatures()).equals("src/test/resources");
        SHAFT.Validations.assertThat().object(Properties.flags.clickUsingJavascriptWhenWebDriverClickFails()).equals(true);
        SHAFT.Validations.assertThat().object(Properties.healenium.imitatePort()).equals("8080");
        SHAFT.Validations.assertThat().object(Properties.internal.allureVersion()).equals("2.2.1");
        SHAFT.Validations.assertThat().object(Properties.jira.isEnabled()).equals(true);
        SHAFT.Validations.assertThat().object(Properties.paths.properties()).equals("src/main/resources/properties/");
        SHAFT.Validations.assertThat().object(Properties.pattern.testDataColumnNamePrefix()).equals("Data");
        SHAFT.Validations.assertThat().object(Properties.platform.executionAddress()).equals("0.0.0.0:4723");
        SHAFT.Validations.assertThat().object(Properties.log4j.name()).equals("PropertiesConfig");
        SHAFT.Validations.assertThat().object(Properties.reporting.captureElementName()).equals(true);
        SHAFT.Validations.assertThat().object(Properties.timeouts.waitForLazyLoading()).equals(true);
        SHAFT.Validations.assertThat().object(Properties.visuals.screenshotParamsWhenToTakeAScreenshot()).equals("ValidationPointsOnly");
    }
}
