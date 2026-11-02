package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

public class CucumberTests {
    boolean cucumberAnsiColorsDisabled;
    boolean cucumberExecutionDryRun;
    String cucumberExecutionLimit;
    String cucumberExecutionOrder;
    boolean cucumberExecutionStrict;
    String cucumberFeatures;
    String cucumberFilterName;
    String cucumberFilterTags;
    String cucumberGlue;
    String cucumberPlugin;
    String cucumberObjectFactory;
    String cucumberSnippetType;
    boolean cucumberPublishQuiet;

    @Test
    public void test() {
        cucumberAnsiColorsDisabled = SHAFT.Properties.cucumber.cucumberAnsiColorsDisabled();
        cucumberExecutionDryRun = SHAFT.Properties.cucumber.cucumberExecutionDryRun();
        cucumberExecutionLimit = SHAFT.Properties.cucumber.cucumberExecutionLimit();
        cucumberExecutionOrder = SHAFT.Properties.cucumber.cucumberExecutionOrder();
        cucumberExecutionStrict = SHAFT.Properties.cucumber.cucumberExecutionStrict();
        cucumberFeatures = SHAFT.Properties.cucumber.cucumberFeatures();
        cucumberFilterName = SHAFT.Properties.cucumber.cucumberFilterName();
        cucumberFilterTags = SHAFT.Properties.cucumber.cucumberFilterTags();
        cucumberGlue = SHAFT.Properties.cucumber.cucumberGlue();
        cucumberPlugin = SHAFT.Properties.cucumber.cucumberPlugin();
        cucumberObjectFactory = SHAFT.Properties.cucumber.cucumberObjectFactory();
        cucumberSnippetType = SHAFT.Properties.cucumber.cucumberSnippetType();
        cucumberPublishQuiet = SHAFT.Properties.cucumber.cucumberPublishQuiet();
    }
}
