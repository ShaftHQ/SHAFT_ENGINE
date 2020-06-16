package cucumberSteps;

import io.cucumber.testng.AbstractTestNGCucumberTests;
import io.cucumber.testng.CucumberOptions;
import org.testng.annotations.Listeners;

@CucumberOptions(features = "src//test//resources//CucumberFeatures")
@Listeners({com.shaft.tools.listeners.AlterSuiteListener.class, com.shaft.tools.listeners.SuiteListener.class, com.shaft.tools.listeners.InvokedMethodListener.class})
public class CucumberTestRunner extends AbstractTestNGCucumberTests {
}
