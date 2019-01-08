package cucumberSteps;

import cucumber.api.CucumberOptions;
import cucumber.api.testng.AbstractTestNGCucumberTests;

@CucumberOptions(features="src//test//resources//CucumberFeatures")
public class CucumberTestRunner extends AbstractTestNGCucumberTests {
}
