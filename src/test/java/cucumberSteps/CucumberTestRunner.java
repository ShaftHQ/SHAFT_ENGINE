package cucumberSteps;

import io.cucumber.testng.AbstractTestNGCucumberTests;
import io.cucumber.testng.CucumberOptions;

@CucumberOptions(features = "src//test//resources//CucumberFeatures", strict = true)
public class CucumberTestRunner extends AbstractTestNGCucumberTests {
}
