package testPackage01;

import org.testng.annotations.Test;

import cucumber.api.CucumberOptions;
import cucumber.api.testng.AbstractTestNGCucumberTests;

@CucumberOptions(features = "cucumberFeatures", glue = "classpath:", tags="@SmokeTest")
@Test
public class CucumberTestRunner extends AbstractTestNGCucumberTests {

}