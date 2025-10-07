package {{PACKAGE_NAME}};

import io.cucumber.testng.AbstractTestNGCucumberTests;
import io.cucumber.testng.CucumberOptions;

@CucumberOptions(
        features = "src/test/resources/features",
        glue = "{{PACKAGE_NAME}}",
        plugin = {
                "pretty",
                "html:target/cucumber-reports/cucumber.html",
                "json:target/cucumber-reports/cucumber.json",
                "com.shaft.listeners.CucumberFeatureListener"
        }
)
public class TestRunner extends AbstractTestNGCucumberTests {
}
