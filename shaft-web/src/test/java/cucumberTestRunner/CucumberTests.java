package cucumberTestRunner;

import io.cucumber.testng.AbstractTestNGCucumberTests;
import org.testng.annotations.Listeners;

@Listeners({com.shaft.listeners.TestNGListener.class})
public class CucumberTests extends AbstractTestNGCucumberTests {
}