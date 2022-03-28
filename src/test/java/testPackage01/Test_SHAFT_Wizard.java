package testPackage01;

import com.shaft.api.RestActions;
import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.Test;

public class Test_SHAFT_Wizard {
    @Test
    public void gui() {
        var driver = SHAFT.GUI.WebDriver.getDriver();

        SHAFT.GUI.WebDriver.performBrowserAction(driver)
                .navigateToURL("https://www.google.com/");

        SHAFT.GUI.WebDriver.performElementAction(driver)
                .type(By.name("q"), "SHAFT_Engine")
                .keyPress(By.name("q"), Keys.ENTER);

        SHAFT.Validations.assertThat()
                .element(driver, By.id("result-stats"))
                .text()
                .doesNotEqual("").perform();

        SHAFT.GUI.WebDriver.closeDriver();
    }

    @Test
    public void api() {
        var apiObject = SHAFT.API.getDriver("https://jsonplaceholder.typicode.com");
        var users = apiObject.buildNewRequest("/users", RestActions.RequestType.GET).setTargetStatusCode(200).performRequest();

        RestActions.getResponseJSONValueAsList(users, "$").forEach(user -> {
            if (RestActions.getResponseJSONValue(user, "name").equals("Leanne Graham")) {
                Validations.verifyThat().response(user).extractedJsonValue("email").isEqualTo("Sincere@april.biz").perform();
                Validations.assertThat().response(user).extractedJsonValue("username").isEqualTo("Bret").perform();
            }
        });
    }

    @Test
    public void cli() {
//        SHAFT.CLI.performCLIAction().performTerminalCommand("ls");
        var string = SHAFT.CLI.performFileAction().readFromFile("src/test/resources/testDataFiles/JsonFileTest.json");
        SHAFT.Report.attach("JSON", "Custom Attachment", string);
    }

    @Test
    public void testData() {
        var string2 = SHAFT.TestData.pdf("src/test/resources/testDataFiles/sample.pdf").readFileContent();
        SHAFT.Report.attach("PDF", "Custom Attachment", string2);
    }
}
