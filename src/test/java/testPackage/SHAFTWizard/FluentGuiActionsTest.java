package testPackage.SHAFTWizard;

import com.shaft.driver.SHAFT;
import io.qameta.allure.Description;
import io.qameta.allure.Epic;
import io.qameta.allure.Feature;
import io.qameta.allure.Step;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

@Epic("SHAFT Demo")
@Feature("GUI Tests")
public class FluentGuiActionsTest {
    private static SHAFT.TestData.JSON testData;
    private final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>(); // initializing a ThreadLocal driver to enable parallel execution
    // Note: you don't have to use thread local, but it is highly recommended since selenium/appium are not natively thread safe
    private final By email_input = By.id("email"); // this is a traditional By locator
    private final By password_label = SHAFT.GUI.Locator.hasTagName("label").hasText("Password").build(); // this is using the new SHAFT locator library
    private final By password_input = SHAFT.GUI.Locator.hasAnyTagName().relativeBy().toRightOf(password_label);     // this is using the new SHAFT locator library powered by selenium 4 relative locators
    private final By register_button = By.xpath("//button[@type='submit']");
    // Note: remember that this is only a technical POC, but for real life cases just use the ID... if you find it :D
    private final By header_label = By.tagName("h2");
    private final By confirmationText_label = SHAFT.GUI.Locator.hasTagName("div").relativeBy().below(header_label); // this is a valid example where it's easier to use this approach than to write a regular xpath

    @Description("When I submit the registration form with valid information\nThen the same information will be displayed on the confirmation page")
    @Test(description = "Fluent GUI Actions Demo")
    public void fluentGuiActions() {
        String validEmail = System.currentTimeMillis() + "_" + testData.getTestData("validEmail"); // we append the system time to ensure a random email is used, the static part is read from an external test data file
        String validPassword = testData.getTestData("validPassword"); // this is also read from an external test data file

        var passwordLocatorPrefix = "psw"; // assuming that you get this value in runtime
        // this will be used later to demo dynamic locators
        By passwordConfirm_input = By.id(passwordLocatorPrefix + "-repeat"); // you can append the dynamic value to the pre-known static part

        // the below code block demonstrates the full potential of fluent design on the technical level
        // Note that it's not recommended to write technical actions in your test method

        // Note: "and()" is syntactic sugar, to make the code more readable, you can omit it
        // Note: you can now chain browser actions, element actions, and end with an assertion or verification
        driver.get().browser().navigateToURL(TestHelpers.registrationForm).and()
                .element().type(email_input, validEmail).and()
                .type(password_input, validPassword).and()
                .type(passwordConfirm_input, validPassword).and()
                .click(register_button).and()
                .assertThat(confirmationText_label).text().contains("email=" + validEmail + "&psw=" + validPassword + "&psw-repeat=" + validPassword)
                .withCustomReportMessage("Confirming that the same data used to register is displayed successfully.").perform();
        // Note: using a custom report message is always helpful to make the report more business readable

        // this is how the above code can look after implementing fluent page object model design pattern.
        // new RegistrationForm().navigate().registerWithValidData().assertSuccessfulRegistration();
    }


    @Step("When the test data is loaded.")
    @BeforeClass(description = "Initializing test data.")
    public void beforeClass() {
        testData = new SHAFT.TestData.JSON("credentials.json"); // initializing the test data file once to optimize performance
    }

    @Step("And the browser is open.")
    @BeforeMethod(description = "Launching browser.")
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @Step("And the browser is closed.")
    @AfterMethod(description = "Quitting browser.", alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
