package testPackage.validationsWizard;

import com.shaft.driver.DriverFactory;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class NegativeValidationsTests {
    private final ThreadLocal<WebDriver> driver = new ThreadLocal<>();
    private final By button = By.cssSelector("button");
    private final By button2 = By.cssSelector("button2");
    private final By checkedBox = By.xpath("//input[@type='checkbox'][2]");
    private final By NotCheckedBox = By.xpath("//input[@type='checkbox'][1]");


    @Test(expectedExceptions = AssertionError.class)
    public void url() {
        Validations.assertThat().browser(driver.get()).url().contains("google.comm").perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void title() {
        Validations.assertThat().browser(driver.get()).title().contains("ooglem").perform();
    }


    @Test(expectedExceptions = AssertionError.class)
    public void exists() {
        Validations.assertThat().element(driver.get(), button2).exists().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void notExists() {
        Validations.assertThat().element(driver.get(), button).doesNotExist().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void matchesReferenceImage() {
        Validations.assertThat().element(driver.get(), button2).matchesReferenceImage().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void isVisible() {
        Validations.assertThat().element(driver.get(), button2).isVisible().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void isEnabled() {
        Validations.assertThat().element(driver.get(), button).isDisabled().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void isChecked() {
        Validations.assertThat().element(driver.get(), button).isChecked().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void isNotSelected() {
        Validations.assertThat().element(driver.get(), button).isSelected().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void attribute() {
        Validations.assertThat().element(driver.get(), button).attribute("alt").isEqualTo("Googlee").perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void text() {
        Validations.assertThat().element(driver.get(), button).text().isEqualTo("Goo").perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void cssProperty() {
        Validations.assertThat().element(driver.get(), button).cssProperty("appearance").matchesRegex("(autoo|buttonn)").perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void isNotChecked() {
        Validations.assertThat().element(driver.get(), checkedBox).isNotChecked().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void isSelected() {
        Validations.assertThat().element(driver.get(), NotCheckedBox).isSelected().perform();
    }




    @BeforeMethod
    public void beforeMethod() {
        driver.set(new DriverFactory().getDriver());
        driver.get().navigate().to("data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button> <div id=\"content\" class=\"large-12 columns\">\n" +
                "        <div class=\"example\">\n" +
                "  <h3>Checkboxes</h3>\n" +
                "  <form id=\"checkboxes\">\n" +
                "    <input type=\"checkbox\"> checkbox 1<br>\n" +
                "    <input type=\"checkbox\" checked=\"\"> checkbox 2\n" +
                "  </form>\n" +
                "</div>\n" +
                "      </div>");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
