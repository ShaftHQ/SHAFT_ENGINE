package testPackage.validationsWizard;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.*;

public class NegativeValidationsTests {
    private final ThreadLocal<WebDriver> driver = new ThreadLocal<>();
    private final By button = By.cssSelector("button");
    private final By button2 = By.cssSelector("button2");
    private final By checkedBox = By.xpath("//input[@type='checkbox'][2]");
    private final By NotCheckedBox = By.xpath("//input[@type='checkbox'][1]");

    private final String url = "data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button> <div id=\"content\" class=\"large-12 columns\">\n" +
            "        <div class=\"example\">\n" +
            "  <h3>Checkboxes</h3>\n" +
            "  <form id=\"checkboxes\">\n" +
            "    <input type=\"checkbox\"> checkbox 1<br>\n" +
            "    <input type=\"checkbox\" checked=\"\"> checkbox 2\n" +
            "  </form>\n" +
            "</div>\n" +
            "      </div>";

    @Test(expectedExceptions = AssertionError.class)
    public void url_failing() {
        Validations.assertThat().browser(driver.get()).url().contains("google.comm").perform();
    }

    @Test
    public void url_passing() {
        driver.get().navigate().to("https://shafthq.github.io/");
        Validations.assertThat().browser(driver.get()).url().isEqualTo("https://shafthq.github.io/").perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void title_failing() {
        Validations.assertThat().browser(driver.get()).title().contains("ooglem").perform();
    }

    @Test
    public void title_passing() {
        Validations.assertThat().browser(driver.get()).title().isEqualTo("").perform();
    }


    double defaultTimeout = 60;

    @Test(expectedExceptions = AssertionError.class)
    public void exists_failing() {
        Validations.assertThat().element(driver.get(), button2).exists().perform();
    }

    @Test
    public void exists_passing() {
        Validations.assertThat().element(driver.get(), button).exists().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void notExists_failing() {
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

    @Test
    public void notExists_passing() {
        Validations.assertThat().element(driver.get(), button2).doesNotExist().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void cssProperty_failing() {
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

    @Test
    public void cssProperty_passing() {
        Validations.assertThat().element(driver.get(), button).cssProperty("appearance").matchesRegex("(auto|button)").perform();
    }

    @BeforeClass
    public void beforeClass() {
        defaultTimeout = SHAFT.Properties.timeouts.defaultElementIdentificationTimeout();
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(2);
    }

    @AfterClass
    public void afterClass() {
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(defaultTimeout);
    }


    @BeforeMethod
    public void beforeMethod() {
        driver.set(new DriverFactory().getDriver());
        driver.get().navigate().to(url);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
