package testPackage.validationsWizard;

import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class NegativeValidationsTests {
    private final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
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
        driver.get().browser().assertThat().url().contains("google.comm").perform();
    }

    @Test
    public void url_passing() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/");
        driver.get().browser().assertThat().url().isEqualTo("https://shafthq.github.io/").perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void title_failing() {
        driver.get().browser().assertThat().title().contains("ooglem").perform();
    }

    @Test
    public void title_passing() {
        driver.get().browser().assertThat().title().isEqualTo("").perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void exists_failing() {
        driver.get().element().assertThat(button2).exists().perform();
    }

    @Test
    public void exists_passing() {
        driver.get().element().assertThat(button).exists().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void notExists_failing() {
        driver.get().element().assertThat(button).doesNotExist().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void matchesReferenceImage() {
        driver.get().element().assertThat(button2).matchesReferenceImage().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void validateEquals_failing() {
        Validations.assertThat().object("zz").isEqualTo("aa");
    }

    @Test
    public void validateEquals_passing() {
        Validations.assertThat().object("aa").isEqualTo("aa");
    }

    @Test(expectedExceptions = AssertionError.class)
    public void isVisible() {
        driver.get().element().assertThat(button2).isVisible().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void isEnabled() {
        driver.get().element().assertThat(button).isDisabled().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void isChecked() {
        driver.get().element().assertThat(button).isChecked().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void isNotSelected() {
        driver.get().element().assertThat(button).isSelected().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void attribute() {
        driver.get().element().assertThat(button).attribute("alt").isEqualTo("Googlee").perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void text() {
        driver.get().element().assertThat(button).text().isEqualTo("Goo").perform();
    }

    @Test
    public void notExists_passing() {
        driver.get().element().assertThat(button2).doesNotExist().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void cssProperty_failing() {
        driver.get().element().assertThat(button).cssProperty("appearance").matchesRegex("(autoo|buttonn)").perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void isNotChecked() {
        driver.get().element().assertThat(checkedBox).isNotChecked().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    public void isSelected() {
        driver.get().element().assertThat(NotCheckedBox).isSelected().perform();
    }

    @Test
    public void cssProperty_passing() {
        driver.get().element().assertThat(button).cssProperty("appearance").matchesRegex("(auto|button)").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL(url);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
