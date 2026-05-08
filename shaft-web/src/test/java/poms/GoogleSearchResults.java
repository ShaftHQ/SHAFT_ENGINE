package poms;

import com.shaft.gui.element.ElementActions;
import com.shaft.validation.WebValidations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class GoogleSearchResults {
    WebDriver driver;
    By resultsStats_label = By.id("appbar");
    By next_button = By.xpath("//span[text()='Next']");

    public GoogleSearchResults(WebDriver driver) {
        this.driver = driver;
    }

    public void assertResultsStatsExistsAndIsNotEmpty() {
            WebValidations.assertThat()
                    .element(driver, resultsStats_label)
                    .text()
                    .doesNotEqual("")
                    .perform();
    }

    public void clickNext() {
        new ElementActions(driver).scrollToElement(next_button).click(next_button);
    }

}
