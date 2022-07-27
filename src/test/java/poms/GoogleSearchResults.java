package poms;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class GoogleSearchResults {
    WebDriver driver;
    By resultsStats_label = By.id("appbar");
    By next_button = By.xpath("//span[text()='Next']");
    By searchResult_box = By.xpath("//div[@class='srg']//div[@class='g']");

    public GoogleSearchResults(WebDriver driver) {
        this.driver = driver;
    }

    public void assertResultsStatsExistsAndIsNotEmpty() {
            Validations.assertThat()
                    .element(driver, resultsStats_label)
                    .text()
                    .doesNotEqual("")
                    .perform();
    }

    public void verifyResultsStatsExists() {
            Validations.verifyThat().element(driver, resultsStats_label)
                    .exists().perform();
    }

    public void clickNext() {
        ElementActions.click(driver, next_button);
    }

    public void assert10ResultsPerPage() {
        SHAFT.Validations.assertThat().number(10).equals(ElementActions.getElementsCount(driver, searchResult_box));
    }

}
