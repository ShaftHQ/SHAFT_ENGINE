package poms;

import com.microsoft.playwright.Page;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionComparisonType;
import com.shaft.validation.Assertions.AssertionType;
import com.shaft.validation.Assertions.ElementAttributeType;
import com.shaft.validation.Validations;
import com.shaft.validation.Verifications;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class GoogleSearchResults {
    WebDriver driver = null;
    Page page = null;

    By resultsStats_label = By.id("appbar");
    String resultsStats_label_stringLocator = "xpath=//div[@id='appbar']";
    By next_button = By.xpath("//span[text()='Next']");
    String next_button_stringLocator = "xpath=//span[text()='Next']";
    By searchResult_box = By.xpath("//div[@class='srg']//div[@class='g']");

    public GoogleSearchResults(WebDriver driver) {
        this.driver = driver;
    }
    
    public GoogleSearchResults(Page page) {
        this.page = page;
    }

    public void assertResultsStatsExistsAndIsNotEmpty() {
    	if(driver != null) {
            Validations.assertThat()
                    .element(driver, resultsStats_label)
                    .text()
                    .doesNotEqual("")
                    .perform();
    	}else {
            Assertions.assertElementAttribute(page, resultsStats_label_stringLocator, ElementAttributeType.TEXT, "", AssertionComparisonType.EQUALS,
                    AssertionType.NEGATIVE);
    	}
    }

    public void verifyResultsStatsExists() {
    	if(driver != null) {
        Verifications.verifyElementExists(driver, resultsStats_label, Verifications.VerificationType.POSITIVE);
    	}else {
    		
    	}
    }

    public void clickNext() {
    	if(driver != null) {
        ElementActions.click(driver, next_button);
    	}else {
    		ElementActions.performElementAction(page).click(next_button_stringLocator);
    	}
    }

    public void assert10ResultsPerPage() {
    	if(driver != null) {
        Assertions.assertEquals(10, ElementActions.getElementsCount(driver, searchResult_box));
    	}else {
    		
    	}
    }

}
