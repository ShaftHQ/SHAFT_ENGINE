package poms;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.validation.Validations;
import lombok.Getter;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class GoogleSearch {
    WebDriver driver;

    @Getter
    static By searchBox_textField = By.xpath("//*[@id='lst-ib' or @class='lst' or @name='q']");
    String url = "https://www.google.com/ncr";

    @SuppressWarnings("SpellCheckingInspection")
    public static By googleLogo_image = By.xpath("//*[@id='hplogo' or @alt='Google']");
    String urlAfterRedirection = "https://www.google.com";

    public GoogleSearch(WebDriver driver) {
        this.driver = driver;
    }

    public void navigateToURL() {
        new BrowserActions(driver).navigateToURL(url, urlAfterRedirection);
    }

    public void assertPageIsOpen() {
            Validations.assertThat()
                    .element(driver, googleLogo_image)
                    .exists()
                    .perform();
    }
}