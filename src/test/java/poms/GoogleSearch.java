package poms;

import com.shaft.enums.internal.ClipboardAction;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import lombok.Getter;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
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

    public void searchForQuery(String searchQuery) {
        new ElementActions(driver).type(searchBox_textField, searchQuery);
        new ElementActions(driver).keyPress(searchBox_textField, Keys.ENTER);
    }

    public void typeQuery(String searchQuery) {
        new ElementActions(driver).type(searchBox_textField, searchQuery);
    }

    public void copyQuery() {
        new ElementActions(driver).clipboardActions(searchBox_textField, ClipboardAction.COPY);
    }

    public void pasteQuery() {
        new ElementActions(driver).clipboardActions(searchBox_textField, ClipboardAction.PASTE);
    }

    public void cutQuery() {
        new ElementActions(driver).clipboardActions(searchBox_textField, ClipboardAction.CUT);
    }

    public void selectQuery() {
        new ElementActions(driver).clipboardActions(searchBox_textField, ClipboardAction.SELECT_ALL);
    }

    public void unSelectQuery() {
        new ElementActions(driver).clipboardActions(searchBox_textField, ClipboardAction.UNSELECT_ALL);
    }

    public void assertPageIsOpen() {
            Validations.assertThat()
                    .element(driver, googleLogo_image)
                    .exists()
                    .perform();
    }
}