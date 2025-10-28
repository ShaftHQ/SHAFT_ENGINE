package testPackage.coverage;

import org.openqa.selenium.By;
import org.testng.annotations.Test;
import testPackage.Tests;

public class ClipboardActionsTests extends Tests {
    private static final By searchBox_textField = By.xpath("//*[@id='lst-ib' or @class='lst' or @name='q']");

    @Test
    public void typeTextAndCopyPaste() {
        driver.get()
                .browser().navigateToURL("https://www.google.com/ncr")
                .and().element()
                .clipboard().deleteAll(searchBox_textField)
                .type(searchBox_textField, "FIRST")
                .clipboard().copyAll(searchBox_textField)
                .clipboard().paste(searchBox_textField)
                .clipboard().cutAll(searchBox_textField)
                .clipboard().paste(searchBox_textField)
                .and().assertThat(searchBox_textField).text().isEqualTo("FIRSTFIRST");
    }
}