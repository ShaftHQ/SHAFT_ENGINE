package com.shaft.dsl.WebElements;

import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class Label extends Element {

    public Label(WebDriver driver, By locator) {
        super(driver, locator);
    }
    public String getText() {
        return elementActions.getText(locator);
    }
    public void shouldHaveText(String expectedValue) {
        Validations.assertThat().object(getText()).isEqualTo(expectedValue).perform();
    }
}
