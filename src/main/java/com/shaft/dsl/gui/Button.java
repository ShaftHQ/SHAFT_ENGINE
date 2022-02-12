package com.shaft.dsl.gui;

import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class Button extends Label {

    public Button(WebDriver driver, By locator) {
        super(driver, locator);
    }
    public void click() {
        elementActions.click(locator);
    }
    public Boolean isEnabled(){
        return Boolean.parseBoolean(elementActions.getAttribute(locator,"disabled"));
    }
    public void shouldBeEnabled()
    {   Validations.assertThat().element(driver,locator).attribute("disabled").isFalse().perform();
    }
    public void shouldBeDisabled()
    {
        Validations.assertThat().element(driver,locator).attribute("disabled").isTrue().perform();
    }

}
