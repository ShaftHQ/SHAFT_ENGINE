package com.shaft.dsl.gui;

import com.shaft.validation.Validations;
import org.openqa.selenium.By;

public class Button extends Label {

    public static final String DISABLED = "disabled";

    public Button(By locator) {
        super(locator);
    }
    public void click() {
        elementActions.click(locator);
    }
    public Boolean isEnabled(){
        return Boolean.parseBoolean(elementActions.getAttribute(locator,DISABLED));
    }
    public void shouldBeEnabled()
    {   Validations.assertThat().element(driver,locator).attribute(DISABLED).isFalse().perform();
    }
    public void shouldBeDisabled()
    {
        Validations.assertThat().element(driver,locator).attribute(DISABLED).isTrue().perform();
    }
    public void shouldBeEnabled()
    {   Validations.assertThat().element(driver,locator).attribute("disabled").isFalse().perform();
    }
    public void shouldBeDisabled()
    {
        Validations.assertThat().element(driver,locator).attribute("disabled").isTrue().perform();
    }

}
