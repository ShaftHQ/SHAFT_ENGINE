package com.shaft.dsl.gui;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class Button extends Label {

    public Button(WebDriver driver, By locator) {
        super(driver, locator);
    }
    public void click() {
        elementActions.click(locator);
    }
    public String getText() {
        return elementActions.getText(locator);
    }
    public Boolean isEnabled(){
        return Boolean.parseBoolean(elementActions.getAttribute(locator,"disabled"));
    }

}
