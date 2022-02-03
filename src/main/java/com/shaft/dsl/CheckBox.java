package com.shaft.dsl;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class CheckBox extends RadioButton{
    public CheckBox(WebDriver driver, By buttonLocator, By labelLoctor) {
        super(driver, buttonLocator, labelLoctor);
    }
}
