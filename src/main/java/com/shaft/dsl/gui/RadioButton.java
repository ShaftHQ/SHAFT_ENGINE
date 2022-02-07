package com.shaft.dsl.gui;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class RadioButton extends Button {
    Label lbl;
    public RadioButton(WebDriver driver, By buttonLocator,By labelLoctor) {
        super(driver, buttonLocator);
        lbl= new Label(driver, labelLoctor);
    }

    @Override
    public String getText() {
        return lbl.getText();
    }
}
