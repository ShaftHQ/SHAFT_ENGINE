package com.shaft.dsl.WebElements;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class RadioButton extends Button {
    Button btn;
    Label lbl;
    public RadioButton(WebDriver driver, By buttonLocator,By labelLoctor) {
        super(driver, buttonLocator);
        btn = new Button(driver, buttonLocator);
        lbl= new Label(driver, labelLoctor);
    }

    @Override
    public String getText() {
        return lbl.getText();
    }
}
