package com.shaft.dsl.gui;

import org.openqa.selenium.By;

public class CheckBox extends RadioButton{
    public CheckBox(By buttonLocator,By statusLocator) {
        super(buttonLocator,statusLocator);
    }
}
