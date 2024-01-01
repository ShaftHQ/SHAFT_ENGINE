package com.shaft.gui.element.internal;

import com.shaft.enums.internal.ElementAction;
import lombok.AllArgsConstructor;
import org.openqa.selenium.By;

@AllArgsConstructor
public class ElementActionInformation {
    public ElementAction action;
    public By locator;
    public String testData;
}
