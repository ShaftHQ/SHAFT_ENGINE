package com.shaft.gui.element;

import com.shaft.gui.element.internal.FluentElementActions;
import org.openqa.selenium.WebDriver;

public class ElementActions extends FluentElementActions {

    public ElementActions() {
        super();
    }

    @SuppressWarnings("unused")
    public ElementActions(WebDriver driver) {
        super();
    }

    public static ElementActions getInstance() {
        return new ElementActions();
    }

}
