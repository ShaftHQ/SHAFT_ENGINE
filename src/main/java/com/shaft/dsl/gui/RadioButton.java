package com.shaft.dsl.gui;

import com.shaft.validation.Validations;
import org.openqa.selenium.By;


public class RadioButton extends Button {
    public static final String SELECTED = "selected";
    By selectedLocator;

    public RadioButton(By buttonLocator, By selectedLocator) {
        super(buttonLocator);
        this.selectedLocator = selectedLocator;
    }

    public void select() {
        if (!(isSelected())) {
            click();
        }
    }

    public void unselect() {
        if ((isSelected())) {
            click();
        }
    }

    public boolean isSelected() {
        return (elementActions.getAttribute(selectedLocator, SELECTED) != null);
    }

    public void shouldBeSelected() {
        Validations.assertThat().object(isSelected()).isTrue().perform();
    }

    public void shouldBeSelected(String reportMsg) {
        Validations.assertThat().object(isSelected()).isTrue().withCustomReportMessage(reportMsg).perform();
    }
}
