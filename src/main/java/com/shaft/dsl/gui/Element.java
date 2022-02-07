package com.shaft.dsl.gui;

import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public abstract class Element {
    By locator;
    WebDriver driver;
    ElementActions elementActions;

    public Element(WebDriver driver, By locator) {
        this.driver = driver;
        this.locator = locator;
        elementActions = new ElementActions(this.driver);
    }

    public void shouldExist()
    {  Validations.assertThat().element(driver,locator).exists().perform();
    }
    public void shouldExist(String reportMsg)
    { Validations.assertThat().element(driver,locator).exists()
            .withCustomReportMessage(reportMsg).perform();
    }
    public void shouldNotExist()
    { Validations.assertThat().element(driver,locator).exists().perform();
    }
    public void shouldNotExist(String reportMsg)
    { Validations.assertThat().element(driver,locator).exists()
            .withCustomReportMessage(reportMsg).perform();
    }
}
