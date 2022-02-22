package com.shaft.dsl.gui;

import org.openqa.selenium.By;

public class SearchField extends TextBox {
   public Button clearBtn;

    public SearchField(By textboxLocator, By clearLocator) {
        super(textboxLocator);
        clearBtn = new Button(clearLocator);
    }

    public void clear() {
        clearBtn.click();
    }

}
