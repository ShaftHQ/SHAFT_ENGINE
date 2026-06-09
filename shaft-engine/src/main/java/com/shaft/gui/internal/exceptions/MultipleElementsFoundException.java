package com.shaft.gui.internal.exceptions;

import org.openqa.selenium.WebDriverException;

public class MultipleElementsFoundException extends WebDriverException {
    public MultipleElementsFoundException() {
        super();
    }

    public MultipleElementsFoundException(String message) {
        super(message);
    }

    public MultipleElementsFoundException(Throwable cause) {
        super(cause);
    }

    public MultipleElementsFoundException(String message, Throwable cause) {
        super(message, cause);
    }
}
