package com.shaft.gui.internal.exceptions;

public class MultipleElementsFoundException extends Exception {
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
