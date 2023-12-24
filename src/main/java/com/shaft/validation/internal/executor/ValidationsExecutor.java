package com.shaft.validation.internal.executor;

public interface ValidationsExecutor {
    /**
     * Analyzes the provided data to perform the validation and return a chainable object
     */
    void perform();

    /**
     * Set a customized business-readable message that will appear in the execution report instead of the technical log message which will be nested under it
     *
     * @param customReportMessage the message that you would like to describe this validation in the execution report
     * @return the current ValidationsExecutor object so that you can call the "perform()" method and execute this validation
     */
    ValidationsExecutor withCustomReportMessage(String customReportMessage);
}
