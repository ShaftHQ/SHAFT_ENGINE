package com.shaft.api.validation.internal;

import com.shaft.validation.internal.ValidationsExecutor;

public class RestValidationsExecutor extends ValidationsExecutor {
    public RestValidationsExecutor(RestValidationsBuilder b) {
        super(b.validationCategory, b.validationType, b.validationMethod, b.reportMessageBuilder);
        setResponse(b.response);
        this.fileAbsolutePath = b.fileAbsolutePath;
        this.restComparisonType = b.restComparisonType;
    }
}
