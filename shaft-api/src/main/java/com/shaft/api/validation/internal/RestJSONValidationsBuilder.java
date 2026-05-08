package com.shaft.api.validation.internal;

import com.shaft.validation.internal.JSONValidationsBuilder;

public class RestJSONValidationsBuilder extends JSONValidationsBuilder {
    public RestJSONValidationsBuilder(RestValidationsBuilder b) {
        super(b.validationCategory, b.validationMethod, b.jsonPath, b.response, b.reportMessageBuilder);
    }
}
