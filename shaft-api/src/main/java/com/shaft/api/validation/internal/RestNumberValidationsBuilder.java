package com.shaft.api.validation.internal;

import com.shaft.validation.internal.NumberValidationsBuilder;

public class RestNumberValidationsBuilder extends NumberValidationsBuilder {
    public RestNumberValidationsBuilder(RestValidationsBuilder b) {
        super(b.validationCategory, b.validationMethod, b.jsonPath, b.response, b.reportMessageBuilder);
    }
}
