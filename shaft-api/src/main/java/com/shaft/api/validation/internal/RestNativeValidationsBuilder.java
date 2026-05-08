package com.shaft.api.validation.internal;

import com.shaft.validation.internal.NativeValidationsBuilder;

public class RestNativeValidationsBuilder extends NativeValidationsBuilder {
    public RestNativeValidationsBuilder(RestValidationsBuilder b) {
        super(b.validationCategory, b.validationMethod, b.jsonPath, b.response, b.reportMessageBuilder);
    }
}
