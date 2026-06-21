package com.shaft.gui.driver;

import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.NativeValidationsBuilder;
import com.shaft.validation.internal.ValidationsExecutor;

/**
 * Public contract for element-level hard/soft validation starters.
 */
public interface ElementAssertions {
    ValidationsExecutor exists();

    ValidationsExecutor doesNotExist();

    ValidationsExecutor matchesReferenceImage();

    ValidationsExecutor matchesReferenceImage(ValidationEnums.VisualValidationEngine visualValidationEngine);

    ValidationsExecutor doesNotMatchReferenceImage();

    ValidationsExecutor doesNotMatchReferenceImage(ValidationEnums.VisualValidationEngine visualValidationEngine);

    NativeValidationsBuilder attribute(String attribute);

    NativeValidationsBuilder domAttribute(String domAttribute);

    NativeValidationsBuilder domProperty(String domProperty);

    NativeValidationsBuilder property(String domProperty);

    ValidationsExecutor isSelected();

    ValidationsExecutor isChecked();

    ValidationsExecutor isVisible();

    ValidationsExecutor isEnabled();

    ValidationsExecutor isNotSelected();

    ValidationsExecutor isNotChecked();

    ValidationsExecutor isHidden();

    ValidationsExecutor isDisabled();

    NativeValidationsBuilder text();

    NativeValidationsBuilder textTrimmed();

    NativeValidationsBuilder cssProperty(String elementCssProperty);
}
