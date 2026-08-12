package com.shaft.gui.driver;

import com.shaft.validation.ValidationEnums;
import com.shaft.validation.VisualComparisonOptions;
import com.shaft.validation.internal.NativeValidationsBuilder;
import com.shaft.validation.internal.ValidationsExecutor;
import com.shaft.gui.ocr.OcrOptions;

/**
 * Public contract for element-level hard/soft validation starters.
 */
public interface ElementAssertions {
    /** Starts a comparison against the number of elements resolved by this target. */
    default NativeValidationsBuilder elementCount() {
        throw unsupported("elementCount");
    }

    /** Starts a comparison against the element's backend-neutral rectangle. */
    default NativeValidationsBuilder elementRectangle() {
        throw unsupported("elementRectangle");
    }

    /** Starts a comparison against the element's computed accessible name. */
    default NativeValidationsBuilder elementAccessibleName() {
        throw unsupported("elementAccessibleName");
    }

    /** Starts a comparison against the element's computed accessibility role. */
    default NativeValidationsBuilder elementRole() {
        throw unsupported("elementRole");
    }

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

    /** Recognizes the target element screenshot and starts a native string assertion. */
    default NativeValidationsBuilder ocrText() {
        return ocrText(OcrOptions.defaults());
    }

    /** Recognizes the target element screenshot with explicit OCR options. */
    default NativeValidationsBuilder ocrText(OcrOptions options) {
        throw unsupported("ocrText");
    }

    NativeValidationsBuilder cssProperty(String elementCssProperty);

    /**
     * Asserts that the element matches its baseline screenshot. Executes immediately, like every other
     * assertion.
     *
     * @return a ValidationsExecutor object to optionally set a custom validation message
     */
    default ValidationsExecutor matchesScreenshot() {
        throw new UnsupportedOperationException("matchesScreenshot is not supported by this element assertions implementation.");
    }

    /**
     * Asserts that the element matches its baseline screenshot, using the given diff-budget/mask
     * options (see {@link VisualComparisonOptions}). Executes immediately.
     *
     * @param options the visual comparison options (diff budgets, masks), or {@code null} for defaults
     * @return a ValidationsExecutor object to optionally set a custom validation message
     */
    default ValidationsExecutor matchesScreenshot(VisualComparisonOptions options) {
        throw new UnsupportedOperationException("matchesScreenshot is not supported by this element assertions implementation.");
    }

    /**
     * Starts an accessible-name-tree regression assertion against the element's baseline aria snapshot.
     *
     * @param snapshotFileName the baseline file name (under the configured aria snapshot folder) to compare against or create
     * @return a ValidationsExecutor object retained for source compatibility
     */
    default ValidationsExecutor matchesAriaSnapshot(String snapshotFileName) {
        throw new UnsupportedOperationException("matchesAriaSnapshot is not supported by this element assertions implementation.");
    }

    private static UnsupportedOperationException unsupported(String operation) {
        return new UnsupportedOperationException(operation + " is not supported by this element assertions implementation.");
    }

}
