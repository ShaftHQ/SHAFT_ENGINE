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

    /**
     * Validates that the element exists.
     */
    ValidationsExecutor exists();

    /**
     * Validates that the element does not exist.
     */
    ValidationsExecutor doesNotExist();

    /**
     * Validates that the element matches its stored reference image.
     */
    ValidationsExecutor matchesReferenceImage();

    /**
     * Validates that the element matches its stored reference image using the given visual engine.
     */
    ValidationsExecutor matchesReferenceImage(ValidationEnums.VisualValidationEngine visualValidationEngine);

    /**
     * Validates that the element does not match its stored reference image.
     */
    ValidationsExecutor doesNotMatchReferenceImage();

    /**
     * Validates that the element does not match its stored reference image using the given visual
     * engine.
     */
    ValidationsExecutor doesNotMatchReferenceImage(ValidationEnums.VisualValidationEngine visualValidationEngine);

    /**
     * Validates the given element attribute.
     */
    NativeValidationsBuilder attribute(String attribute);

    /**
     * Validates the given DOM attribute.
     */
    NativeValidationsBuilder domAttribute(String domAttribute);

    /**
     * Validates the given DOM property.
     */
    NativeValidationsBuilder domProperty(String domProperty);

    /**
     * Validates the given element property.
     */
    NativeValidationsBuilder property(String domProperty);

    /**
     * Validates that the element is selected.
     */
    ValidationsExecutor isSelected();

    /**
     * Validates that the element is checked.
     */
    ValidationsExecutor isChecked();

    /**
     * Validates that the element is visible.
     */
    ValidationsExecutor isVisible();

    /**
     * Validates that the element is enabled.
     */
    ValidationsExecutor isEnabled();

    /**
     * Validates that the element is not selected.
     */
    ValidationsExecutor isNotSelected();

    /**
     * Validates that the element is not checked.
     */
    ValidationsExecutor isNotChecked();

    /**
     * Validates that the element is hidden.
     */
    ValidationsExecutor isHidden();

    /**
     * Validates that the element is disabled.
     */
    ValidationsExecutor isDisabled();

    /**
     * Validates the element text.
     */
    NativeValidationsBuilder text();

    /**
     * Validates the element text with surrounding whitespace removed.
     */
    NativeValidationsBuilder textTrimmed();

    /** Recognizes the target element screenshot and starts a native string assertion. */
    default NativeValidationsBuilder ocrText() {
        return ocrText(OcrOptions.defaults());
    }

    /** Recognizes the target element screenshot with explicit OCR options. */
    default NativeValidationsBuilder ocrText(OcrOptions options) {
        throw unsupported("ocrText");
    }

    /**
     * Validates the given CSS property of the element.
     */
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
