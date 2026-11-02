package com.shaft.gui.internal.natural;

/**
 * Engine-supported natural-action operations.
 */
public enum NaturalActionKind {
    ELEMENT_CLICK(Target.ELEMENT),
    ELEMENT_TYPE(Target.ELEMENT),
    ELEMENT_TYPE_SECURELY(Target.ELEMENT),
    ELEMENT_CLEAR(Target.ELEMENT),
    TOUCH_TAP(Target.TOUCH),
    TOUCH_DOUBLE_TAP(Target.TOUCH),
    TOUCH_LONG_TAP(Target.TOUCH),
    BROWSER_NAVIGATE(Target.BROWSER),
    BROWSER_REFRESH(Target.BROWSER),
    BROWSER_BACK(Target.BROWSER),
    BROWSER_FORWARD(Target.BROWSER);

    private final Target target;

    NaturalActionKind(Target target) {
        this.target = target;
    }

    /**
     * @return action target category
     */
    public Target target() {
        return target;
    }

    /**
     * Natural-action target categories.
     */
    public enum Target {
        BROWSER,
        ELEMENT,
        TOUCH
    }
}
