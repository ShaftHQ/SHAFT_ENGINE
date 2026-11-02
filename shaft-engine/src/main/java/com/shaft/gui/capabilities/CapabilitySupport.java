package com.shaft.gui.capabilities;

/**
 * Describes how an automation feature is supplied by the active backend.
 */
public enum CapabilitySupport {
    /** The active backend exposes the feature directly. */
    NATIVE,
    /** SHAFT provides equivalent semantics by adapting one or more backend operations. */
    ADAPTED,
    /** The active, live session cannot prove that the feature is available. */
    UNSUPPORTED
}
