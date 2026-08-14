package com.shaft.infrastructure;

/** Independent operations or constraints exposed by a setup target. */
public enum SetupCapability {
    INSTALLABLE,
    PREWARMABLE,
    STARTABLE,
    CLEANABLE,
    ROLLBACKABLE,
    HOST_PREREQUISITE,
    PROVIDED,
    PRIVILEGED
}
