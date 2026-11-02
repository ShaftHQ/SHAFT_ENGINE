package com.shaft.infrastructure;

/** One mutation or read-only preparation operation in an exact setup plan. */
public enum SetupActionKind {
    DIAGNOSE,
    DOWNLOAD,
    INSTALL,
    PREWARM,
    CONFIGURE,
    START,
    CLEAN,
    ROLLBACK
}
