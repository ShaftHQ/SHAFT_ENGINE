package com.shaft.listeners.internal;

import java.lang.reflect.Method;

/**
 * Runner-neutral test execution metadata used by SHAFT listeners.
 *
 * @param stableId    stable runner identifier used for deduplicated counting
 * @param className   test class name
 * @param methodName  test method name
 * @param displayName runner display name
 * @param description runner or annotation description
 * @param method      reflected Java test method when available
 * @param throwable   execution throwable when the test did not pass
 * @param retried     whether this result belongs to a retried invocation
 */
public record TestExecutionInfo(String stableId, String className, String methodName, String displayName,
                                String description, Method method, Throwable throwable, boolean retried) {
}
