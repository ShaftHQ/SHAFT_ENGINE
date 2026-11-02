package com.shaft.intellij.java;

/**
 * Java source location selected by an IntelliJ action.
 *
 * @param sourcePath source file path
 * @param packageName Java package
 * @param className nearest Java class
 * @param methodName nearest Java method or class name
 */
public record JavaTargetContext(
        String sourcePath,
        String packageName,
        String className,
        String methodName) {
    /**
     * Returns a compact display name for notifications.
     *
     * @return package-qualified class and method
     */
    public String displayName() {
        String owner = packageName == null || packageName.isBlank() ? className : packageName + "." + className;
        return owner + "#" + methodName;
    }
}
