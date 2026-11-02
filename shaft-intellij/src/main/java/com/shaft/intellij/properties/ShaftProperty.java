package com.shaft.intellij.properties;

/**
 * One SHAFT Engine property, as declared by an {@code @Key} method in
 * {@code shaft-engine/src/main/java/com/shaft/properties/internal}.
 *
 * @param key          property key, e.g. {@code browserNavigationTimeout}
 * @param type         declared Java return type, e.g. {@code int} or {@code Boolean}
 * @param defaultValue {@code @DefaultValue}, empty when none is declared
 * @param description  Javadoc summary, empty when none is declared
 * @param group        declaring interface, e.g. {@code Timeouts}
 */
public record ShaftProperty(String key, String type, String defaultValue, String description, String group) {
}
