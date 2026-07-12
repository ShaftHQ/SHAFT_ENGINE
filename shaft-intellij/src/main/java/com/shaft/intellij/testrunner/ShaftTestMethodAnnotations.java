package com.shaft.intellij.testrunner;

import java.util.Collection;
import java.util.Set;

/**
 * Pure predicate over annotation fully-qualified names deciding whether a Java method is a
 * runnable SHAFT test entry point. Kept free of PSI/platform types so it is directly unit
 * testable; {@link ShaftTestLineMarkerContributor} and the run-configuration producers extract
 * plain annotation FQNs from PSI and hand them here.
 */
public final class ShaftTestMethodAnnotations {
    public static final String TESTNG_TEST = "org.testng.annotations.Test";
    public static final String JUNIT4_TEST = "org.junit.Test";
    public static final String JUNIT5_TEST = "org.junit.jupiter.api.Test";

    private static final Set<String> RUNNABLE_TEST_ANNOTATION_FQNS = Set.of(TESTNG_TEST, JUNIT4_TEST, JUNIT5_TEST);

    private ShaftTestMethodAnnotations() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns whether a method carrying the given annotation fully-qualified names is a SHAFT
     * runnable test method (TestNG {@code @Test}, JUnit 4 {@code @Test}, or JUnit 5 {@code @Test}).
     *
     * @param annotationQualifiedNames qualified names of the annotations present on the method, or
     *                                 {@code null}
     * @return {@code true} when at least one recognized test annotation is present
     */
    public static boolean isShaftRunnableTestMethod(Collection<String> annotationQualifiedNames) {
        if (annotationQualifiedNames == null || annotationQualifiedNames.isEmpty()) {
            return false;
        }
        return annotationQualifiedNames.stream().anyMatch(RUNNABLE_TEST_ANNOTATION_FQNS::contains);
    }
}
