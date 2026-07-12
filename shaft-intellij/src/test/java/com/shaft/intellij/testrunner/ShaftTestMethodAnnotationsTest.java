package com.shaft.intellij.testrunner;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftTestMethodAnnotationsTest {
    @Test
    void recognizesTestNgAnnotation() {
        assertTrue(ShaftTestMethodAnnotations.isShaftRunnableTestMethod(
                List.of("org.testng.annotations.Test")));
    }

    @Test
    void recognizesJUnit4Annotation() {
        assertTrue(ShaftTestMethodAnnotations.isShaftRunnableTestMethod(
                List.of("org.junit.Test")));
    }

    @Test
    void recognizesJUnit5Annotation() {
        assertTrue(ShaftTestMethodAnnotations.isShaftRunnableTestMethod(
                List.of("org.junit.jupiter.api.Test")));
    }

    @Test
    void ignoresUnrelatedAnnotations() {
        assertFalse(ShaftTestMethodAnnotations.isShaftRunnableTestMethod(
                List.of("java.lang.Override", "javax.annotation.Nullable")));
    }

    @Test
    void returnsFalseForEmptyOrNullCollections() {
        assertFalse(ShaftTestMethodAnnotations.isShaftRunnableTestMethod(List.of()));
        assertFalse(ShaftTestMethodAnnotations.isShaftRunnableTestMethod(null));
    }

    @Test
    void recognizesTestAnnotationMixedWithOthers() {
        assertTrue(ShaftTestMethodAnnotations.isShaftRunnableTestMethod(
                Set.of("java.lang.Override", "org.junit.jupiter.api.Test")));
    }
}
