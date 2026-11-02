package com.shaft.intellij.testrunner;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
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

    @Test
    void hasRunnableTestMethodReturnsFalseForNullOuterCollection() {
        assertFalse(ShaftTestMethodAnnotations.hasRunnableTestMethod(null));
    }

    @Test
    void hasRunnableTestMethodReturnsFalseForEmptyOuterCollection() {
        assertFalse(ShaftTestMethodAnnotations.hasRunnableTestMethod(List.of()));
    }

    @Test
    void hasRunnableTestMethodReturnsFalseWhenNoEntryIsRunnable() {
        assertFalse(ShaftTestMethodAnnotations.hasRunnableTestMethod(
                List.of(List.of(), List.of("java.lang.Override"))));
    }

    @Test
    void hasRunnableTestMethodReturnsTrueForSingleTestNgEntry() {
        assertTrue(ShaftTestMethodAnnotations.hasRunnableTestMethod(
                List.of(List.of(ShaftTestMethodAnnotations.TESTNG_TEST))));
    }

    @Test
    void hasRunnableTestMethodReturnsTrueWhenOneOfMixedEntriesIsRunnable() {
        assertTrue(ShaftTestMethodAnnotations.hasRunnableTestMethod(
                List.of(List.of("java.lang.Override"), List.of(ShaftTestMethodAnnotations.JUNIT5_TEST))));
    }

    @Test
    void hasRunnableTestMethodReturnsTrueWhenANullEntryIsMixedWithARunnableEntry() {
        List<List<String>> methodsAnnotationQualifiedNames = new ArrayList<>(Arrays.asList(null, List.of(ShaftTestMethodAnnotations.TESTNG_TEST)));
        assertTrue(ShaftTestMethodAnnotations.hasRunnableTestMethod(methodsAnnotationQualifiedNames));
    }
}
