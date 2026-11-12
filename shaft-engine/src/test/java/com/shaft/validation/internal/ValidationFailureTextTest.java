package com.shaft.validation.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

public class ValidationFailureTextTest {
    @Test
    public void composeUsesAssertWordingWhenCategoryAndMessageAreBlank() {
        Assert.assertEquals(
                ValidationFailureText.compose(null, "a", "b", null),
                "Assertion failed; expected a, but found b");
        Assert.assertEquals(
                ValidationFailureText.compose("Verify", 1, 2, "  "),
                "Verification failed; expected 1, but found 2");
    }

    @Test
    public void composePrefixesACustomMessageUnlessItIsAlreadyPresent() {
        Assert.assertEquals(
                ValidationFailureText.compose("Assert", "a", "b", " custom "),
                "custom — Assertion failed; expected a, but found b");
        Assert.assertEquals(
                ValidationFailureText.compose("Assert", "a", "custom", "custom"),
                "Assertion failed; expected a, but found custom");
        Assert.assertEquals(
                ValidationFailureText.compose("Assert", "a", "b", "expected"),
                "Assertion failed; expected a, but found b");
        Assert.assertEquals(
                ValidationFailureText.compose("Assert", "a", null, "note"),
                "note — Assertion failed; expected a, but found null");
    }
}
