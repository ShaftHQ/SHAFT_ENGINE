package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

public class JsonEscapesTest {
    @Test
    public void escapeCoversNullQuotesControlsAndOrdinaryText() {
        Assert.assertEquals(JsonEscapes.escape(null), "");
        Assert.assertEquals(JsonEscapes.escape("a\"b"), "a\\\"b");
        Assert.assertEquals(JsonEscapes.escape("a\\b"), "a\\\\b");
        Assert.assertEquals(JsonEscapes.escape("a\nb"), "a\\nb");
        Assert.assertEquals(JsonEscapes.escape("a\rb"), "a\\rb");
        Assert.assertEquals(JsonEscapes.escape("a\tb"), "a\\tb");
        Assert.assertEquals(JsonEscapes.escape("a\u0001b"), "a\\u0001b");
        Assert.assertEquals(JsonEscapes.escape("ok"), "ok");
    }
}
