package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

class MapResultTest {
    @Test
    void renderedConsumedAndUnknownSeams() {
        MapResult rendered = MapResult.rendered("line");
        assertInstanceOf(MapResult.Rendered.class, rendered);
        assertEquals("line", ((MapResult.Rendered) rendered).text());
        assertThrows(NullPointerException.class, () -> MapResult.rendered(null));

        assertSame(MapResult.CONSUMED, MapResult.CONSUMED);
        assertInstanceOf(MapResult.Consumed.class, MapResult.CONSUMED);
        assertSame(MapResult.UNKNOWN, MapResult.UNKNOWN);
        assertInstanceOf(MapResult.Unknown.class, MapResult.UNKNOWN);
    }
}
