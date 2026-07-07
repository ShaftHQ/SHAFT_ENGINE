package com.shaft.capture.runtime;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class CaptureManagerModeTest {

    @Test
    void modeDefaultsToRecord() {
        CaptureManager manager = new CaptureManager();

        assertEquals("record", manager.mode());

        manager.close();
    }

    @Test
    void setModeTogglesToInspectAndBack() {
        CaptureManager manager = new CaptureManager();

        assertEquals("inspect", manager.setMode("inspect"));
        assertEquals("inspect", manager.mode());
        assertEquals("record", manager.setMode("RECORD"));
        assertEquals("record", manager.mode());

        manager.close();
    }

    @Test
    void setModeRejectsUnsupportedValues() {
        CaptureManager manager = new CaptureManager();

        assertThrows(IllegalArgumentException.class, () -> manager.setMode("not-a-mode"));
        assertThrows(IllegalArgumentException.class, () -> manager.setMode(""));
        assertThrows(IllegalArgumentException.class, () -> manager.setMode(null));

        manager.close();
    }
}
