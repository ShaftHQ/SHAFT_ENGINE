package com.shaft.integration.browserstack;

import org.junit.jupiter.api.Test;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class BrowserStackModuleTest {
    @Test
    void constructorRejectsInstantiation() throws Exception {
        Constructor<BrowserStackModule> constructor = BrowserStackModule.class.getDeclaredConstructor();
        constructor.setAccessible(true);

        InvocationTargetException thrown = assertThrows(InvocationTargetException.class, constructor::newInstance);

        assertEquals(IllegalStateException.class, thrown.getCause().getClass());
        assertEquals("Utility class", thrown.getCause().getMessage());
    }
}
