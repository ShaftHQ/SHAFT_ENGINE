package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ShaftLocatorChainTest {
    @Test
    void chainedBuilderResolvesToTheEngineLocator() {
        assertEquals(SHAFT.GUI.Locator.hasTagName("button").containsText("Save").hasAttribute("type", "submit").isFirst().build(),
                EngineService.getLocator(locatorStrategy.SHAFT_LOCATOR,
                        "[[\"hasTagName\",\"button\"],[\"containsText\",\"Save\"],[\"hasAttribute\",\"type\",\"submit\"],[\"isFirst\"]]"));
    }

    @Test
    void anyTagNameAndIndexAreSupported() {
        assertEquals(SHAFT.GUI.Locator.hasAnyTagName().hasId("q").hasIndex(2).build(),
                EngineService.getLocator(locatorStrategy.SHAFT_LOCATOR, "[[\"hasAnyTagName\"],[\"hasId\",\"q\"],[\"hasIndex\",\"2\"]]"));
    }

    @Test
    void chainRendersAsJavaSource() {
        assertEquals("SHAFT.GUI.Locator.hasTagName(\"a\").containsText(\"say \\\"hi\\\"\").hasIndex(2).build()",
                McpMobileCode.locatorCode(locatorStrategy.SHAFT_LOCATOR,
                        "[[\"hasTagName\",\"a\"],[\"containsText\",\"say \\\"hi\\\"\"],[\"hasIndex\",\"2\"]]"));
    }

    @Test
    void unknownStepsAndMalformedChainsAreRejected() {
        assertThrows(IllegalArgumentException.class,
                () -> EngineService.getLocator(locatorStrategy.SHAFT_LOCATOR, "[[\"hasTagName\",\"a\"],[\"byRelation\"]]"));
        assertThrows(IllegalArgumentException.class,
                () -> EngineService.getLocator(locatorStrategy.SHAFT_LOCATOR, "[[\"hasText\",\"a\"]]"));
        assertThrows(IllegalArgumentException.class,
                () -> EngineService.getLocator(locatorStrategy.SHAFT_LOCATOR, "not json"));
    }
}
