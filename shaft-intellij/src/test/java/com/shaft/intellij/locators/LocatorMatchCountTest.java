package com.shaft.intellij.locators;

import com.google.gson.JsonObject;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LocatorMatchCountTest {
    @Test
    void mapsByFactoriesToMcpLocatorArguments() {
        JsonObject arguments = LocatorMatchCount.arguments("cssSelector", "li.item");
        assertEquals("CSSSELECTOR", arguments.get("locatorStrategy").getAsString());
        assertEquals("li.item", arguments.get("locatorValue").getAsString());
        assertEquals("XPATH", LocatorMatchCount.arguments("xpath", "//a").get("locatorStrategy").getAsString());
        assertNull(LocatorMatchCount.arguments("linkText", "Home"));
    }

    @Test
    void builderChainsUseTheShaftLocatorStrategy() {
        JsonObject arguments = LocatorMatchCount.chainArguments(
                java.util.List.of(java.util.List.of("hasTagName", "a"), java.util.List.of("isFirst")));
        assertEquals("SHAFT_LOCATOR", arguments.get("locatorStrategy").getAsString());
        assertEquals("[[\"hasTagName\",\"a\"],[\"isFirst\"]]", arguments.get("locatorValue").getAsString());
    }

    @Test
    void liveSessionShowsMatchCount() {
        String wrapped = "{\"content\":[{\"type\":\"text\",\"text\":\"{\\\"activeEngine\\\":\\\"WEB\\\",\\\"count\\\":3}\"}]}";
        assertEquals("3 matches", LocatorMatchCount.message(new ShaftMcpToolResult(true, wrapped, null, null)));
        assertEquals("1 match", LocatorMatchCount.message(
                new ShaftMcpToolResult(true, "{\"activeEngine\":\"WEB\",\"count\":1}", null, null)));
        assertEquals("0 matches", LocatorMatchCount.message(
                new ShaftMcpToolResult(true, "{\"count\":0}", null, null)));
    }

    @Test
    void noSessionGuidesTheUserToStartOne() {
        assertTrue(LocatorMatchCount.message(new ShaftMcpToolResult(false, "no active driver", null, null))
                .startsWith(LocatorMatchCount.START_SESSION));
        assertTrue(LocatorMatchCount.message(null).startsWith(LocatorMatchCount.START_SESSION));
    }
}
