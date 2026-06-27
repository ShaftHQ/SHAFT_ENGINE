package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class McpAppiumLocatorSuggesterTest {

    @Test
    void prefersAccessibilityIdFromContentDescription() {
        McpAppiumLocatorSuggester.LocatorSuggestion suggestion = suggester("""
                <hierarchy>
                  <android.widget.Button content-desc="login" resource-id="com.example:id/login" bounds="[10,20][110,70]"/>
                </hierarchy>
                """).locatorAt(50, 40).orElseThrow();

        assertEquals(locatorStrategy.ACCESSIBILITY_ID, suggestion.strategy());
        assertEquals("login", suggestion.value());
    }

    @Test
    void fallsBackToAndroidUiAutomatorBeforeXPath() {
        McpAppiumLocatorSuggester.LocatorSuggestion suggestion = suggester("""
                <hierarchy>
                  <android.widget.TextView text="Save" class="android.widget.TextView" bounds="[0,0][100,50]"/>
                  <android.widget.TextView text="Save" class="android.widget.TextView" bounds="[0,60][100,110]"/>
                </hierarchy>
                """).locatorAt(20, 20).orElseThrow();

        assertEquals(locatorStrategy.ANDROID_UIAUTOMATOR, suggestion.strategy());
        assertEquals("new UiSelector().text(\"Save\").instance(0)", suggestion.value());
    }

    @Test
    void treatsClassLikeTagNamesAsClassNameCandidates() {
        McpAppiumLocatorSuggester.LocatorSuggestion suggestion = suggester("""
                <hierarchy>
                  <android.widget.Button bounds="[0,0][100,50]"/>
                  <android.widget.TextView bounds="[0,60][100,110]"/>
                </hierarchy>
                """).locatorAt(20, 20).orElseThrow();

        assertEquals(locatorStrategy.CLASSNAME, suggestion.strategy());
        assertEquals("android.widget.Button", suggestion.value());
    }

    @Test
    void uiautomatorInstanceCountsAllElementsMatchingTheEmittedSelector() {
        McpAppiumLocatorSuggester.LocatorSuggestion suggestion = suggester("""
                <hierarchy>
                  <node text="Save" bounds="[0,0][100,50]"/>
                  <other text="Save" bounds="[0,60][100,110]"/>
                </hierarchy>
                """).locatorAt(20, 20).orElseThrow();

        assertEquals(locatorStrategy.ANDROID_UIAUTOMATOR, suggestion.strategy());
        assertEquals("new UiSelector().text(\"Save\").instance(0)", suggestion.value());
    }

    @Test
    void usesHierarchicalXPathOnlyAfterNativeLocatorOptionsFail() {
        McpAppiumLocatorSuggester.LocatorSuggestion suggestion = suggester("""
                <hierarchy>
                  <node bounds="[0,0][100,50]"/>
                  <node bounds="[0,60][100,110]"/>
                </hierarchy>
                """).locatorAt(20, 20).orElseThrow();

        assertEquals(locatorStrategy.XPATH, suggestion.strategy());
        assertEquals("/hierarchy/node[1]", suggestion.value());
    }

    private static McpAppiumLocatorSuggester suggester(String sourceXml) {
        return McpAppiumLocatorSuggester.parse(sourceXml).orElseThrow();
    }
}
