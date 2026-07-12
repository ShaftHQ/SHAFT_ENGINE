package com.shaft.gui.internal.aria;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class MobileAccessibilityTreeConverterUnitTest {

    private static final String ANDROID_XML = """
            <hierarchy>
              <android.widget.FrameLayout class="android.widget.FrameLayout" bounds="[0,0][1080,2160]">
                <android.widget.LinearLayout class="android.widget.LinearLayout" bounds="[0,0][1080,200]">
                  <android.widget.TextView class="android.widget.TextView" text="Welcome" bounds="[20,20][500,80]"/>
                  <android.widget.Button class="android.widget.Button" content-desc="Sign in" resource-id="com.example:id/signin" bounds="[600,20][1000,80]"/>
                </android.widget.LinearLayout>
                <android.widget.ViewGroup class="android.widget.ViewGroup" bounds="[0,200][1080,2160]">
                  <android.widget.EditText class="android.widget.EditText" text="" resource-id="com.example:id/username" bounds="[20,220][1000,280]"/>
                </android.widget.ViewGroup>
              </android.widget.FrameLayout>
            </hierarchy>
            """;

    private static final String IOS_XML = """
            <XCUIElementTypeOther type="XCUIElementTypeOther">
              <XCUIElementTypeStaticText type="XCUIElementTypeStaticText" name="Welcome" label="Welcome" value="Welcome"/>
              <XCUIElementTypeButton type="XCUIElementTypeButton" name="Sign in" label="Sign in"/>
              <XCUIElementTypeOther type="XCUIElementTypeOther">
                <XCUIElementTypeTextField type="XCUIElementTypeTextField" name="username" value=""/>
              </XCUIElementTypeOther>
            </XCUIElementTypeOther>
            """;

    @Test(description = "convert should skip the synthetic Android hierarchy wrapper and hoist nameless generic containers, keeping named/leaf nodes")
    public void convertShouldSkipHierarchyWrapperAndHoistNamelessContainers() {
        List<AriaNode> forest = MobileAccessibilityTreeConverter.convert(ANDROID_XML);

        // FrameLayout, LinearLayout, and ViewGroup are all nameless generic containers: hoisted away,
        // leaving their named/leaf descendants at the top level.
        Assert.assertEquals(forest, List.of(
                new AriaNode("textview", "Welcome", List.of()),
                new AriaNode("button", "Sign in", List.of()),
                new AriaNode("edittext", "", List.of())
        ));
    }

    @Test(description = "convert should derive iOS roles by stripping the XCUIElementType prefix and hoist nameless XCUIElementTypeOther containers")
    public void convertShouldDeriveIosRolesAndHoistNamelessOtherContainers() {
        List<AriaNode> forest = MobileAccessibilityTreeConverter.convert(IOS_XML);

        Assert.assertEquals(forest, List.of(
                new AriaNode("statictext", "Welcome", List.of()),
                new AriaNode("button", "Sign in", List.of()),
                new AriaNode("textfield", "username", List.of())
        ));
    }

    @Test(description = "convert should preserve real nesting for named containers instead of hoisting them")
    public void convertShouldPreserveNestingForNamedContainers() {
        String xml = """
                <hierarchy>
                  <android.widget.LinearLayout class="android.widget.LinearLayout" content-desc="Toolbar" bounds="[0,0][1080,200]">
                    <android.widget.Button class="android.widget.Button" content-desc="Back" bounds="[0,0][100,200]"/>
                  </android.widget.LinearLayout>
                </hierarchy>
                """;

        List<AriaNode> forest = MobileAccessibilityTreeConverter.convert(xml);

        Assert.assertEquals(forest, List.of(
                new AriaNode("linearlayout", "Toolbar", List.of(
                        new AriaNode("button", "Back", List.of())
                ))
        ));
    }

    @Test(description = "convert should drop a nameless generic container that has no surviving children")
    public void convertShouldDropEmptyNamelessGenericContainer() {
        String xml = """
                <hierarchy>
                  <android.widget.FrameLayout class="android.widget.FrameLayout" bounds="[0,0][1080,2160]"/>
                </hierarchy>
                """;

        List<AriaNode> forest = MobileAccessibilityTreeConverter.convert(xml);

        Assert.assertTrue(forest.isEmpty());
    }

    @Test(description = "convert should fall back to the tag name when the class attribute is absent")
    public void convertShouldFallBackToTagNameForRole() {
        String xml = """
                <hierarchy>
                  <node text="Save" bounds="[0,0][100,50]"/>
                </hierarchy>
                """;

        List<AriaNode> forest = MobileAccessibilityTreeConverter.convert(xml);

        Assert.assertEquals(forest, List.of(new AriaNode("node", "Save", List.of())));
    }

    @Test(description = "serialized converted forest should match SHAFT's aria snapshot YAML shape")
    public void convertedForestShouldSerializeToExpectedYamlShape() {
        String yaml = AriaSnapshotHelper.serialize(MobileAccessibilityTreeConverter.convert(ANDROID_XML));

        Assert.assertEquals(yaml, """
                - textview "Welcome"
                - button "Sign in"
                - edittext
                """);
    }

    @Test(description = "a baseline captured from the Android tree should match-subset an actual snapshot from the same tree")
    public void convertedAndroidForestShouldRoundTripThroughMatch() {
        String baseline = AriaSnapshotHelper.serialize(MobileAccessibilityTreeConverter.convert(ANDROID_XML));
        String actual = AriaSnapshotHelper.serialize(MobileAccessibilityTreeConverter.convert(ANDROID_XML));

        var result = AriaSnapshotHelper.match(baseline, actual);

        Assert.assertTrue(result.matched());
    }

    @Test(description = "a baseline captured from the iOS tree should match-subset an actual snapshot from the same tree")
    public void convertedIosForestShouldRoundTripThroughMatch() {
        String baseline = AriaSnapshotHelper.serialize(MobileAccessibilityTreeConverter.convert(IOS_XML));
        String actual = AriaSnapshotHelper.serialize(MobileAccessibilityTreeConverter.convert(IOS_XML));

        var result = AriaSnapshotHelper.match(baseline, actual);

        Assert.assertTrue(result.matched());
    }

    @Test(description = "convert should reject blank input")
    public void convertShouldRejectBlankInput() {
        Assert.assertThrows(IllegalArgumentException.class, () -> MobileAccessibilityTreeConverter.convert(""));
        Assert.assertThrows(IllegalArgumentException.class, () -> MobileAccessibilityTreeConverter.convert(null));
    }

    @Test(description = "convert should throw a clear IllegalArgumentException for malformed XML")
    public void convertShouldThrowForMalformedXml() {
        IllegalArgumentException exception = Assert.expectThrows(IllegalArgumentException.class,
                () -> MobileAccessibilityTreeConverter.convert("<hierarchy><node bounds=\"[0,0][10,10]\"></hierarchy>"));

        Assert.assertTrue(exception.getMessage().contains("Failed to parse Appium accessibility XML"));
    }
}
