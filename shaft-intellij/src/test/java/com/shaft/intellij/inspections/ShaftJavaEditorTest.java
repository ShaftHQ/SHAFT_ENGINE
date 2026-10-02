package com.shaft.intellij.inspections;

import com.intellij.codeInsight.template.impl.TemplateManagerImpl;
import com.intellij.openapi.actionSystem.IdeActions;
import com.intellij.openapi.vfs.newvfs.impl.VfsRootAccess;
import com.intellij.testFramework.fixtures.LightJavaCodeInsightFixtureTestCase;

/** Editor fixture coverage for SHAFT Java inspections, API docs links and live templates (#6418-#6420). */
public class ShaftJavaEditorTest extends LightJavaCodeInsightFixtureTestCase {
    @Override
    protected void setUp() throws Exception {
        super.setUp();
        // Resolving JDK members can touch the plugin sandbox's own jars during first indexing.
        VfsRootAccess.allowRootAccess(getTestRootDisposable(), new java.io.File(".intellijPlatform").getAbsolutePath());
        // The light fixture ships no JDK here; stub the one JDK member the inspection resolves.
        myFixture.addClass("package java.lang; public class Thread { public static void sleep(long millis) {} }");
        myFixture.addClass("package org.testng.annotations; public @interface Test {}");
        myFixture.addClass("package org.openqa.selenium; public class By {}");
        myFixture.addClass("""
                package com.shaft.driver;
                public class SHAFT {
                    public static class GUI {
                        public static class WebDriver {
                            public Element element() { return new Element(); }
                        }
                        public static class Element {
                            /** @deprecated use {@link Element#click(org.openqa.selenium.By)} */
                            @Deprecated public Element clickOn(org.openqa.selenium.By by) { return this; }
                            /** @deprecated no replacement */
                            @Deprecated public Element legacy() { return this; }
                            public Element click(org.openqa.selenium.By by) { return this; }
                        }
                    }
                }
                """);
    }

    public void testThreadSleepInAShaftTestIsFlagged() {
        myFixture.enableInspections(new ShaftThreadSleepInspection());
        myFixture.configureByText("LoginTest.java", """
                import com.shaft.driver.SHAFT;
                class LoginTest {
                    @org.testng.annotations.Test void login() throws Exception { Thread.sleep(500); }
                }
                """);
        assertTrue(myFixture.doHighlighting().stream()
                .anyMatch(info -> ShaftThreadSleepInspection.MESSAGE.equals(info.getDescription())));
    }

    public void testThreadSleepOutsideShaftFilesIsIgnored() {
        myFixture.enableInspections(new ShaftThreadSleepInspection());
        myFixture.configureByText("PlainTest.java", """
                class PlainTest { @org.testng.annotations.Test void t() throws Exception { Thread.sleep(5); } }
                """);
        assertTrue(myFixture.doHighlighting().stream()
                .noneMatch(info -> ShaftThreadSleepInspection.MESSAGE.equals(info.getDescription())));
    }

    public void testDeprecatedShaftCallIsMigratedToTheLinkedReplacement() {
        myFixture.enableInspections(new ShaftDeprecatedApiInspection());
        myFixture.configureByText("FlowTest.java", """
                import com.shaft.driver.SHAFT;
                import org.openqa.selenium.By;
                class FlowTest {
                    void run(SHAFT.GUI.WebDriver driver, By button) { driver.element().click<caret>On(button).legacy(); }
                }
                """);
        assertTrue(myFixture.doHighlighting().stream()
                .anyMatch(info -> "Deprecated SHAFT API 'legacy'".equals(info.getDescription())));
        myFixture.launchAction(myFixture.findSingleIntention("Replace with 'click'"));
        assertTrue(myFixture.getFile().getText().contains("driver.element().click(button).legacy();"));
    }

    public void testShaftApiElementsLinkToTheUserGuide() {
        var shaft = myFixture.findClass("com.shaft.driver.SHAFT.GUI.Element");
        assertEquals("Element.click", ShaftApiDocumentationProvider.query(shaft.findMethodsByName("click", false)[0]));
        assertEquals(java.util.List.of(ShaftApiDocumentationProvider.SEARCH_URL + "Element"),
                new ShaftApiDocumentationProvider().getUrlFor(shaft, null));
        assertNull(ShaftApiDocumentationProvider.query(myFixture.findClass("org.openqa.selenium.By")));
    }

    public void testLiveTemplatesExpandToShaftCalls() {
        TemplateManagerImpl.setTemplateTesting(getTestRootDisposable());
        assertExpands("shclick", "driver.element().click(button);");
        assertExpands("shtype", "driver.element().type(button, \"\");");
        assertExpands("shassert", "driver.assertThat().element(button).text().isEqualTo(\"\").perform();");
        assertExpands("shapi", "api.get(\"/\").setTargetStatusCode(200).perform();");
        myFixture.configureByText("T.java", "class T {\n    <caret>\n}");
        myFixture.type("shtest");
        myFixture.performEditorAction(IdeActions.ACTION_EXPAND_LIVE_TEMPLATE_BY_TAB);
        assertTrue(myFixture.getFile().getText(), myFixture.getFile().getText().contains("public void shouldDoSomething()"));
    }

    private void assertExpands(String abbreviation, String expected) {
        myFixture.configureByText("T.java", """
                import com.shaft.driver.SHAFT;
                import org.openqa.selenium.By;
                class T {
                    void m(SHAFT.GUI.WebDriver driver, By button) {
                        <caret>
                    }
                }
                """);
        myFixture.type(abbreviation);
        myFixture.performEditorAction(IdeActions.ACTION_EXPAND_LIVE_TEMPLATE_BY_TAB);
        assertTrue(myFixture.getFile().getText(), myFixture.getFile().getText().contains(expected));
    }
}
