package com.shaft.intellij.properties;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.properties.psi.PropertiesFile;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;

import java.util.List;

/** Editor fixture coverage for SHAFT property completion, validation and docs (#6417, #6418). */
public class ShaftPropertiesEditorTest extends BasePlatformTestCase {
    public void testCompletionListsShaftKeysWithDefaults() {
        myFixture.configureByText("custom.properties", "waitForLazyLoading=true\nbrowserNav<caret>\n");
        myFixture.completeBasic();
        List<String> lookups = myFixture.getLookupElementStrings();
        if (lookups == null) {
            assertTrue(myFixture.getEditor().getDocument().getText().contains("browserNavigationTimeout"));
        } else {
            assertContainsElements(lookups, "browserNavigationTimeout");
        }
    }

    public void testTypoGetsAWarningAndADidYouMeanFix() {
        myFixture.enableInspections(new ShaftPropertyInspection());
        myFixture.configureByText("custom.properties", "waitForLazyLoading=true\nbrowserNavig<caret>atonTimeout=45\nmyProjectUrl=x\n");
        List<HighlightInfo> warnings = myFixture.doHighlighting().stream()
                .filter(info -> info.getDescription() != null && info.getDescription().contains("SHAFT")).toList();
        assertEquals(1, warnings.size());
        assertEquals("Unknown SHAFT property 'browserNavigatonTimeout'. Did you mean 'browserNavigationTimeout'?",
                warnings.get(0).getDescription());
        myFixture.launchAction(myFixture.findSingleIntention("Change to 'browserNavigationTimeout'"));
        myFixture.checkResult("waitForLazyLoading=true\nbrowserNavigationTimeout=45\nmyProjectUrl=x\n");
    }

    public void testInvalidBooleanValueIsFlagged() {
        myFixture.enableInspections(new ShaftPropertyInspection());
        myFixture.configureByText("custom.properties", "waitForLazyLoading=yes\n");
        assertTrue(myFixture.doHighlighting().stream()
                .anyMatch(info -> "Expected true or false for 'waitForLazyLoading'".equals(info.getDescription())));
    }

    public void testFilesWithoutShaftKeysOutsideShaftProjectsAreLeftAlone() {
        myFixture.enableInspections(new ShaftPropertyInspection());
        myFixture.configureByText("app.properties", "browserNavigatonTimeout=45\n");
        assertTrue(myFixture.doHighlighting().stream()
                .noneMatch(info -> info.getDescription() != null && info.getDescription().contains("SHAFT")));
    }

    public void testQuickDocumentationShowsDescriptionAndDefault() {
        PropertiesFile file = (PropertiesFile) myFixture.configureByText("custom.properties", "browserNavigationTimeout=45\n");
        String doc = new ShaftPropertyDocumentationProvider()
                .generateDoc(file.getProperties().get(0).getPsiElement(), null);
        assertNotNull(doc);
        assertTrue(doc, doc.contains("Timeout in seconds for browser navigation"));
        assertTrue(doc, doc.contains("<code>30</code>"));
        assertTrue(doc, doc.contains(ShaftPropertyCatalog.USER_GUIDE_URL));
    }
}
