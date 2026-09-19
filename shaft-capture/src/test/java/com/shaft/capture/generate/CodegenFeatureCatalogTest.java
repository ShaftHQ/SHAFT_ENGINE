package com.shaft.capture.generate;

import org.junit.jupiter.api.Test;

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CodegenFeatureCatalogTest {
    @Test
    void catalogCoversCaptureBacklogTrackingFeatures() {
        Set<String> actualNames = CodegenFeatureCatalog.features().stream()
                .map(CodegenFeatureCatalog.Feature::name)
                .collect(Collectors.toSet());
        Set<String> missing = new LinkedHashSet<>(Set.of(
                "Live readiness scoring",
                "Fallback locator replay",
                "Control-flow suggestions",
                "In-panel assertions and checkpoints",
                "Step inspector with reorder controls",
                "Session goal metadata",
                "Generated review header",
                "MCP Page Object draft",
                "Workbench review sections",
                "MCP Playwright recording schema convergence"));
        missing.removeAll(actualNames);

        assertTrue(missing.isEmpty(), "Missing #3116 catalog features: " + missing);
    }

    /**
     * Issue #5963: catalog locks Java-only fluent codegen; --target is metadata; no multi-language exporters.
     */
    @Test
    void catalogLocksJavaOnlyFluentCodegenPolicy() {
        var byName = CodegenFeatureCatalog.features().stream()
                .collect(Collectors.toMap(CodegenFeatureCatalog.Feature::name, feature -> feature));

        CodegenFeatureCatalog.Feature targetLanguage = byName.get("Target language");
        assertTrue(targetLanguage != null, "Target language feature missing");
        assertTrue(targetLanguage.notes().contains("fluent Java"), targetLanguage.notes());
        assertTrue(targetLanguage.notes().contains("Metadata only")
                        || targetLanguage.notes().contains("metadata"),
                targetLanguage.notes());
        assertTrue(targetLanguage.notes().toLowerCase().contains("python")
                        || targetLanguage.notes().contains("No Python"),
                targetLanguage.notes());

        CodegenFeatureCatalog.Feature javaOnly = byName.get("Java-only fluent codegen");
        assertTrue(javaOnly != null, "Java-only fluent codegen feature missing");
        assertEquals("SUPPORTED", javaOnly.shaftSupport());
        assertTrue(javaOnly.notes().contains("SHAFT."), javaOnly.notes());
        assertTrue(javaOnly.notes().contains("@playwright/test"), javaOnly.notes());
        assertTrue(javaOnly.notes().contains("insertion"), javaOnly.notes());
    }
}
