package com.shaft.capture.generate;

import org.junit.jupiter.api.Test;

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;

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
}
