package com.shaft.infrastructure;

import java.net.URI;
import java.util.List;
import java.util.Set;

/** Release-coupled planner for the managed Lighthouse command-line runtime. */
public final class LighthouseSetupPlanner {
    public static final String LIGHTHOUSE_VERSION = "13.4.1";
    public static final String LIGHTHOUSE_LOCK_SHA256 =
            "5691359da63475578daef5b322a620311110ff6a09953cdb898bee314106c4dd";
    private static final String LIGHTHOUSE_SHA256 =
            "110759ba9e863c024e214e9b08ed2b0344d89b492286227235d4dfb990dc3e54";

    private LighthouseSetupPlanner() { }

    /**
     * Creates the exact Lighthouse plan shipped with this release.
     *
     * @param platform target operating system
     * @param architecture target CPU architecture
     * @param mode requested ownership mode
     * @return immutable release-pinned setup plan
     */
    public static SetupPlan plan(SetupPlatform platform, SetupArchitecture architecture, SetupMode mode) {
        SetupAction node = ReportingSetupPlanner.plan(platform, architecture, mode).actions().getFirst();
        SetupActionKind kind = mode == SetupMode.EXTERNAL ? SetupActionKind.DIAGNOSE : SetupActionKind.INSTALL;
        SetupAction lighthouse = new SetupAction(SetupTarget.LIGHTHOUSE, kind, LIGHTHOUSE_VERSION,
                URI.create("https://registry.npmjs.org/lighthouse/-/lighthouse-" + LIGHTHOUSE_VERSION + ".tgz"),
                "sha256:" + LIGHTHOUSE_SHA256, "sha256:" + LIGHTHOUSE_LOCK_SHA256, false, Set.of());
        return SetupPlan.create(SetupProfile.LIGHTHOUSE, platform, architecture, mode, List.of(node, lighthouse));
    }
}
