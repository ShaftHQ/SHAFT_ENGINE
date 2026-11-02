package com.shaft.infrastructure;

import java.net.URI;
import java.util.List;
import java.util.Set;

/** Release-coupled planner for the managed Lighthouse command-line runtime. */
public final class LighthouseSetupPlanner {
    public static final String LIGHTHOUSE_VERSION = "13.6.0";
    public static final String LIGHTHOUSE_LOCK_SHA256 =
            "3247e967d8894de14d3e7e2357e2a1f726b5d4e759529026f73965a01d586aa4";
    static final String LIGHTHOUSE_SHA256 =
            "214d7c83d25de829b0f0b6dad48c9fe8d994e1e76a51788a9a1d6911bc45ddaa";

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
