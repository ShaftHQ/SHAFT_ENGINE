package com.shaft.infrastructure;

import java.net.URI;
import java.util.List;
import java.util.Set;

/** Release-coupled planner for the managed Lighthouse command-line runtime. */
public final class LighthouseSetupPlanner {
    public static final String LIGHTHOUSE_VERSION = "13.5.0";
    public static final String LIGHTHOUSE_LOCK_SHA256 =
            "f82003256f3e7c7167a8ac0b1727938d9485cabd7bf6c168dfaad5a69a36946b";
    static final String LIGHTHOUSE_SHA256 =
            "4be86876145663e10e8bb3178ebe27b2613a1debd4c04b4e0ec92a0487a4e6e2";

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
