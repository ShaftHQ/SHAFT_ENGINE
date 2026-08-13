package com.shaft.infrastructure;

import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Set;

/** Release-coupled planner for the reporting profile's portable Node and Allure tools. */
public final class ReportingSetupPlanner {
    public static final String NODE_VERSION = "24.19.0";
    public static final String ALLURE_VERSION = "3.14.3";
    private static final String ALLURE_SHA256 = "6388ce188104d0b58236598a2e8ff61882066fcf4d3b7c9e9e680a75344e2264";
    public static final String ALLURE_LOCK_SHA256 = "385f9d727c62be5e5628a4826fc089cb99a8d9378f226cdcf781b4ac563abff1";
    private static final Map<String, String> NODE_SHA256 = Map.of(
            "darwin-arm64.tar.gz", "8294b7aa9b03997481c06babf1e8b270c859358f27da57a11509afe537ac381d",
            "darwin-x64.tar.gz", "d1b5e999db158c62fe8f7267a4476b035d8bd93b1a605bac24a3f0dd166e3316",
            "linux-arm64.tar.gz", "d28c8a5bf0a808f0ed434a1dce8c54ae98f0371c0bd86ac58abc613f73e6643f",
            "linux-x64.tar.gz", "f625d97cd707df4ff96254916fbc5ff014f09c09effe5a1e0ca8f6d41a8789d4",
            "win-arm64.zip", "8502f4a50b458d4cc38ed8f2001556c2cd239d464920f74017926ccb1e1c157f",
            "win-x64.zip", "57f71ab3652e797d84acddc79c81cc9ff1c6ddb2a1974cdb83f00fee9bff4c73");

    private ReportingSetupPlanner() { }

    public static SetupPlan plan(SetupPlatform platform, SetupArchitecture architecture, SetupMode mode) {
        String platformToken = switch (platform) {
            case WINDOWS -> "win";
            case MACOS -> "darwin";
            case LINUX -> "linux";
        };
        String extension = platform == SetupPlatform.WINDOWS ? "zip"
                : "tar.gz";
        String classifier = platformToken + '-' + architecture.artifactName() + '.' + extension;
        String nodeFile = "node-v" + NODE_VERSION + '-' + classifier;
        String nodeHash = NODE_SHA256.get(classifier);
        if (nodeHash == null) throw new IllegalArgumentException("Unsupported reporting platform: " + classifier);
        SetupActionKind kind = mode == SetupMode.EXTERNAL ? SetupActionKind.DIAGNOSE : SetupActionKind.INSTALL;
        SetupAction node = new SetupAction(SetupTarget.NODE, kind, NODE_VERSION,
                URI.create("https://nodejs.org/dist/v" + NODE_VERSION + '/' + nodeFile),
                "sha256:" + nodeHash, false, Set.of());
        SetupAction allure = new SetupAction(SetupTarget.ALLURE, kind, ALLURE_VERSION,
                URI.create("https://registry.npmjs.org/allure/-/allure-" + ALLURE_VERSION + ".tgz"),
                "sha256:" + ALLURE_SHA256, "sha256:" + ALLURE_LOCK_SHA256, false, Set.of());
        return SetupPlan.create(SetupProfile.REPORTING, platform, architecture, mode, List.of(node, allure));
    }
}
