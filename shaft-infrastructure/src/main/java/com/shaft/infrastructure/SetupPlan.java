package com.shaft.infrastructure;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.List;
import java.util.Objects;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/** Immutable, content-addressed setup plan. Action order is significant. */
public record SetupPlan(int schemaVersion, SetupProfile profile, SetupPlatform platform,
                        SetupArchitecture architecture, SetupMode mode,
                        List<SetupAction> actions, String executionPolicyDigest, String digest) {
    public SetupPlan {
        if (schemaVersion != 2 && schemaVersion != 3 && schemaVersion != 4) {
            throw new IllegalArgumentException("Unsupported plan schema version: " + schemaVersion);
        }
        Objects.requireNonNull(profile, "profile");
        Objects.requireNonNull(platform, "platform");
        Objects.requireNonNull(architecture, "architecture");
        Objects.requireNonNull(mode, "mode");
        actions = List.copyOf(Objects.requireNonNull(actions, "actions"));
        if (actions.isEmpty()) throw new IllegalArgumentException("Plan must contain at least one action.");
        if (schemaVersion == 2) executionPolicyDigest = "";
        else if (schemaVersion == 3 || executionPolicyDigest == null || !executionPolicyDigest.isBlank()) {
            requireSha256(executionPolicyDigest, "executionPolicyDigest");
        }
        if (schemaVersion < 4 && actions.stream().anyMatch(action -> action.artifactBytes() != 0)) {
            throw new IllegalArgumentException("Setup plan schemas 2 and 3 cannot bind artifact bytes.");
        }
        validatePolicy(mode, actions);
        if (digest == null || digest.isBlank()) throw new IllegalArgumentException("Plan digest must not be blank.");
        String expected = calculateDigest(schemaVersion, profile, platform, architecture, mode, actions,
                executionPolicyDigest);
        if (!expected.equals(digest)) throw new IllegalArgumentException("Plan digest does not match its content.");
    }

    public static SetupPlan create(SetupProfile profile, SetupPlatform platform, SetupArchitecture architecture,
                                   SetupMode mode, List<SetupAction> actions) {
        List<SetupAction> immutable = List.copyOf(actions);
        int schemaVersion = immutable.stream().anyMatch(action -> action.artifactBytes() != 0) ? 4 : 2;
        return new SetupPlan(schemaVersion, profile, platform, architecture, mode, immutable,
                "", calculateDigest(schemaVersion, profile, platform, architecture, mode, immutable, ""));
    }

    /** Binds an existing release plan to caller execution policy and destination roots. */
    public static SetupPlan bind(SetupPlan plan, String executionPolicyDigest) {
        Objects.requireNonNull(plan, "plan");
        requireSha256(executionPolicyDigest, "executionPolicyDigest");
        return new SetupPlan(4, plan.profile(), plan.platform(), plan.architecture(), plan.mode(), plan.actions(),
                executionPolicyDigest, calculateDigest(4, plan.profile(), plan.platform(), plan.architecture(),
                plan.mode(), plan.actions(), executionPolicyDigest));
    }

    private static String calculateDigest(int schemaVersion, SetupProfile profile, SetupPlatform platform,
                                          SetupArchitecture architecture, SetupMode mode,
                                          List<SetupAction> actions, String executionPolicyDigest) {
        StringBuilder canonical = new StringBuilder();
        append(canonical, Integer.toString(schemaVersion));
        append(canonical, profile.name());
        append(canonical, platform.name());
        append(canonical, architecture.name());
        append(canonical, mode.name());
        if (schemaVersion >= 3) append(canonical, executionPolicyDigest);
        append(canonical, Integer.toString(actions.size()));
        actions.forEach(action -> {
            append(canonical, action.target().name());
            append(canonical, action.kind().name());
            append(canonical, action.version());
            append(canonical, action.source().toString());
            append(canonical, action.checksum());
            if (schemaVersion >= 4) append(canonical, Long.toString(action.artifactBytes()));
            append(canonical, action.dependencyLockChecksum());
            append(canonical, Boolean.toString(action.privileged()));
            append(canonical, Integer.toString(action.requiredLicenses().size()));
            action.requiredLicenses().stream().sorted().forEach(license -> append(canonical, license));
        });
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            for (int index = 0; index < canonical.length(); index++) {
                char codeUnit = canonical.charAt(index);
                digest.update((byte) (codeUnit >>> 8));
                digest.update((byte) codeUnit);
            }
            return "sha256:" + HexFormat.of().formatHex(digest.digest());
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("SHA-256 is required by the Java platform.", impossible);
        }
    }

    private static void requireSha256(String value, String name) {
        if (value == null || !value.matches("sha256:[0-9a-fA-F]{64}")) {
            throw new IllegalArgumentException(name + " must be a SHA-256 digest.");
        }
    }

    private static void append(StringBuilder canonical, String value) {
        canonical.append(value.length()).append(':').append(value);
    }

    private static void validatePolicy(SetupMode mode, List<SetupAction> actions) {
        Map<SetupTarget, SetupTargetDefinition> definitions = SetupCatalog.builtIn().targets().stream()
                .collect(Collectors.toUnmodifiableMap(SetupTargetDefinition::target, Function.identity()));
        for (SetupAction action : actions) {
            if (mode == SetupMode.EXTERNAL && action.kind() != SetupActionKind.DIAGNOSE) {
                throw new IllegalArgumentException("External setup plans may only diagnose.");
            }
            Set<SetupCapability> capabilities = definitions.get(action.target()).capabilities();
            SetupCapability required = switch (action.kind()) {
                case DOWNLOAD, INSTALL, CONFIGURE -> SetupCapability.INSTALLABLE;
                case PREWARM -> SetupCapability.PREWARMABLE;
                case START -> SetupCapability.STARTABLE;
                case CLEAN -> SetupCapability.CLEANABLE;
                case ROLLBACK -> SetupCapability.ROLLBACKABLE;
                case DIAGNOSE -> null;
            };
            if (required != null && !capabilities.contains(required)) {
                throw new IllegalArgumentException(action.target() + " does not support " + action.kind());
            }
            boolean expectedPrivileged = action.kind() != SetupActionKind.DIAGNOSE
                    && capabilities.contains(SetupCapability.PRIVILEGED);
            if (action.privileged() != expectedPrivileged) {
                throw new IllegalArgumentException("Privilege disclosure does not match trusted target metadata.");
            }
        }
    }
}
