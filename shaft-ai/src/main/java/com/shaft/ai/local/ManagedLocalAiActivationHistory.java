package com.shaft.ai.local;

import tools.jackson.core.StreamReadFeature;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.net.URI;
import java.nio.channels.FileChannel;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.util.Set;
import java.util.UUID;
import java.util.function.BooleanSupplier;
import java.util.regex.Pattern;

/** Atomic, privacy-safe record of the current and immediately prior reviewed activation. */
final class ManagedLocalAiActivationHistory {
    static final String FILE = "activation-history.json";
    private static final ObjectMapper JSON = JsonMapper.builder()
            .enable(StreamReadFeature.STRICT_DUPLICATE_DETECTION).build();
    private static final Set<String> ROOT_FIELDS = Set.of("schemaVersion", "active", "previous");
    private static final Set<String> LEGACY_ACTIVATION_FIELDS = Set.of(
            "runtimeId", "runtimeVersion", "runtimePlatform", "runtimeLicense", "runtimeFile",
            "runtimeUrl", "runtimeSha256", "runtimeArtifactBytes", "runtimeExecutable", "modelId",
            "modelArtifactId", "modelName", "modelTier", "modelLicense", "modelSource", "modelRevision",
            "modelFile", "modelUrl", "modelSha256", "modelArtifactBytes");
    private static final Set<String> ACTIVATION_FIELDS = new java.util.HashSet<>(LEGACY_ACTIVATION_FIELDS);
    static {
        ACTIVATION_FIELDS.addAll(Set.of("runtimeAbi", "runtimeMinimumAbiVersion", "modelAutomatic",
            "modelFirstPartyQuantization", "modelMinimumRamGb", "modelMinimumCpuCount",
            "modelMinimumFreeDiskGb"));
    }
    private static final Pattern PORTABLE_NAME = Pattern.compile("[A-Za-z0-9][A-Za-z0-9._+-]{0,199}");
    private static final Set<String> PLATFORMS = Set.of("windows-x86_64", "windows-aarch64", "macos-x86_64",
            "macos-aarch64", "linux-x86_64", "linux-aarch64");
    private static final Set<String> LICENSES = Set.of("MIT", "Apache-2.0");
    private static final Set<String> TIERS = Set.of("lite", "balanced", "challenger");

    private ManagedLocalAiActivationHistory() {
        throw new IllegalStateException("Utility class");
    }

    static Activation from(ManagedLocalAiSnapshot snapshot, ManagedLocalAiManifest manifest) {
        if (snapshot.state() != ManagedLocalAiSnapshot.State.READY || snapshot.selectedModelId() == null) {
            throw new IllegalArgumentException("Only a ready managed-local snapshot can be activated.");
        }
        ManagedLocalAiSnapshot.Model snapshotModel = snapshot.models().get(snapshot.selectedModelId());
        ManagedLocalAiManifest.RuntimeAsset asset = manifest.runtime().assets().stream()
                .filter(candidate -> candidate.platform().equals(snapshot.platform())).findFirst().orElseThrow();
        ManagedLocalAiManifest.ModelManifest model = manifest.models().stream()
                .filter(candidate -> candidate.id().equals(snapshot.selectedModelId())).findFirst().orElseThrow();
        if (snapshotModel == null || !snapshot.runtimeVersion().equals(manifest.runtime().version())
                || !snapshot.runtimeAssetSha256().equals(asset.sha256())
                || !snapshotModel.sha256().equals(model.sha256())) {
            throw new IllegalArgumentException("Ready managed-local snapshot has no selected model metadata.");
        }
        return new Activation(ManagedLocalAiService.runtimeInstallationId(manifest, snapshot.platform()),
                manifest.runtime().version(), snapshot.platform(), manifest.runtime().license(), asset.file(),
                asset.url().toString(), asset.sha256(), asset.size(), asset.executable(), asset.abi(),
                asset.minimumAbiVersion(),
                ManagedLocalAiService.modelInstallationId(model), model.id(), model.displayName(), model.tier(),
                model.license(), model.source(), model.revision(), model.file(), model.url().toString(),
                model.sha256(), model.size(), model.automatic(), model.firstPartyQuantization(),
                model.minimumRamGb(), model.minimumCpuCount(), model.minimumFreeDiskGb());
    }

    static boolean publish(Path cache, Duration timeout, Activation activation, BooleanSupplier complete)
            throws Exception {
        return ManagedLocalAiCache.withLock(cache, timeout, () -> publishLocked(cache, activation, complete));
    }

    static History readVerified(Path cache) throws IOException {
        Path target = cache.toAbsolutePath().normalize().resolve(FILE);
        byte[] value = readBytes(target);
        if (value == null) {
            return null;
        }
        History history = parse(value);
        verifyActivation(cache, history.active());
        return history;
    }

    static Activation rollbackCandidate(Path cache) throws IOException {
        History history = readVerified(cache);
        if (history == null || history.previous() == null) {
            return null;
        }
        verifyActivation(cache, history.previous());
        return history.previous();
    }

    static void rollbackLocked(Path cache, Activation expected, CheckedRunnable beforeCommit) throws Exception {
        Path target = cache.toAbsolutePath().normalize().resolve(FILE);
        byte[] value = readBytes(target);
        if (value == null) {
            throw new IllegalStateException("No reviewed managed-local rollback candidate is available.");
        }
        History history = parse(value);
        if (history.previous() == null || !history.previous().equals(expected)) {
            throw new IllegalStateException("Managed-local rollback candidate changed before execution.");
        }
        verifyActivation(cache, history.active());
        verifyActivation(cache, history.previous());
        beforeCommit.run();
        write(target, serialize(new History(history.previous(), history.active())));
    }

    static void clearLocked(Path cache) throws IOException {
        Path target = cache.toAbsolutePath().normalize().resolve(FILE);
        if (Files.exists(target, LinkOption.NOFOLLOW_LINKS)
                && !Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS)) {
            throw new IllegalStateException("Managed-local activation history is not a regular file.");
        }
        Files.deleteIfExists(target);
    }

    private static boolean publishLocked(Path cache, Activation activation, BooleanSupplier complete)
            throws IOException {
        Path target = cache.toAbsolutePath().normalize().resolve(FILE);
        byte[] priorBytes = readBytes(target);
        History prior = priorBytes == null ? null : parse(priorBytes);
        History next = prior == null
                ? new History(activation, null)
                : prior.active().equals(activation) ? prior : new History(activation, prior.active());
        verifyActivation(cache, activation);
        if (!complete.getAsBoolean()) {
            return false;
        }
        write(target, serialize(next));
        return true;
    }

    private static void verifyActivation(Path cache, Activation activation) throws IOException {
        ManagedLocalAiCache.Installation runtime = ManagedLocalAiCache.verify(cache, activation.runtimeId());
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.verify(cache, activation.modelId());
        requireOwned(runtime, activation.runtimeFile(), activation.runtimeArtifactBytes(),
                activation.runtimeSha256());
        requireOwned(runtime, activation.runtimeExecutable(), null, null);
        requireOwned(model, activation.modelFile(), activation.modelArtifactBytes(), activation.modelSha256());
    }

    private static void requireOwned(ManagedLocalAiCache.Installation installation, String name,
                                     Long size, String sha256) {
        long matches = installation.files().stream().filter(file -> {
            Path path = Path.of(file.path());
            return path.getFileName() != null && path.getFileName().toString().equals(name)
                    && (size == null || file.size() == size)
                    && (sha256 == null || file.sha256().equals(sha256));
        }).count();
        if (matches != 1) {
            throw new IllegalStateException("Managed-local activation does not match its owned installation.");
        }
    }

    private static byte[] readBytes(Path target) throws IOException {
        if (!Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
            return null;
        }
        if (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS)) {
            throw new IllegalStateException("Managed-local activation history is not a regular file.");
        }
        return Files.readAllBytes(target);
    }

    static History parse(byte[] value) throws IOException {
        JsonNode root = JSON.readTree(value);
        requireObject(root, "history");
        requireFields(root, ROOT_FIELDS, "history");
        int schema = root.path("schemaVersion").asInt(-1);
        if (schema != 1 && schema != 2) {
            throw invalid("schemaVersion must be 1 or 2");
        }
        Activation active = parseActivation(root.get("active"), "active", schema);
        JsonNode previousValue = root.get("previous");
        Activation previous = previousValue == null || previousValue.isNull()
                ? null : parseActivation(previousValue, "previous", schema);
        return new History(active, previous);
    }

    private static Activation parseActivation(JsonNode node, String field, int schema) {
        requireObject(node, field);
        requireFields(node, schema == 1 ? LEGACY_ACTIVATION_FIELDS : ACTIVATION_FIELDS, field);
        if (schema == 1) {
            return migrateLegacy(node);
        }
        return new Activation(text(node, "runtimeId"), text(node, "runtimeVersion"),
                text(node, "runtimePlatform"), text(node, "runtimeLicense"), text(node, "runtimeFile"),
                text(node, "runtimeUrl"), digest(node, "runtimeSha256"),
                positiveLong(node, "runtimeArtifactBytes"), text(node, "runtimeExecutable"),
                text(node, "runtimeAbi"), stringValue(node, "runtimeMinimumAbiVersion"),
                text(node, "modelId"), text(node, "modelArtifactId"), text(node, "modelName"),
                text(node, "modelTier"), text(node, "modelLicense"), text(node, "modelSource"),
                text(node, "modelRevision"), text(node, "modelFile"), text(node, "modelUrl"),
                digest(node, "modelSha256"), positiveLong(node, "modelArtifactBytes"),
                booleanValue(node, "modelAutomatic"), booleanValue(node, "modelFirstPartyQuantization"),
                positiveDouble(node, "modelMinimumRamGb"), positiveInt(node, "modelMinimumCpuCount"),
                positiveDouble(node, "modelMinimumFreeDiskGb"));
    }

    static byte[] serialize(History history) throws IOException {
        ObjectNode root = JSON.createObjectNode();
        root.put("schemaVersion", 2);
        writeActivation(root.putObject("active"), history.active());
        if (history.previous() == null) {
            root.putNull("previous");
        } else {
            writeActivation(root.putObject("previous"), history.previous());
        }
        return JSON.writeValueAsBytes(root);
    }

    private static void writeActivation(ObjectNode node, Activation value) {
        node.put("runtimeId", value.runtimeId());
        node.put("runtimeVersion", value.runtimeVersion());
        node.put("runtimePlatform", value.runtimePlatform());
        node.put("runtimeLicense", value.runtimeLicense());
        node.put("runtimeFile", value.runtimeFile());
        node.put("runtimeUrl", value.runtimeUrl());
        node.put("runtimeSha256", value.runtimeSha256());
        node.put("runtimeArtifactBytes", value.runtimeArtifactBytes());
        node.put("runtimeExecutable", value.runtimeExecutable());
        node.put("runtimeAbi", value.runtimeAbi());
        node.put("runtimeMinimumAbiVersion", value.runtimeMinimumAbiVersion());
        node.put("modelId", value.modelId());
        node.put("modelArtifactId", value.modelArtifactId());
        node.put("modelName", value.modelName());
        node.put("modelTier", value.modelTier());
        node.put("modelLicense", value.modelLicense());
        node.put("modelSource", value.modelSource());
        node.put("modelRevision", value.modelRevision());
        node.put("modelFile", value.modelFile());
        node.put("modelUrl", value.modelUrl());
        node.put("modelSha256", value.modelSha256());
        node.put("modelArtifactBytes", value.modelArtifactBytes());
        node.put("modelAutomatic", value.modelAutomatic());
        node.put("modelFirstPartyQuantization", value.modelFirstPartyQuantization());
        node.put("modelMinimumRamGb", value.modelMinimumRamGb());
        node.put("modelMinimumCpuCount", value.modelMinimumCpuCount());
        node.put("modelMinimumFreeDiskGb", value.modelMinimumFreeDiskGb());
    }

    private static void write(Path target, byte[] value) throws IOException {
        Files.createDirectories(target.getParent());
        Path stage = target.resolveSibling(target.getFileName() + ".stage-" + UUID.randomUUID());
        try {
            Files.write(stage, value, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE);
            try (FileChannel channel = FileChannel.open(stage, StandardOpenOption.WRITE)) {
                channel.force(true);
            }
            Files.move(stage, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
        } catch (AtomicMoveNotSupportedException unsupported) {
            throw new IOException("Cache filesystem does not support atomic activation publication.", unsupported);
        } finally {
            Files.deleteIfExists(stage);
        }
    }

    private static void requireObject(JsonNode node, String field) {
        if (node == null || !node.isObject()) {
            throw invalid(field + " must be an object");
        }
    }

    private static void requireFields(JsonNode node, Set<String> expected, String field) {
        if (!Set.copyOf(node.propertyNames()).equals(expected)) {
            throw invalid(field + " contains missing or unknown fields");
        }
    }

    private static String text(JsonNode node, String field) {
        JsonNode value = node.get(field);
        if (value == null || !value.isTextual() || value.asText().isBlank()) {
            throw invalid(field + " must be non-empty text");
        }
        return value.asText();
    }

    private static String digest(JsonNode node, String field) {
        String value = text(node, field);
        if (!value.matches("[0-9a-f]{64}")) {
            throw invalid(field + " must be a lowercase SHA-256 digest");
        }
        return value;
    }

    private static long positiveLong(JsonNode node, String field) {
        JsonNode value = node.get(field);
        if (value == null || !value.isIntegralNumber() || value.asLong(-1) <= 0) {
            throw invalid(field + " must be positive");
        }
        return value.asLong();
    }

    private static int positiveInt(JsonNode node, String field) {
        JsonNode value = node.get(field);
        if (value == null || !value.isIntegralNumber() || !value.canConvertToInt() || value.asInt() <= 0) {
            throw invalid(field + " must be a positive integer");
        }
        return value.asInt();
    }

    private static double positiveDouble(JsonNode node, String field) {
        JsonNode value = node.get(field);
        double number = value == null ? Double.NaN : value.asDouble(Double.NaN);
        if (value == null || !value.isNumber() || !Double.isFinite(number) || number <= 0) {
            throw invalid(field + " must be positive");
        }
        return number;
    }

    private static boolean booleanValue(JsonNode node, String field) {
        JsonNode value = node.get(field);
        if (value == null || !value.isBoolean()) {
            throw invalid(field + " must be boolean");
        }
        return value.asBoolean();
    }

    private static String stringValue(JsonNode node, String field) {
        JsonNode value = node.get(field);
        if (value == null || !value.isTextual()) {
            throw invalid(field + " must be text");
        }
        return value.asText();
    }

    private static Activation migrateLegacy(JsonNode node) {
        String runtimeVersion = text(node, "runtimeVersion");
        String platform = text(node, "runtimePlatform");
        String runtimeSha = digest(node, "runtimeSha256");
        String modelRevision = text(node, "modelRevision");
        String modelSha = digest(node, "modelSha256");
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        ManagedLocalAiManifest.RuntimeAsset asset = manifest.runtime().assets().stream()
                .filter(candidate -> candidate.platform().equals(platform)
                        && candidate.sha256().equals(runtimeSha)
                        && manifest.runtime().version().equals(runtimeVersion))
                .findFirst().orElseThrow(() -> invalid("legacy runtime is no longer in the reviewed manifest"));
        ManagedLocalAiManifest.ModelManifest model = manifest.models().stream()
                .filter(candidate -> candidate.revision().equals(modelRevision) && candidate.sha256().equals(modelSha))
                .findFirst().orElseThrow(() -> invalid("legacy model is no longer in the reviewed manifest"));
        Activation migrated = fromLegacy(node, asset, manifest, model);
        return migrated;
    }

    private static Activation fromLegacy(JsonNode node, ManagedLocalAiManifest.RuntimeAsset asset,
                                         ManagedLocalAiManifest manifest,
                                         ManagedLocalAiManifest.ModelManifest model) {
        Activation migrated = new Activation(text(node, "runtimeId"), text(node, "runtimeVersion"),
                text(node, "runtimePlatform"), text(node, "runtimeLicense"), text(node, "runtimeFile"),
                text(node, "runtimeUrl"), digest(node, "runtimeSha256"),
                positiveLong(node, "runtimeArtifactBytes"), text(node, "runtimeExecutable"), asset.abi(),
                asset.minimumAbiVersion(), text(node, "modelId"), text(node, "modelArtifactId"),
                text(node, "modelName"), text(node, "modelTier"), text(node, "modelLicense"),
                text(node, "modelSource"), text(node, "modelRevision"), text(node, "modelFile"),
                text(node, "modelUrl"), digest(node, "modelSha256"), positiveLong(node, "modelArtifactBytes"),
                model.automatic(), model.firstPartyQuantization(), model.minimumRamGb(), model.minimumCpuCount(),
                model.minimumFreeDiskGb());
        Activation reviewed = fromManifest(manifest, asset, model);
        if (!migrated.equals(reviewed)) {
            throw invalid("legacy activation does not exactly match the reviewed manifest");
        }
        return migrated;
    }

    private static Activation fromManifest(ManagedLocalAiManifest manifest,
                                           ManagedLocalAiManifest.RuntimeAsset asset,
                                           ManagedLocalAiManifest.ModelManifest model) {
        return new Activation(ManagedLocalAiService.runtimeInstallationId(manifest, asset.platform()),
                manifest.runtime().version(), asset.platform(), manifest.runtime().license(), asset.file(),
                asset.url().toString(), asset.sha256(), asset.size(), asset.executable(), asset.abi(),
                asset.minimumAbiVersion(), ManagedLocalAiService.modelInstallationId(model), model.id(),
                model.displayName(), model.tier(), model.license(), model.source(), model.revision(), model.file(),
                model.url().toString(), model.sha256(), model.size(), model.automatic(),
                model.firstPartyQuantization(), model.minimumRamGb(), model.minimumCpuCount(),
                model.minimumFreeDiskGb());
    }

    private static IllegalStateException invalid(String reason) {
        return new IllegalStateException("Invalid managed-local activation history: " + reason + ".");
    }

    record Activation(String runtimeId, String runtimeVersion, String runtimePlatform, String runtimeLicense,
                      String runtimeFile, String runtimeUrl, String runtimeSha256, long runtimeArtifactBytes,
                      String runtimeExecutable, String runtimeAbi, String runtimeMinimumAbiVersion,
                      String modelId, String modelArtifactId, String modelName,
                      String modelTier, String modelLicense, String modelSource, String modelRevision,
                      String modelFile, String modelUrl, String modelSha256, long modelArtifactBytes,
                      boolean modelAutomatic, boolean modelFirstPartyQuantization, double modelMinimumRamGb,
                      int modelMinimumCpuCount, double modelMinimumFreeDiskGb) {
        Activation {
            portable(runtimeVersion, "runtimeVersion");
            portable(runtimeFile, "runtimeFile");
            portable(runtimeExecutable, "runtimeExecutable");
            portable(modelArtifactId, "modelArtifactId");
            portable(modelFile, "modelFile");
            bounded(modelName, "modelName", 200);
            if (runtimeId == null || !PORTABLE_NAME.matcher(runtimeId).matches()
                    || !PLATFORMS.contains(runtimePlatform) || !LICENSES.contains(runtimeLicense)
                    || runtimeSha256 == null || !runtimeSha256.matches("[0-9a-f]{64}")
                    || runtimeArtifactBytes <= 0 || modelId == null || !PORTABLE_NAME.matcher(modelId).matches()
                    || !validAbi(runtimePlatform, runtimeAbi, runtimeMinimumAbiVersion)
                    || !TIERS.contains(modelTier) || !LICENSES.contains(modelLicense)
                    || modelSource == null || !modelSource.matches("[A-Za-z0-9._-]+/[A-Za-z0-9._-]+")
                    || modelRevision == null || !modelRevision.matches("[0-9a-f]{40}")
                    || modelSha256 == null || !modelSha256.matches("[0-9a-f]{64}") || modelArtifactBytes <= 0
                    || modelAutomatic && !modelFirstPartyQuantization || !Double.isFinite(modelMinimumRamGb)
                    || modelMinimumRamGb <= 0 || modelMinimumCpuCount <= 0
                    || !Double.isFinite(modelMinimumFreeDiskGb) || modelMinimumFreeDiskGb <= 0) {
                throw invalid("activation fields are invalid");
            }
            String expectedRuntimeId = "llama.cpp-" + runtimeVersion + "-" + runtimePlatform + "-"
                    + runtimeSha256;
            String expectedModelId = "model-" + modelArtifactId + "-" + modelRevision + "-" + modelSha256;
            if (!runtimeId.equals(expectedRuntimeId) || !modelId.equals(expectedModelId)) {
                throw invalid("installation identities do not match artifact metadata");
            }
            URI runtime = https(runtimeUrl, "runtimeUrl");
            URI model = https(modelUrl, "modelUrl");
            if (!"github.com".equalsIgnoreCase(runtime.getHost())
                    || !runtime.getPath().equals("/ggml-org/llama.cpp/releases/download/" + runtimeVersion + "/"
                    + runtimeFile) || runtime.getQuery() != null || runtime.getFragment() != null
                    || !"huggingface.co".equalsIgnoreCase(model.getHost())
                    || !model.getPath().equals("/" + modelSource + "/resolve/" + modelRevision + "/" + modelFile)
                    || model.getQuery() != null || model.getFragment() != null) {
                throw invalid("artifact provenance is not trusted");
            }
        }
    }

    private static void portable(String value, String field) {
        if (value == null || !PORTABLE_NAME.matcher(value).matches() || value.endsWith(".") || value.endsWith(" ")) {
            throw invalid(field + " is not a portable basename");
        }
        String stem = value.split("\\.", 2)[0].toUpperCase(java.util.Locale.ROOT);
        if (ManagedLocalAiManifest.WINDOWS_DEVICES.contains(stem)) {
            throw invalid(field + " is a reserved device name");
        }
    }

    private static void bounded(String value, String field, int limit) {
        if (value == null || value.isBlank() || value.length() > limit || value.chars().anyMatch(Character::isISOControl)) {
            throw invalid(field + " is invalid");
        }
    }

    private static URI https(String value, String field) {
        try {
            URI uri = URI.create(value);
            if (!"https".equalsIgnoreCase(uri.getScheme()) || uri.getHost() == null || uri.getUserInfo() != null
                    || uri.getPort() != -1) {
                throw invalid(field + " must be an authority-free HTTPS URI");
            }
            return uri;
        } catch (IllegalArgumentException malformed) {
            throw invalid(field + " is not a valid URI");
        }
    }

    private static boolean validAbi(String platform, String abi, String minimum) {
        String expected = platform.startsWith("windows-") ? "windows-msvc"
                : platform.startsWith("macos-") ? "macos-darwin" : "linux-glibc";
        if (!expected.equals(abi) || minimum == null) return false;
        return platform.startsWith("linux-") ? minimum.matches("[0-9]+\\.[0-9]+(?:\\.[0-9]+)?")
                : minimum.isEmpty();
    }

    @FunctionalInterface
    interface CheckedRunnable {
        void run() throws Exception;
    }

    record History(Activation active, Activation previous) {
    }
}
