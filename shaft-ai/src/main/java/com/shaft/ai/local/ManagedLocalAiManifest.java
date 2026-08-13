package com.shaft.ai.local;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.core.StreamReadFeature;
import tools.jackson.core.JacksonException;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Immutable, validated artifact inventory for SHAFT-managed local inference.
 *
 * <p>The manifest is data, not authority: parsing succeeds only when every consumed field is
 * structurally valid and bound to the reviewed upstream host, release, revision, and filename.</p>
 */
final class ManagedLocalAiManifest {
    private static final ObjectMapper JSON = JsonMapper.builder()
            .enable(StreamReadFeature.STRICT_DUPLICATE_DETECTION)
            .build();
    private static final String RESOURCE = "/com/shaft/ai/local/manifest.json";
    private static final Pattern SHA_256 = Pattern.compile("[0-9a-f]{64}");
    private static final Pattern PORTABLE_NAME = Pattern.compile("[A-Za-z0-9][A-Za-z0-9._+-]{0,199}");
    private static final Set<String> LICENSES = Set.of("MIT", "Apache-2.0");
    private static final Set<String> WINDOWS_DEVICES = Set.of("CON", "PRN", "AUX", "NUL",
            "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9",
            "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9");

    private final int schemaVersion;
    private final RuntimeManifest runtime;
    private final List<ModelManifest> models;

    private ManagedLocalAiManifest(int schemaVersion, RuntimeManifest runtime, List<ModelManifest> models) {
        this.schemaVersion = schemaVersion;
        this.runtime = runtime;
        this.models = List.copyOf(models);
    }

    static ManagedLocalAiManifest loadDefault() {
        try (InputStream input = ManagedLocalAiManifest.class.getResourceAsStream(RESOURCE)) {
            if (input == null) {
                throw new IllegalStateException("Managed local AI manifest is not packaged.");
            }
            return parse(input);
        } catch (IOException failure) {
            throw new IllegalStateException("Managed local AI manifest cannot be read.", failure);
        }
    }

    static ManagedLocalAiManifest parse(InputStream input) {
        try {
            JsonNode root = JSON.readTree(input);
            requireObject(root, "manifest");
            requireFields(root, "manifest", Set.of("schemaVersion", "runtime", "models"));
            int schemaVersion = positiveInt(root.get("schemaVersion"), "schemaVersion");
            if (schemaVersion != 1) {
                throw invalid("schemaVersion must be 1");
            }
            RuntimeManifest runtime = parseRuntime(root.get("runtime"));
            List<ModelManifest> models = parseModels(root.get("models"));
            return new ManagedLocalAiManifest(schemaVersion, runtime, models);
        } catch (JacksonException malformed) {
            throw new IllegalArgumentException("Invalid managed local AI manifest: malformed JSON.", malformed);
        }
    }

    int schemaVersion() {
        return schemaVersion;
    }

    RuntimeManifest runtime() {
        return runtime;
    }

    List<ModelManifest> models() {
        return models;
    }

    RuntimeAsset selectRuntime(String osName, String architecture, String abi, String abiVersion) {
        String platform = platform(osName, architecture);
        return runtime.assets().stream()
                .filter(asset -> asset.platform().equals(platform))
                .filter(asset -> compatibleAbi(asset, abi, abiVersion))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("No compatible managed local AI runtime is available."));
    }

    boolean supportsRuntime(String osName, String architecture, String abi, String abiVersion) {
        try {
            selectRuntime(osName, architecture, abi, abiVersion);
            return true;
        } catch (IllegalArgumentException unsupported) {
            return false;
        }
    }

    private static RuntimeManifest parseRuntime(JsonNode node) {
        requireObject(node, "runtime");
        requireFields(node, "runtime", Set.of("id", "version", "license", "releaseUrl", "assets"));
        String id = text(node, "id");
        String version = portableName(text(node, "version"), "runtime.version");
        String license = license(text(node, "license"), "runtime.license");
        URI release = https(node, "releaseUrl");
        if (!"llama.cpp".equals(id) || !"github.com".equalsIgnoreCase(release.getHost())
                || !release.getPath().equals("/ggml-org/llama.cpp/releases/tag/" + version)
                || release.getQuery() != null || release.getFragment() != null) {
            throw invalid("runtime release provenance is not trusted");
        }
        JsonNode assetsNode = node.get("assets");
        requireNonEmptyArray(assetsNode, "runtime.assets");
        List<RuntimeAsset> assets = new ArrayList<>();
        Set<String> platforms = new HashSet<>();
        for (JsonNode assetNode : assetsNode) {
            requireObject(assetNode, "runtime asset");
            Set<String> assetFields = new HashSet<>(Set.of("platform", "file", "url", "size", "sha256",
                    "executable", "abi"));
            if (assetNode.has("minimumAbiVersion")) {
                assetFields.add("minimumAbiVersion");
            }
            requireFields(assetNode, "runtime asset", assetFields);
            String platform = text(assetNode, "platform");
            if (!Set.of("windows-x86_64", "windows-aarch64", "macos-x86_64", "macos-aarch64",
                    "linux-x86_64", "linux-aarch64").contains(platform) || !platforms.add(platform)) {
                throw invalid("runtime platform is unsupported or duplicated");
            }
            String file = portableName(text(assetNode, "file"), "runtime file");
            URI url = https(assetNode, "url");
            String expectedPath = "/ggml-org/llama.cpp/releases/download/" + version + "/" + file;
            if (!"github.com".equalsIgnoreCase(url.getHost()) || !expectedPath.equals(url.getPath())
                    || url.getQuery() != null || url.getFragment() != null) {
                throw invalid("runtime asset provenance is not trusted");
            }
            long size = positiveLong(assetNode.get("size"), "runtime size");
            String digest = digest(text(assetNode, "sha256"), "runtime sha256");
            String executable = portableName(text(assetNode, "executable"), "runtime executable");
            String abi = text(assetNode, "abi");
            String minimumAbiVersion = optionalText(assetNode, "minimumAbiVersion");
            validateAbi(platform, abi, minimumAbiVersion);
            assets.add(new RuntimeAsset(platform, file, url, size, digest, executable, abi, minimumAbiVersion));
        }
        if (assets.size() != 6) {
            throw invalid("runtime assets must cover exactly six desktop platforms");
        }
        return new RuntimeManifest(id, version, license, release, List.copyOf(assets));
    }

    private static List<ModelManifest> parseModels(JsonNode node) {
        requireNonEmptyArray(node, "models");
        List<ModelManifest> models = new ArrayList<>();
        Set<String> identifiers = new HashSet<>();
        for (JsonNode model : node) {
            requireObject(model, "model");
            requireFields(model, "model", Set.of("id", "displayName", "tier", "automatic",
                    "firstPartyQuantization", "license", "source", "revision", "file", "url", "size",
                    "sha256", "minimumRamGb", "minimumCpuCount", "minimumFreeDiskGb"));
            String id = portableName(text(model, "id"), "model id");
            if (!identifiers.add(id)) {
                throw invalid("model id is duplicated");
            }
            String displayName = boundedText(model, "displayName", 200);
            String tier = text(model, "tier");
            if (!Set.of("lite", "balanced", "challenger").contains(tier)) {
                throw invalid("model tier is unsupported");
            }
            boolean automatic = booleanValue(model, "automatic");
            boolean firstParty = booleanValue(model, "firstPartyQuantization");
            String license = license(text(model, "license"), "model license");
            String source = source(text(model, "source"));
            String revision = revision(text(model, "revision"));
            String file = portableName(text(model, "file"), "model file");
            URI url = https(model, "url");
            String expectedPath = "/" + source + "/resolve/" + revision + "/" + file;
            if (!"huggingface.co".equalsIgnoreCase(url.getHost()) || !expectedPath.equals(url.getPath())
                    || url.getQuery() != null || url.getFragment() != null) {
                throw invalid("model provenance is not trusted");
            }
            if (automatic && !firstParty) {
                throw invalid("automatic models must use first-party quantization");
            }
            models.add(new ModelManifest(id, displayName, tier, automatic, firstParty, license, source, revision,
                    file, url, positiveLong(model.get("size"), "model size"), digest(text(model, "sha256"),
                    "model sha256"), positiveNumber(model.get("minimumRamGb"), "minimumRamGb"),
                    positiveInt(model.get("minimumCpuCount"), "minimumCpuCount"),
                    positiveNumber(model.get("minimumFreeDiskGb"), "minimumFreeDiskGb")));
        }
        return List.copyOf(models);
    }

    private static String platform(String osName, String architecture) {
        String os = osName.toLowerCase(Locale.ROOT);
        String normalizedOs = os.startsWith("windows") ? "windows"
                : os.startsWith("mac") || os.startsWith("darwin") ? "macos"
                : os.startsWith("linux") ? "linux" : "";
        String arch = architecture.toLowerCase(Locale.ROOT);
        String normalizedArch = switch (arch) {
            case "amd64", "x86_64", "x64" -> "x86_64";
            case "aarch64", "arm64" -> "aarch64";
            default -> "";
        };
        if (normalizedOs.isEmpty() || normalizedArch.isEmpty()) {
            throw invalid("host platform is unsupported");
        }
        return normalizedOs + "-" + normalizedArch;
    }

    private static boolean compatibleAbi(RuntimeAsset asset, String family, String version) {
        String suppliedFamily = family == null ? "" : family;
        String normalizedFamily = asset.platform().startsWith("linux-")
                && Set.of("glibc", "gnu libc").contains(suppliedFamily.toLowerCase(Locale.ROOT))
                ? "linux-glibc" : suppliedFamily;
        if (!asset.abi().equalsIgnoreCase(normalizedFamily)) {
            return false;
        }
        if (!asset.platform().startsWith("linux-")) {
            return true;
        }
        int[] current = version(version);
        int[] minimum = version(asset.minimumAbiVersion());
        return current[0] > minimum[0] || current[0] == minimum[0] && current[1] >= minimum[1];
    }

    private static int[] version(String value) {
        if (value == null || !value.matches("[0-9]+\\.[0-9]+(?:\\.[0-9]+)?")) {
            throw invalid("ABI version is malformed");
        }
        String[] parts = value.split("\\.");
        return new int[]{Integer.parseInt(parts[0]), Integer.parseInt(parts[1])};
    }

    private static void validateAbi(String platform, String abi, String minimumVersion) {
        String expected = platform.startsWith("windows-") ? "windows-msvc"
                : platform.startsWith("macos-") ? "macos-darwin" : "linux-glibc";
        if (!expected.equals(abi)) {
            throw invalid("runtime ABI does not match its platform");
        }
        if (platform.startsWith("linux-")) {
            version(minimumVersion);
        } else if (!minimumVersion.isEmpty()) {
            throw invalid("minimum ABI version is only supported for Linux artifacts");
        }
    }

    private static URI https(JsonNode node, String field) {
        URI uri;
        try {
            uri = URI.create(text(node, field));
        } catch (IllegalArgumentException malformed) {
            throw invalid(field + " is not a valid URI");
        }
        if (!"https".equalsIgnoreCase(uri.getScheme()) || uri.getHost() == null || uri.getUserInfo() != null
                || uri.getPort() != -1) {
            throw invalid(field + " must be an authority-free HTTPS URI");
        }
        return uri;
    }

    private static String portableName(String value, String field) {
        if (!PORTABLE_NAME.matcher(value).matches() || value.endsWith(".") || value.endsWith(" ")) {
            throw invalid(field + " is not a portable basename");
        }
        String stem = value.split("\\.", 2)[0].toUpperCase(Locale.ROOT);
        if (WINDOWS_DEVICES.contains(stem)) {
            throw invalid(field + " is a reserved device name");
        }
        return value;
    }

    private static String source(String value) {
        if (!value.matches("[A-Za-z0-9._-]+/[A-Za-z0-9._-]+")) {
            throw invalid("model source is malformed");
        }
        return value;
    }

    private static String revision(String value) {
        if (!value.matches("[0-9a-f]{40}")) {
            throw invalid("model revision must be a full commit digest");
        }
        return value;
    }

    private static String digest(String value, String field) {
        if (!SHA_256.matcher(value).matches()) {
            throw invalid(field + " must be a lowercase SHA-256 digest");
        }
        return value;
    }

    private static String license(String value, String field) {
        if (!LICENSES.contains(value)) {
            throw invalid(field + " is unsupported");
        }
        return value;
    }

    private static String text(JsonNode node, String field) {
        JsonNode value = node == null ? null : node.get(field);
        if (value == null || !value.isTextual() || value.asText().isBlank()) {
            throw invalid(field + " must be a non-blank string");
        }
        return value.asText();
    }

    private static String optionalText(JsonNode node, String field) {
        JsonNode value = node.get(field);
        if (value == null || value.isNull()) {
            return "";
        }
        if (!value.isTextual()) {
            throw invalid(field + " must be a string");
        }
        return value.asText();
    }

    private static String boundedText(JsonNode node, String field, int maximum) {
        String value = text(node, field);
        if (value.length() > maximum) {
            throw invalid(field + " is too long");
        }
        return value;
    }

    private static boolean booleanValue(JsonNode node, String field) {
        JsonNode value = node.get(field);
        if (value == null || !value.isBoolean()) {
            throw invalid(field + " must be a boolean");
        }
        return value.asBoolean();
    }

    private static int positiveInt(JsonNode node, String field) {
        if (node == null || !node.isIntegralNumber() || !node.canConvertToInt() || node.asInt() <= 0) {
            throw invalid(field + " must be a positive integer");
        }
        return node.asInt();
    }

    private static long positiveLong(JsonNode node, String field) {
        if (node == null || !node.isIntegralNumber() || !node.canConvertToLong() || node.asLong() <= 0) {
            throw invalid(field + " must be a positive integer");
        }
        return node.asLong();
    }

    private static double positiveNumber(JsonNode node, String field) {
        if (node == null || !node.isNumber() || !Double.isFinite(node.asDouble()) || node.asDouble() <= 0) {
            throw invalid(field + " must be a positive finite number");
        }
        return node.asDouble();
    }

    private static void requireObject(JsonNode node, String field) {
        if (node == null || !node.isObject()) {
            throw invalid(field + " must be an object");
        }
    }

    private static void requireNonEmptyArray(JsonNode node, String field) {
        if (node == null || !node.isArray() || node.isEmpty()) {
            throw invalid(field + " must be a non-empty array");
        }
    }

    private static void requireFields(JsonNode node, String field, Set<String> expected) {
        if (!Set.copyOf(node.propertyNames()).equals(expected)) {
            throw invalid(field + " contains missing or unknown fields");
        }
    }

    private static IllegalArgumentException invalid(String reason) {
        return new IllegalArgumentException("Invalid managed local AI manifest: " + reason + ".");
    }

    record RuntimeManifest(String id, String version, String license, URI releaseUrl, List<RuntimeAsset> assets) {
    }

    record RuntimeAsset(String platform, String file, URI url, long size, String sha256, String executable,
                        String abi, String minimumAbiVersion) {
    }

    record ModelManifest(String id, String displayName, String tier, boolean automatic,
                         boolean firstPartyQuantization, String license, String source, String revision,
                         String file, URI url, long size, String sha256, double minimumRamGb,
                         int minimumCpuCount, double minimumFreeDiskGb) {
    }
}
