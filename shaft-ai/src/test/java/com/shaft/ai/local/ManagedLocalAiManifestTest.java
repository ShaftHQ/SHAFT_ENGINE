package com.shaft.ai.local;

import org.junit.jupiter.api.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManagedLocalAiManifestTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @Test
    void packagesTheReviewedManifestWithTheProviderAdapter() {
        assertNotNull(getClass().getResource("/com/shaft/ai/local/manifest.json"),
                "The reviewed managed-local-AI manifest must be packaged with shaft-ai.");
    }

    @Test
    void loadsTheReviewedRuntimeAndModelInventory() {
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();

        assertEquals(1, manifest.schemaVersion());
        assertEquals("llama.cpp", manifest.runtime().id());
        assertEquals("b10400", manifest.runtime().version());
        assertEquals(6, manifest.runtime().assets().size());
        assertEquals(5, manifest.models().size());
        ManagedLocalAiManifest.ModelManifest compact = manifest.models().stream()
                .filter(model -> model.id().equals("qwen3-0.6b-q8_0")).findFirst().orElseThrow();
        assertEquals("Qwen/Qwen3-0.6B-GGUF", compact.source());
        assertEquals("23749fefcc72300e3a2ad315e1317431b06b590a", compact.revision());
        assertEquals("Qwen3-0.6B-Q8_0.gguf", compact.file());
        assertEquals(639446688L, compact.size());
        assertEquals("9465e63a22add5354d9bb4b99e90117043c7124007664907259bd16d043bb031",
                compact.sha256());
        assertEquals("Apache-2.0", compact.license());
        assertFalse(compact.automatic(), "the compact baseline remains manual until its benchmark passes");
    }

    @Test
    void selectsOnlyAnExactCompatiblePlatform() {
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();

        assertEquals("linux-x86_64", manifest.selectRuntime("Linux", "amd64", "glibc", "2.31").platform());
        assertEquals("macos-aarch64",
                manifest.selectRuntime("Mac OS X", "aarch64", "macos-darwin", "").platform());
        assertTrue(manifest.supportsRuntime("Windows 11", "x86_64", "windows-msvc", ""));
        assertFalse(manifest.supportsRuntime("Windows 11", "x86_64", "linux-glibc", "2.31"));
        assertFalse(manifest.supportsRuntime("Mac OS X", "aarch64", "windows-msvc", ""));
        assertFalse(manifest.supportsRuntime("Linux", "amd64", "musl", "1.2"));
        assertFalse(manifest.supportsRuntime("Linux", "amd64", "glibc", "2.30"));
        assertFalse(manifest.supportsRuntime("Plan 9", "amd64", "", ""));
    }

    @Test
    void rejectsUntrustedOrAmbiguousManifestMutations() throws Exception {
        List<Mutation> mutations = List.of(
                new Mutation("path traversal", "/runtime/assets/0/file", "../runtime.zip"),
                new Mutation("non-HTTPS runtime", "/runtime/assets/0/url", "http://github.com/ggml-org/llama.cpp/releases/download/b10400/llama-b10400-bin-win-cpu-x64.zip"),
                new Mutation("wrong release host", "/runtime/assets/0/url", "https://attacker.invalid/ggml-org/llama.cpp/releases/download/b10400/llama-b10400-bin-win-cpu-x64.zip"),
                new Mutation("noncanonical HTTPS port", "/runtime/assets/0/url", "https://github.com:444/ggml-org/llama.cpp/releases/download/b10400/llama-b10400-bin-win-cpu-x64.zip"),
                new Mutation("invalid digest", "/runtime/assets/0/sha256", "abc"),
                new Mutation("missing ABI", "/runtime/assets/0/abi", ""),
                new Mutation("unsafe model id", "/models/0/id", "../../escape"),
                new Mutation("untrusted model host", "/models/0/url", "https://attacker.invalid/Qwen/Qwen3-1.7B-GGUF/resolve/90862c4b9d2787eaed51d12237eafdfe7c5f6077/Qwen3-1.7B-Q8_0.gguf"),
                new Mutation("boolean size", "/models/0/size", true)
        );

        for (Mutation mutation : mutations) {
            JsonNode root = JSON.readTree(defaultBytes());
            replace(root, mutation.pointer(), mutation.value());
            IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                    () -> ManagedLocalAiManifest.parse(new ByteArrayInputStream(JSON.writeValueAsBytes(root))),
                    mutation.name());
            assertFalse(failure.getMessage().isBlank(), mutation.name());
        }
    }

    @Test
    void rejectsUnknownAndDuplicateKeys() throws Exception {
        JsonNode unknown = JSON.readTree(defaultBytes());
        ((tools.jackson.databind.node.ObjectNode) unknown).put("ignoredTrustOverride", true);
        assertThrows(IllegalArgumentException.class,
                () -> ManagedLocalAiManifest.parse(new ByteArrayInputStream(JSON.writeValueAsBytes(unknown))));

        String duplicate = new String(defaultBytes(), StandardCharsets.UTF_8)
                .replaceFirst("\\{", "{\\\"schemaVersion\\\":999,");
        assertThrows(IllegalArgumentException.class,
                () -> ManagedLocalAiManifest.parse(new ByteArrayInputStream(duplicate.getBytes(StandardCharsets.UTF_8))));
    }

    private byte[] defaultBytes() throws Exception {
        try (var input = getClass().getResourceAsStream("/com/shaft/ai/local/manifest.json")) {
            assertNotNull(input);
            return input.readAllBytes();
        }
    }

    private static void replace(JsonNode root, String pointer, Object value) {
        int separator = pointer.lastIndexOf('/');
        JsonNode parent = root.at(pointer.substring(0, separator));
        String field = pointer.substring(separator + 1);
        if (parent.isArray()) {
            int index = Integer.parseInt(field);
            if (value instanceof Boolean booleanValue) {
                ((tools.jackson.databind.node.ArrayNode) parent).set(index,
                        tools.jackson.databind.node.BooleanNode.valueOf(booleanValue));
            }
        } else {
            ((tools.jackson.databind.node.ObjectNode) parent).put(field, String.valueOf(value));
        }
    }

    private record Mutation(String name, String pointer, Object value) {
    }
}
