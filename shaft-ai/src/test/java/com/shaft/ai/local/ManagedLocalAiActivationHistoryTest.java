package com.shaft.ai.local;

import org.junit.jupiter.api.Test;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.ObjectNode;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ManagedLocalAiActivationHistoryTest {
    @Test
    void safelyMigratesTheImmediatelyPriorReviewedSchema() throws Exception {
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        ManagedLocalAiManifest.RuntimeAsset runtime = manifest.runtime().assets().stream()
                .filter(candidate -> candidate.platform().equals("windows-x86_64")).findFirst().orElseThrow();
        ManagedLocalAiManifest.ModelManifest model = manifest.models().stream()
                .filter(candidate -> candidate.id().equals("qwen3-0.6b-q8_0")).findFirst().orElseThrow();
        ManagedLocalAiSnapshot snapshot = new ManagedLocalAiSnapshot(ManagedLocalAiSnapshot.State.READY,
                "ready", java.nio.file.Path.of("C:/managed-ai").toAbsolutePath(), true, true, model.id(),
                model.id(), runtime.platform(), manifest.runtime().id(), manifest.runtime().version(),
                manifest.runtime().license(), runtime.file(), runtime.sha256(), runtime.executable(), runtime.size(),
                ManagedLocalAiSnapshot.CacheHealth.READY, ManagedLocalAiSnapshot.CacheHealth.READY,
                ManagedLocalAiSnapshot.Phase.IDLE, 0, 0, 16L << 30, 8, 64L << 30,
                java.util.Map.of(model.id(), new ManagedLocalAiSnapshot.Model(model.displayName(), model.tier(),
                        model.license(), model.revision(), model.file(), model.sha256(), model.automatic(), true,
                        java.util.List.of(), model.size(), model.size())));
        ManagedLocalAiActivationHistory.Activation activation =
                ManagedLocalAiActivationHistory.from(snapshot, manifest);
        JsonMapper json = JsonMapper.builder().build();
        ObjectNode legacy = (ObjectNode) json.readTree(ManagedLocalAiActivationHistory.serialize(
                new ManagedLocalAiActivationHistory.History(activation, activation)));
        legacy.put("schemaVersion", 1);
        for (String side : java.util.List.of("active", "previous")) {
            ObjectNode value = (ObjectNode) legacy.get(side);
            for (String field : java.util.List.of("runtimeAbi", "runtimeMinimumAbiVersion", "modelAutomatic",
                    "modelFirstPartyQuantization", "modelMinimumRamGb", "modelMinimumCpuCount",
                    "modelMinimumFreeDiskGb")) {
                value.remove(field);
            }
        }

        ManagedLocalAiActivationHistory.History migrated =
                ManagedLocalAiActivationHistory.parse(json.writeValueAsBytes(legacy));

        assertEquals(activation, migrated.active());
        assertEquals(activation, migrated.previous());
    }
}
