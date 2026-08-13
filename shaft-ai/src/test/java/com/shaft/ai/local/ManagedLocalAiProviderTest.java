package com.shaft.ai.local;

import com.shaft.pilot.ai.AiProviderAvailability;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiUsage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import tools.jackson.databind.node.JsonNodeFactory;

import java.nio.file.Path;
import java.time.Duration;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManagedLocalAiProviderTest {
    @TempDir
    Path temp;

    @Test
    void availabilityAllowsReadyAndTransparentProvisionableStatesOnly() {
        assertTrue(provider(snapshot(ManagedLocalAiSnapshot.State.READY, true), null, true).availability().available());
        assertTrue(provider(snapshot(ManagedLocalAiSnapshot.State.NOT_PROVISIONED, true), null, true)
                .availability().available());

        AiProviderAvailability explicit = provider(
                snapshot(ManagedLocalAiSnapshot.State.NOT_PROVISIONED, false), null, true).availability();
        assertFalse(explicit.available());
        assertEquals("Provision the reviewed managed runtime and model.", explicit.reason());
        assertFalse(provider(snapshot(ManagedLocalAiSnapshot.State.EXCLUDED, true), null, true)
                .availability().available());
        assertFalse(provider(snapshot(ManagedLocalAiSnapshot.State.READY, true), null, false)
                .availability().available());
    }

    @Test
    void executionDelegatesOnlyTheAlreadyApprovedRequest() {
        AiRequest request = AiRequest.builder("managed-provider", JsonNodeFactory.instance.objectNode())
                .deterministicFallback(JsonNodeFactory.instance.objectNode().put("fallback", true)).build();
        AiResponse expected = AiResponse.success("managed-local", "selected-model",
                JsonNodeFactory.instance.objectNode().put("answer", "local"), Duration.ofMillis(5),
                AiUsage.empty(), request.deterministicFallback());

        AtomicReference<AiRequest> received = new AtomicReference<>();
        AtomicInteger calls = new AtomicInteger();
        ManagedLocalAiProvider provider = new ManagedLocalAiProvider(new ManagedLocalAiProvider.Lifecycle() {
            @Override public boolean executable() { return true; }
            @Override public ManagedLocalAiSnapshot inspect() {
                return snapshot(ManagedLocalAiSnapshot.State.READY, true);
            }
            @Override public AiResponse execute(AiRequest actual) {
                received.set(actual);
                calls.incrementAndGet();
                return expected;
            }
        });

        assertSame(expected, provider.execute(request));
        assertSame(request, received.get());
        assertEquals(1, calls.get());

        AiResponse failed = new ManagedLocalAiProvider(new ManagedLocalAiProvider.Lifecycle() {
            @Override public boolean executable() { return true; }
            @Override public ManagedLocalAiSnapshot inspect() { throw new AssertionError(); }
            @Override public AiResponse execute(AiRequest ignored) {
                throw new IllegalStateException("secret-lifecycle-detail");
            }
        }).execute(request);
        assertEquals(request.deterministicFallback(), failed.structuredPayload());
        assertFalse(failed.fallbackReason().contains("secret-lifecycle-detail"));
    }

    private ManagedLocalAiProvider provider(ManagedLocalAiSnapshot snapshot, AiResponse response,
                                             boolean executable) {
        return new ManagedLocalAiProvider(new ManagedLocalAiProvider.Lifecycle() {
            @Override
            public boolean executable() {
                return executable;
            }

            @Override
            public ManagedLocalAiSnapshot inspect() {
                return snapshot;
            }

            @Override
            public AiResponse execute(AiRequest request) {
                return response;
            }
        });
    }

    private ManagedLocalAiSnapshot snapshot(ManagedLocalAiSnapshot.State state, boolean transparent) {
        return new ManagedLocalAiSnapshot(state, "Provision the reviewed managed runtime and model.",
                temp.toAbsolutePath(), true, transparent, "auto", "selected-model", "windows-x86_64",
                "llama.cpp", "test", "MIT", "runtime.zip", "0".repeat(64), "llama-server.exe", 1,
                ManagedLocalAiSnapshot.CacheHealth.MISSING, ManagedLocalAiSnapshot.CacheHealth.MISSING,
                ManagedLocalAiSnapshot.Phase.IDLE, 0, 0, 16, 8, 32, Map.of());
    }
}
