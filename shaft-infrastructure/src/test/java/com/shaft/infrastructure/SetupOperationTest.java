package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SetupOperationTest {
    private static final String CHECKSUM = "sha256:" + "a".repeat(64);

    @Test
    void exactActionKindsRecoverOneOperationAndRejectMixedPlans() {
        SetupAction clean = action(SetupTarget.MANAGED_LOCAL_AI_RUNTIME, SetupActionKind.CLEAN);
        SetupAction rollback = action(SetupTarget.MANAGED_LOCAL_AI_MODEL, SetupActionKind.ROLLBACK);
        SetupAction install = action(SetupTarget.MANAGED_LOCAL_AI_MODEL, SetupActionKind.INSTALL);

        assertEquals(SetupOperation.CLEAN, SetupOperation.fromPlan(plan(List.of(clean))));
        assertEquals(SetupOperation.ROLLBACK, SetupOperation.fromPlan(plan(List.of(rollback))));
        assertEquals(SetupOperation.INSTALL, SetupOperation.fromPlan(plan(List.of(install))));
        assertThrows(IllegalArgumentException.class, () -> SetupOperation.fromPlan(plan(List.of(clean, install))));
        assertThrows(IllegalArgumentException.class, () -> SetupOperation.fromPlan(plan(List.of(clean, rollback))));
    }

    private static SetupPlan plan(List<SetupAction> actions) {
        return SetupPlan.create(SetupProfile.LOCAL_AI, SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, actions);
    }

    private static SetupAction action(SetupTarget target, SetupActionKind kind) {
        return new SetupAction(target, kind, "reviewed", URI.create("https://example.invalid/artifact"),
                CHECKSUM, false, Set.of());
    }
}
