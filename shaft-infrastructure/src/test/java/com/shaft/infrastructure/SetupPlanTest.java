package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;

import java.net.URI;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SetupPlanTest {
    @Test
    void digestIsDeterministicAndBindsEveryMutationRelevantField() {
        SetupAction action = action(SetupTarget.ALLURE, "2.34.1");
        SetupPlan first = SetupPlan.create(SetupPlatform.LINUX, SetupMode.MANAGED, List.of(action));
        SetupPlan same = SetupPlan.create(SetupPlatform.LINUX, SetupMode.MANAGED, List.of(action));
        SetupPlan changed = SetupPlan.create(SetupPlatform.LINUX, SetupMode.MANAGED,
                List.of(action(SetupTarget.ALLURE, "2.35.0")));

        assertEquals(first.digest(), same.digest());
        assertNotEquals(first.digest(), changed.digest());
        assertNotEquals(first.digest(), SetupPlan.create(SetupPlatform.WINDOWS, SetupMode.MANAGED,
                List.of(action)).digest());
        SetupAction aliasedSource = new SetupAction(SetupTarget.ALLURE, SetupActionKind.INSTALL, "2.34.1",
                URI.create("https://example.invalid/a/../allure"), checksum(), false, Set.of());
        assertNotEquals(first.digest(), SetupPlan.create(SetupPlatform.LINUX, SetupMode.MANAGED,
                List.of(aliasedSource)).digest());
        SetupAction surrogateOne = new SetupAction(SetupTarget.ALLURE, SetupActionKind.INSTALL, "\uD800",
                URI.create("https://example.invalid/allure"), checksum(), false, Set.of());
        SetupAction surrogateTwo = new SetupAction(SetupTarget.ALLURE, SetupActionKind.INSTALL, "\uD801",
                URI.create("https://example.invalid/allure"), checksum(), false, Set.of());
        assertNotEquals(SetupPlan.create(SetupPlatform.LINUX, SetupMode.MANAGED, List.of(surrogateOne)).digest(),
                SetupPlan.create(SetupPlatform.LINUX, SetupMode.MANAGED, List.of(surrogateTwo)).digest());
    }

    @Test
    void actionFailureCarriesPartialReceiptAndFailedAction() {
        SetupAction first = action(SetupTarget.ALLURE, "2.34.1");
        SetupAction failed = action(SetupTarget.SELENIUM_BROWSER, "stable");
        SetupPlan plan = SetupPlan.create(SetupPlatform.LINUX, SetupMode.MANAGED, List.of(first, failed));

        SetupExecutionException failure = assertThrows(SetupExecutionException.class,
                () -> SetupExecutor.execute(plan, approval(plan), action -> {
                    if (action.equals(failed)) throw new IllegalStateException("failed");
                }));

        assertEquals(failed, failure.failedAction());
        assertEquals(List.of(first), failure.partialReceipt().completedActions());
        assertEquals(plan.digest(), failure.partialReceipt().planDigest());
    }

    @Test
    void actionRejectsUnverifiableChecksum() {
        assertThrows(IllegalArgumentException.class, () -> new SetupAction(SetupTarget.ALLURE,
                SetupActionKind.INSTALL, "2.34.1", URI.create("https://example.invalid/allure"),
                "sha256:test", false, Set.of()));
    }

    @Test
    void staleApprovalIsRejectedBeforeAnyActionRuns() {
        SetupPlan approvedPlan = SetupPlan.create(SetupPlatform.LINUX, SetupMode.MANAGED,
                List.of(action(SetupTarget.ALLURE, "2.34.1")));
        SetupPlan changedPlan = SetupPlan.create(SetupPlatform.LINUX, SetupMode.MANAGED,
                List.of(action(SetupTarget.ALLURE, "2.35.0")));
        SetupApproval staleApproval = new SetupApproval(approvedPlan.digest(), Instant.EPOCH, Set.of());
        List<SetupAction> mutations = new ArrayList<>();

        assertThrows(StaleSetupApprovalException.class,
                () -> SetupExecutor.execute(changedPlan, staleApproval, mutations::add));
        assertEquals(List.of(), mutations);
    }

    @Test
    void matchingApprovalProducesImmutableReceiptInActionOrder() {
        SetupAction allure = action(SetupTarget.ALLURE, "2.34.1");
        SetupAction browser = action(SetupTarget.SELENIUM_BROWSER, "stable");
        SetupPlan plan = SetupPlan.create(SetupPlatform.LINUX, SetupMode.HYBRID, List.of(allure, browser));
        List<SetupAction> mutations = new ArrayList<>();

        SetupReceipt receipt = SetupExecutor.execute(plan,
                approval(plan), mutations::add);

        assertEquals(List.of(allure, browser), mutations);
        assertEquals(plan.digest(), receipt.planDigest());
        assertEquals(List.of(allure, browser), receipt.completedActions());
        assertThrows(UnsupportedOperationException.class, () -> receipt.completedActions().add(allure));

        SetupStatus status = new SetupStatus(SetupTarget.ALLURE, SetupReadiness.READY, null, null);
        assertEquals("", status.detectedVersion());
        assertEquals("", status.detail());
    }

    @Test
    void rejectsExternalMutationsPrivilegeSpoofingAndMissingLicenseBeforeMutation() {
        SetupAction install = action(SetupTarget.ALLURE, "2.34.1");
        assertThrows(IllegalArgumentException.class,
                () -> SetupPlan.create(SetupPlatform.LINUX, SetupMode.EXTERNAL, List.of(install)));
        assertThrows(IllegalArgumentException.class, () -> SetupPlan.create(SetupPlatform.LINUX,
                SetupMode.MANAGED, List.of(new SetupAction(SetupTarget.JAVA, SetupActionKind.CONFIGURE,
                        "25", URI.create("https://example.invalid/java"), checksum(), false, Set.of()))));
        assertThrows(IllegalArgumentException.class, () -> new SetupPlan(2, SetupPlatform.LINUX,
                SetupMode.MANAGED, List.of(install), "sha256:" + "0".repeat(64)));
        assertThrows(IllegalArgumentException.class, () -> SetupPlan.create(SetupPlatform.WINDOWS,
                SetupMode.MANAGED, List.of(new SetupAction(SetupTarget.WINAPPDRIVER, SetupActionKind.INSTALL,
                        "1.2.1", URI.create("https://example.invalid/wad"), checksum(), false, Set.of()))));

        SetupAction licensed = new SetupAction(SetupTarget.ALLURE, SetupActionKind.INSTALL, "2.34.1",
                URI.create("https://example.invalid/allure"), checksum(), false, Set.of("allure-eula"));
        SetupPlan plan = SetupPlan.create(SetupPlatform.LINUX, SetupMode.MANAGED, List.of(licensed));
        List<SetupAction> mutations = new ArrayList<>();
        assertThrows(IllegalArgumentException.class, () -> SetupExecutor.execute(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()), mutations::add));
        assertEquals(List.of(), mutations);
        SetupExecutor.execute(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of("allure-eula")),
                mutations::add);
        assertEquals(List.of(licensed), mutations);
    }

    private static SetupAction action(SetupTarget target, String version) {
        SetupActionKind kind = target == SetupTarget.SELENIUM_BROWSER
                ? SetupActionKind.PREWARM : SetupActionKind.INSTALL;
        return new SetupAction(target, kind, version,
                URI.create("https://example.invalid/" + target.name().toLowerCase()), checksum(), false, Set.of());
    }

    private static SetupApproval approval(SetupPlan plan) {
        return new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());
    }

    private static String checksum() {
        return "sha256:" + "0".repeat(64);
    }
}
