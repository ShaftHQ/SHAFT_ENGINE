package com.shaft.intellij.ui;

import com.intellij.openapi.application.ApplicationManager;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Issue #4242: {@link LiveChatToolE2ESupport#install} calls {@code
 * ApplicationManager.setApplication(fakeApplication, applicationDisposable)}, and {@link
 * LiveChatToolE2ESupport#close} disposed {@code applicationDisposable} alone, on the same incorrect
 * "fully reversible" premise the class-level javadoc used to state. Decompiling {@code
 * ApplicationManager.class} shows the two-arg overload's restore-on-dispose callback is {@code
 * previous -> { if (previous != null) setApplication(previous); }} -- it only reinstates a NON-null
 * previous {@code Application}, so it silently no-ops here: this whole Gradle test JVM has no live
 * {@code Application} (i.e. {@code ApplicationManager.getApplication() == null}) before {@link
 * LiveChatToolE2ESupport#install} runs, exactly the gap PR #4238/#4239 already fixed for {@code
 * AssistantTranscriptViewTest}. Every real caller of this fixture is gated behind
 * {@code -Dshaft.intellij.liveToolE2E=true}-style flags never exercised by the normal PR-gate CI
 * run, so this test exercises the lifecycle directly and unconditionally instead, with no live
 * flags or MCP credentials required: {@code install}'s own constructor work (fake {@code
 * Application}/{@code Project} plus a {@code ShaftMcpInvocationService} that spawns its MCP server
 * process lazily, only on first tool call -- see that class's constructor) never touches a real
 * process, so {@code close()} can be asserted against with no {@link
 * LiveChatToolE2ESupport#send} ever invoked.
 */
class LiveChatToolE2ESupportTest {
    @Test
    void closeRestoresApplicationManagerToNullEvenThoughNoneExistedBeforeInstall(@TempDir Path workspace) {
        LiveChatToolE2ESupport fixture = LiveChatToolE2ESupport.install(workspace, "\"java\" \"-version\"");

        fixture.close();

        assertNull(ApplicationManager.getApplication(),
                "close() must not leak the fake Application into later tests sharing this JVM fork");
    }
}
