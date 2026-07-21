package com.shaft.commandline.command;

import com.shaft.commandline.runtime.McpConnector;
import com.shaft.commandline.testsupport.InProcessMcp;
import org.junit.jupiter.api.Test;
import picocli.CommandLine;

import java.io.PrintWriter;
import java.io.StringWriter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Curated {@code capture} aliases must stay in sync with the 89-tool catalog (tracker #3866
 * T3/#3869): {@code capture_step_delete}/{@code capture_step_reorder} (W1 commit 3) had no curated
 * shortcut yet.
 */
class CaptureCommandTest {

    private final McpConnector connector = InProcessMcp.connector();

    @Test
    void stepDeleteActionResolvesToCaptureStepDeleteTool() {
        StringWriter out = new StringWriter();
        int exit = new CommandLine(new CaptureCommand(connector))
                .setOut(new PrintWriter(out, true))
                .execute("step-delete", "stepId=42");

        assertEquals(0, exit);
        assertTrue(out.toString().contains("called capture_step_delete"),
                "step-delete should dispatch to capture_step_delete");
    }

    @Test
    void stepReorderActionResolvesToCaptureStepReorderTool() {
        StringWriter out = new StringWriter();
        int exit = new CommandLine(new CaptureCommand(connector))
                .setOut(new PrintWriter(out, true))
                .execute("step-reorder", "stepId=42", "direction=up");

        assertEquals(0, exit);
        assertTrue(out.toString().contains("called capture_step_reorder"),
                "step-reorder should dispatch to capture_step_reorder");
    }

    @Test
    void unknownActionReturnsExitCodeTwoWithoutDispatching() {
        StringWriter err = new StringWriter();
        int exit = new CommandLine(new CaptureCommand(connector))
                .setErr(new PrintWriter(err, true))
                .execute("not-a-real-action");

        assertEquals(2, exit);
        assertTrue(err.toString().contains("Unknown capture action"), err.toString());
    }

    @Test
    void defaultConstructorUsesRealConnectionFactoryWithoutConnecting() {
        CaptureCommand command = new CaptureCommand();

        CommandLine.Model.CommandSpec spec = new CommandLine(command).getCommandSpec();
        assertEquals("capture", spec.name());
        assertEquals(2, spec.positionalParameters().size());
        assertEquals("ACTION", spec.positionalParameters().get(0).paramLabel());
    }
}
