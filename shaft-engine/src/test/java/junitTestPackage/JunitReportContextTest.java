package junitTestPackage;

import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.tools.io.internal.ReportContext;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class JunitReportContextTest {
    @AfterEach
    void cleanup() {
        ReportContext.clear();
    }

    @Test
    void reportManagerHelperShouldReadCurrentTestFromReportContext() {
        ReportContext.start(new TestExecutionInfo("id", "fixture.SampleTest", "sampleMethod",
                "sample display", "sample description", null, null, false));

        assertEquals("fixture.SampleTest", ReportManagerHelper.getTestClassName());
        assertEquals("sampleMethod", ReportManagerHelper.getTestMethodName());
    }

    @Test
    void reportManagerHelperShouldWriteLogsToReportContextSink() {
        List<String> sink = new ArrayList<>();
        ReportContext.setLogSink(sink::add);

        ReportManagerHelper.createLogEntry("neutral reporter log", Level.INFO);

        assertTrue(ReportContext.snapshotOutput().stream().anyMatch(log -> log.contains("neutral reporter log")));
        assertTrue(sink.stream().anyMatch(log -> log.contains("neutral reporter log")));
    }

    @Test
    void reportContextShouldRecordAndClearAttachmentManifest() {
        ReportContext.start(new TestExecutionInfo("id", "fixture.SampleTest", "sampleMethod",
                "sample display", "sample description", null, null, false));

        ReportContext.recordAttachment("Screenshot - failure", "image/png", ".png", "screenshot", 42);

        List<ReportContext.AttachmentRecord> attachments = ReportContext.snapshotAttachments();
        assertEquals(1, attachments.size());
        assertEquals("Screenshot - failure", attachments.getFirst().description());
        assertEquals("screenshot", attachments.getFirst().purpose());
        assertEquals(42, attachments.getFirst().sizeBytes());

        ReportContext.clear();

        assertTrue(ReportContext.snapshotAttachments().isEmpty());
    }
}
