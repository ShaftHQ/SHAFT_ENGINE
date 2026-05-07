package testPackage.unitTests;

import com.epam.reportportal.service.ReportPortal;
import com.shaft.listeners.TestNGListener;
import com.shaft.tools.io.internal.AttachmentReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.mockito.MockedStatic;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Date;
import java.util.stream.Stream;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

/**
 * Verifies that attachment-related IO paths close {@link InputStream}s and that
 * ReportPortal temp files are deleted after emit (no cumulative {@code rp-test*} leaks).
 *
 * @see <a href="https://shafthq.github.io/">SHAFT User Guide</a>
 */
public class ReportManagerHelperAttachmentIoUnitTest {

    private static final class CloseTrackingInputStream extends InputStream {
        private final InputStream delegate;
        private volatile boolean closed;

        CloseTrackingInputStream(InputStream delegate) {
            this.delegate = delegate;
        }

        @Override
        public int read() throws IOException {
            return delegate.read();
        }

        @Override
        public void close() throws IOException {
            closed = true;
            delegate.close();
        }

        boolean isClosed() {
            return closed;
        }
    }

    private static long countRpTestFilesInTempDir() throws IOException {
        Path tmp = Path.of(System.getProperty("java.io.tmpdir"));
        if (!Files.isDirectory(tmp)) {
            return 0L;
        }
        try (Stream<Path> stream = Files.list(tmp)) {
            return stream
                    .map(p -> p.getFileName().toString())
                    .filter(name -> name.startsWith("rp-test"))
                    .count();
        }
    }

    /**
     * {@link ReportManagerHelper} must close attachment streams after copying bytes so
     * parallel suites do not leak file descriptors from {@link java.io.FileInputStream}.
     */
    @Test
    public void shouldCloseInputStreamAfterCreateAttachment() throws Exception {
        CloseTrackingInputStream tracking = new CloseTrackingInputStream(
                new ByteArrayInputStream("attachment-payload".getBytes(StandardCharsets.UTF_8)));

        Method m = ReportManagerHelper.class.getDeclaredMethod(
                "createAttachment", String.class, String.class, InputStream.class);
        m.setAccessible(true);
        m.invoke(null, "Unit Test Attachment", "stream-closure.txt", tracking);

        assertTrue(tracking.isClosed(), "createAttachment must close the supplied InputStream");
    }

    /**
     * When ReportPortal is enabled, {@link AttachmentReporter} writes a temp file for
     * {@code emitLog}; it must be deleted (or scheduled for delete) so CI agents do not
     * accumulate garbage under {@code java.io.tmpdir}.
     */
    @Test
    public void shouldNotLeaveReportPortalTempFilesBehindWhenEmitSucceeds() throws Exception {
        long before = countRpTestFilesInTempDir();

        ByteArrayOutputStream content = new ByteArrayOutputStream();
        content.write("sample".getBytes(StandardCharsets.UTF_8));

        try (MockedStatic<TestNGListener> tl = mockStatic(TestNGListener.class);
             MockedStatic<ReportPortal> rp = mockStatic(ReportPortal.class)) {
            tl.when(TestNGListener::isReportPortalEnabled).thenReturn(true);
            rp.when(() -> ReportPortal.emitLog(anyString(), anyString(), any(Date.class), any(File.class)))
                    .thenAnswer(invocation -> null);

            AttachmentReporter.attachBasedOnFileType(
                    "screenshot", "s.png", content, "screenshot - s.png");
        }

        long after = countRpTestFilesInTempDir();
        assertEquals(after, before,
                "rp-test* temp file count should not increase after ReportPortal attachment");
    }
}
