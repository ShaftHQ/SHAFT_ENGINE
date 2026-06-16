package testPackage.unitTests;

import com.shaft.tools.io.internal.LogRedirector;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Unit tests for {@link LogRedirector}.
 * Verifies that bytes written to the stream are forwarded to the backing {@link Logger}
 * and that line boundaries are handled correctly.
 *
 * <p>The tests use a real Log4j2 logger obtained via {@link LogManager#getLogger(Class)}.
 * Because Log4j2 is already on the classpath, no mocking framework is needed.
 */
public class LogRedirectorUnitTest {

    private LogRedirector redirector;

    @BeforeMethod
    public void setUp() {
        Logger logger = LogManager.getLogger(LogRedirectorUnitTest.class);
        redirector = new LogRedirector(logger, Level.DEBUG);
    }

    // ─── write(int) ───────────────────────────────────────────────────────────

    @Test(description = "write(int): single printable character does not throw")
    public void writeSingleCharDoesNotThrow() {
        redirector.write('A');
        // No assertion needed — method must complete without exception
    }

    @Test(description = "write(int): newline character flushes without exception")
    public void writeNewlineCharFlushesWithoutException() {
        redirector.write('H');
        redirector.write('i');
        redirector.write('\n'); // should trigger a logger call
    }

    @Test(description = "write(int): carriage-return flushes without exception")
    public void writeCarriageReturnFlushesWithoutException() {
        redirector.write('O');
        redirector.write('K');
        redirector.write('\r'); // should trigger a logger call
    }

    @Test(description = "write(int): multiple lines written sequentially does not throw")
    public void writeMultipleLinesDoesNotThrow() {
        byte[] text = "line one\nline two\nline three\n".getBytes(StandardCharsets.UTF_8);
        for (byte b : text) {
            redirector.write(b);
        }
    }

    @Test(description = "write(int): empty line (consecutive newlines) does not flush empty string")
    public void writeConsecutiveNewlinesDoesNotThrow() {
        redirector.write('\n');
        redirector.write('\n');
        redirector.write('\n');
    }

    // ─── write(byte[]) ────────────────────────────────────────────────────────

    @Test(description = "write(byte[]): full array written without exception")
    public void writeByteArrayDoesNotThrow() {
        byte[] data = "hello from byte array\n".getBytes(StandardCharsets.UTF_8);
        redirector.write(data);
    }

    @Test(description = "write(byte[]): empty array does not throw")
    public void writeEmptyByteArrayDoesNotThrow() {
        redirector.write(new byte[0]);
    }

    @Test(description = "write(byte[]): array without terminator accumulates content silently")
    public void writeByteArrayWithoutNewlineDoesNotThrow() {
        byte[] data = "no newline here".getBytes(StandardCharsets.UTF_8);
        redirector.write(data);
        // content sits in the internal buffer; no exception expected
    }

    // ─── write(byte[], off, len) ──────────────────────────────────────────────

    @Test(description = "write(byte[], off, len): partial array written without exception")
    public void writePartialByteArrayDoesNotThrow() {
        byte[] data = "XYZABC\n".getBytes(StandardCharsets.UTF_8);
        // Write only "ABC\n" (offset 3, length 4)
        redirector.write(data, 3, 4);
    }

    @Test(description = "write(byte[], off, len): zero-length write does not throw")
    public void writeZeroLengthDoesNotThrow() {
        byte[] data = "anything".getBytes(StandardCharsets.UTF_8);
        redirector.write(data, 0, 0);
    }

    @Test(description = "write(byte[], off, len): out-of-bounds throws IndexOutOfBoundsException")
    public void writeWithOutOfBoundsThrowsException() {
        byte[] data = "short".getBytes(StandardCharsets.UTF_8);
        Assert.assertThrows(IndexOutOfBoundsException.class,
                () -> redirector.write(data, 0, data.length + 10));
    }

    // ─── multi-line round-trip ────────────────────────────────────────────────

    @Test(description = "write: mixed single-byte and multi-line writes do not throw")
    public void mixedWriteModesDoNotThrow() {
        // Combine all three write overloads in sequence
        redirector.write('S');
        redirector.write((int) 't');
        redirector.write("art\n".getBytes(StandardCharsets.UTF_8));
        byte[] more = "second line\nthird line\n".getBytes(StandardCharsets.UTF_8);
        redirector.write(more, 0, more.length);
    }

    @Test(description = "write: partial lines should remain isolated between concurrent threads")
    public void partialLinesShouldStayIsolatedPerThread() throws Exception {
        Logger logger = Mockito.mock(Logger.class);
        LogRedirector sharedRedirector = new LogRedirector(logger, Level.INFO);
        CountDownLatch alphaBuffered = new CountDownLatch(1);
        CountDownLatch betaFlushed = new CountDownLatch(1);
        AtomicReference<Throwable> failure = new AtomicReference<>();

        Thread alpha = new Thread(() -> {
            try {
                sharedRedirector.write("alpha-مرحبا".getBytes(StandardCharsets.UTF_8));
                alphaBuffered.countDown();
                if (!betaFlushed.await(5, TimeUnit.SECONDS)) {
                    throw new AssertionError("Timed out waiting for beta line to flush");
                }
                sharedRedirector.write('\n');
            } catch (Throwable throwable) {
                failure.compareAndSet(null, throwable);
            }
        }, "redirect-alpha");

        Thread beta = new Thread(() -> {
            try {
                if (!alphaBuffered.await(5, TimeUnit.SECONDS)) {
                    throw new AssertionError("Timed out waiting for alpha line to buffer");
                }
                sharedRedirector.write("beta-世界\n".getBytes(StandardCharsets.UTF_8));
                betaFlushed.countDown();
            } catch (Throwable throwable) {
                failure.compareAndSet(null, throwable);
                betaFlushed.countDown();
            }
        }, "redirect-beta");

        alpha.start();
        beta.start();
        alpha.join(5_000);
        beta.join(5_000);

        Assert.assertFalse(alpha.isAlive(), "Alpha redirect thread should finish");
        Assert.assertFalse(beta.isAlive(), "Beta redirect thread should finish");
        if (failure.get() != null) {
            throw new AssertionError("Concurrent redirector write failed", failure.get());
        }

        ArgumentCaptor<String> messageCaptor = ArgumentCaptor.forClass(String.class);
        Mockito.verify(logger, Mockito.times(2)).log(Mockito.eq(Level.INFO), messageCaptor.capture());
        Assert.assertTrue(messageCaptor.getAllValues().contains("alpha-مرحبا"));
        Assert.assertTrue(messageCaptor.getAllValues().contains("beta-世界"));
    }
}
