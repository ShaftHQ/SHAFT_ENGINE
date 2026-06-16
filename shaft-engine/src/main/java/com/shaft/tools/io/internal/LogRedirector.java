package com.shaft.tools.io.internal;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.Logger;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Objects;

public class LogRedirector extends OutputStream {
    private final Logger logger;
    private final Level level;
    private final ThreadLocal<ByteArrayOutputStream> lineBuffers =
            ThreadLocal.withInitial(ByteArrayOutputStream::new);

    public LogRedirector(Logger logger, Level level) {
        this.logger = logger;
        this.level = level;
    }

    @Override
    public void write(byte[] b) {
        write(b, 0, b.length);
    }

    @Override
    public void write(byte[] b, int off, int len) {
        Objects.checkFromIndexSize(off, len, b.length);
        // len == 0 condition implicitly handled by loop bounds
        for (int i = 0; i < len; i++) {
            write(b[off + i]);
        }
    }

    @Override
    public void write(int b) {
        char c = (char) b;
        if (c == '\r' || c == '\n') {
            flush();
        } else {
            lineBuffers.get().write(b);
        }
    }

    @Override
    public void flush() {
        ByteArrayOutputStream currentThreadBuffer = lineBuffers.get();
        if (currentThreadBuffer.size() > 0) {
            logger.log(level, currentThreadBuffer.toString(StandardCharsets.UTF_8));
            currentThreadBuffer.reset();
        }
    }

    @Override
    public void close() {
        flush();
        lineBuffers.remove();
    }
}
