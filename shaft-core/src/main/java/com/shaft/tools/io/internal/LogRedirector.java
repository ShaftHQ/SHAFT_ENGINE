package com.shaft.tools.io.internal;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.Logger;

import java.io.OutputStream;
import java.util.Objects;

public class LogRedirector extends OutputStream {
    private final Logger logger;
    private final Level level;
    private StringBuilder stringBuilder;

    public LogRedirector(Logger logger, Level level) {
        this.logger = logger;
        this.level = level;
        stringBuilder = new StringBuilder();
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
            if (!stringBuilder.isEmpty()) {
                logger.log(level, stringBuilder.toString());
                stringBuilder = new StringBuilder();
            }
        } else
            stringBuilder.append(c);
    }
}
