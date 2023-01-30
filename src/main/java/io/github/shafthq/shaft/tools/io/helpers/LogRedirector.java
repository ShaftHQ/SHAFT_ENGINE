package io.github.shafthq.shaft.tools.io.helpers;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.io.OutputStream;

public class LogRedirector extends OutputStream {
    private final Logger logger;
    private final Level logLevel;
    private final OutputStream outputStream;

    public LogRedirector(Logger logger, Level logLevel, OutputStream outputStream) {
        super();

        this.logger = logger;
        this.logLevel = logLevel;
        this.outputStream = outputStream;
    }

    @Override
    public void write(byte[] b) throws IOException {
        outputStream.write(b);
        String string = new String(b);
        if (!string.trim().isEmpty())
            logger.log(logLevel, string);
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        outputStream.write(b, off, len);
        String string = new String(b, off, len);
        if (!string.trim().isEmpty())
            logger.log(logLevel, string);
    }

    @Override
    public void write(int b) throws IOException {
        outputStream.write(b);
        String string = String.valueOf((char) b);
        if (!string.trim().isEmpty())
            logger.log(logLevel, string);
    }
}
