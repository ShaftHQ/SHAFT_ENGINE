package com.shaft.capture.runtime;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/**
 * Cross-process lock that permits one managed capture session per runtime directory.
 */
final class CaptureSingleSessionLock implements AutoCloseable {
    private final FileChannel channel;
    private final FileLock lock;

    private CaptureSingleSessionLock(FileChannel channel, FileLock lock) {
        this.channel = channel;
        this.lock = lock;
    }

    static CaptureSingleSessionLock acquire(Path runtimeDirectory) {
        try {
            Files.createDirectories(runtimeDirectory);
            FileChannel channel = FileChannel.open(
                    runtimeDirectory.resolve("capture.lock"),
                    StandardOpenOption.CREATE,
                    StandardOpenOption.WRITE);
            FileLock lock;
            try {
                lock = channel.tryLock();
            } catch (OverlappingFileLockException exception) {
                lock = null;
            }
            if (lock == null) {
                channel.close();
                throw new IllegalStateException(
                        "Another SHAFT Capture session is already active for this runtime directory.");
            }
            return new CaptureSingleSessionLock(channel, lock);
        } catch (IOException exception) {
            throw new IllegalStateException("SHAFT Capture could not acquire its local session lock.", exception);
        }
    }

    @Override
    public void close() {
        try {
            lock.release();
        } catch (IOException ignored) {
            // Closing the channel below also releases the process lock.
        }
        try {
            channel.close();
        } catch (IOException ignored) {
            // Best-effort local lock cleanup.
        }
    }
}
