package com.shaft.infrastructure;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SeekableByteChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.NoSuchFileException;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Set;

/** Bounded, no-follow reader for provider-owned UTF-8 log files. */
final class OwnedLogReader {
    static final long MAX_BYTES = 2L * 1024 * 1024;

    private OwnedLogReader() {
        throw new IllegalStateException("Utility class");
    }

    static String read(String label, Path path) throws IOException {
        VerifiedArtifactStore.requireUnlinkedAncestors(path);
        if (!Files.exists(path, LinkOption.NOFOLLOW_LINKS)) return "";
        if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException(label + " log is not an owned regular file: " + path);
        }
        Set<OpenOption> options = Set.of(StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS);
        try (SeekableByteChannel channel = Files.newByteChannel(path, options)) {
            if (channel.size() > MAX_BYTES) throw tooLarge(label, path);
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            ByteBuffer buffer = ByteBuffer.allocate(8192);
            while (channel.read(buffer) >= 0) {
                buffer.flip();
                if ((long) output.size() + buffer.remaining() > MAX_BYTES) throw tooLarge(label, path);
                output.write(buffer.array(), buffer.position(), buffer.remaining());
                buffer.clear();
            }
            return output.toString(StandardCharsets.UTF_8);
        } catch (NoSuchFileException disappeared) {
            return "";
        }
    }

    private static IOException tooLarge(String label, Path path) {
        return new IOException(label + " log exceeds the 2 MiB safety limit: " + path);
    }
}
