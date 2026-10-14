package com.shaft.tools.io.internal;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Scans a report for credential shapes in plaintext and in decoded base64 blobs.
 * A hit blocks publish. The secret value is not returned, logged, or written to the block file.
 * The scan streams the file and does not load the whole HTML as one string.
 */
public final class ReportPublishGuard {
    private static final int WINDOW = 65_536;
    private static final int MAX_BLOB = 65_536;
    private static final List<Shape> SHAPES = List.of(
            new Shape("api-token", Pattern.compile("sk-[A-Za-z0-9]{20,}")),
            new Shape("github-token", Pattern.compile("ghp_[A-Za-z0-9]{20,}")),
            new Shape("cloud-access-key", Pattern.compile("AKIA[0-9A-Z]{16}")),
            new Shape("bearer-token", Pattern.compile("(?i)bearer\\s+[A-Za-z0-9\\-._~+/]{16,}={0,2}")));

    private ReportPublishGuard() {
    }

    public static List<String> prepare(Path report) throws IOException {
        if (report == null || !Files.isRegularFile(report)) {
            return List.of();
        }
        List<Hit> hits = new ArrayList<>();
        try (InputStream input = Files.newInputStream(report)) {
            scan(input, hits);
        }
        if (hits.isEmpty()) {
            return List.of();
        }
        java.util.Map<Long, Hit> unique = new java.util.LinkedHashMap<>();
        for (Hit hit : hits) {
            unique.putIfAbsent(hit.offset(), hit);
        }
        try (var channel = java.nio.channels.FileChannel.open(report, java.nio.file.StandardOpenOption.WRITE)) {
            for (Hit hit : unique.values()) {
                if (hit.replacement().length == 0) {
                    continue;
                }
                channel.position(hit.offset());
                channel.write(java.nio.ByteBuffer.wrap(hit.replacement()));
            }
        }
        Set<String> classes = new LinkedHashSet<>();
        for (Hit hit : unique.values()) {
            for (String secretClass : hit.secretClass().split(",")) {
                if (!secretClass.isBlank()) {
                    classes.add(secretClass);
                }
            }
        }
        Path blocked = report.resolveSibling("PUBLISH_BLOCKED.txt");
        StringBuilder body = new StringBuilder("publish blocked");
        for (String secretClass : classes) {
            body.append(System.lineSeparator()).append(secretClass);
        }
        Files.writeString(blocked, body.toString());
        return List.copyOf(classes);
    }

    private static void scan(InputStream input, List<Hit> hits) throws IOException {
        byte[] window = new byte[WINDOW];
        byte[] pending = new byte[0];
        long pendingStart = 0;
        long absolute = 0;
        while (true) {
            int read = input.read(window);
            if (read < 0) {
                break;
            }
            byte[] chunk = new byte[pending.length + read];
            System.arraycopy(pending, 0, chunk, 0, pending.length);
            System.arraycopy(window, 0, chunk, pending.length, read);
            long chunkStart = pendingStart;
            String text = new String(chunk, StandardCharsets.ISO_8859_1);
            collectPlaintext(text, chunkStart, hits);
            collectBase64(text, chunk, chunkStart, hits);
            int carry = Math.min(chunk.length, MAX_BLOB);
            pending = new byte[carry];
            System.arraycopy(chunk, chunk.length - carry, pending, 0, carry);
            pendingStart = chunkStart + chunk.length - carry;
            absolute += read;
            if (absolute < 0) {
                break;
            }
        }
    }

    private static void collectPlaintext(String text, long chunkStart, List<Hit> hits) {
        for (Shape shape : SHAPES) {
            Matcher matcher = shape.pattern().matcher(text);
            while (matcher.find()) {
                if (insideDecodedBlob(text, matcher.start())) {
                    continue;
                }
                byte[] replacement = asterisks(matcher.end() - matcher.start());
                hits.add(new Hit(shape.secretClass(), chunkStart + matcher.start(), replacement));
            }
        }
    }

    private static void collectBase64(String text, byte[] chunk, long chunkStart, List<Hit> hits) {
        Matcher matcher = ReportBlobScan.tokens(text);
        while (matcher.find()) {
            int length = matcher.end() - matcher.start();
            if (!ReportBlobScan.atLeast(matcher, ReportBlobScan.PUBLISH_MINIMUM)
                    || length > MAX_BLOB
                    || imageContext(text, matcher.start())) {
                continue;
            }
            byte[] decoded;
            try {
                decoded = Base64.getDecoder().decode(matcher.group());
            } catch (IllegalArgumentException ignored) {
                continue;
            }
            String decodedText = new String(decoded, StandardCharsets.UTF_8);
            if (decodedText.indexOf('\uFFFD') >= 0) {
                continue;
            }
            boolean changed = false;
            StringBuilder redacted = new StringBuilder(decodedText);
            Set<String> classes = new LinkedHashSet<>();
            for (Shape shape : SHAPES) {
                Matcher secret = shape.pattern().matcher(redacted);
                StringBuffer replaced = new StringBuffer();
                boolean found = false;
                while (secret.find()) {
                    found = true;
                    classes.add(shape.secretClass());
                    secret.appendReplacement(replaced, Matcher.quoteReplacement("*".repeat(secret.end() - secret.start())));
                }
                if (found) {
                    secret.appendTail(replaced);
                    redacted = new StringBuilder(replaced.toString());
                    changed = true;
                }
            }
            if (!changed) {
                continue;
            }
            byte[] reencoded = Base64.getEncoder().encode(redacted.toString().getBytes(StandardCharsets.UTF_8));
            byte[] original = matcher.group().getBytes(StandardCharsets.US_ASCII);
            if (reencoded.length != original.length) {
                reencoded = asterisks(original.length);
            }
            hits.add(new Hit(String.join(",", classes), chunkStart + matcher.start(), reencoded));
        }
    }

    private static boolean imageContext(String text, int start) {
        int from = Math.max(0, start - 80);
        String context = text.substring(from, start).toLowerCase(java.util.Locale.ROOT);
        return context.contains("image/") || context.contains("data:image") || context.contains("video/");
    }

    private static boolean insideDecodedBlob(String text, int index) {
        Matcher matcher = ReportBlobScan.tokens(text);
        while (matcher.find()) {
            if (ReportBlobScan.atLeast(matcher, ReportBlobScan.PUBLISH_MINIMUM)
                    && matcher.start() <= index
                    && index < matcher.end()) {
                return true;
            }
        }
        return false;
    }

    private static byte[] asterisks(int length) {
        byte[] bytes = new byte[length];
        java.util.Arrays.fill(bytes, (byte) '*');
        return bytes;
    }

    private record Shape(String secretClass, Pattern pattern) {
    }

    private record Hit(String secretClass, long offset, byte[] replacement) {
    }
}
