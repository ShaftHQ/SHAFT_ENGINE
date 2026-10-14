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
        List<Hit> hits = scanReport(report);
        if (hits.isEmpty()) {
            return List.of();
        }
        List<Hit> unique = uniqueHits(hits);
        redact(report, unique);
        return writeBlock(report, unique);
    }

    private static List<Hit> scanReport(Path report) throws IOException {
        List<Hit> hits = new ArrayList<>();
        try (InputStream input = Files.newInputStream(report)) {
            scan(input, hits);
        }
        return hits;
    }

    private static List<Hit> uniqueHits(List<Hit> hits) {
        java.util.Map<Long, Hit> unique = new java.util.LinkedHashMap<>();
        for (Hit hit : hits) {
            unique.putIfAbsent(hit.offset(), hit);
        }
        return List.copyOf(unique.values());
    }

    private static void redact(Path report, List<Hit> hits) throws IOException {
        try (var channel = java.nio.channels.FileChannel.open(report, java.nio.file.StandardOpenOption.WRITE)) {
            for (Hit hit : hits) {
                writeReplacement(channel, hit);
            }
        }
    }

    private static void writeReplacement(java.nio.channels.FileChannel channel, Hit hit) throws IOException {
        if (hit.replacement().length == 0) {
            return;
        }
        channel.position(hit.offset());
        channel.write(java.nio.ByteBuffer.wrap(hit.replacement()));
    }

    private static List<String> writeBlock(Path report, List<Hit> hits) throws IOException {
        Set<String> classes = secretClasses(hits);
        Path blocked = report.resolveSibling("PUBLISH_BLOCKED.txt");
        Files.writeString(blocked, blockBody(classes));
        return List.copyOf(classes);
    }

    private static Set<String> secretClasses(List<Hit> hits) {
        Set<String> classes = new LinkedHashSet<>();
        for (Hit hit : hits) {
            addClasses(classes, hit.secretClass());
        }
        return classes;
    }

    private static void addClasses(Set<String> classes, String secretClass) {
        for (String name : secretClass.split(",")) {
            if (!name.isBlank()) {
                classes.add(name);
            }
        }
    }

    private static String blockBody(Set<String> classes) {
        StringBuilder body = new StringBuilder("publish blocked");
        for (String secretClass : classes) {
            body.append(System.lineSeparator()).append(secretClass);
        }
        return body.toString();
    }

    private static void scan(InputStream input, List<Hit> hits) throws IOException {
        byte[] slide = new byte[WINDOW + MAX_BLOB];
        ReportBlobScan.Latin1Window text = new ReportBlobScan.Latin1Window(slide);
        Matcher tokens = ReportBlobScan.tokens(text);
        Matcher blobs = ReportBlobScan.tokens(text);
        Matcher[] plain = new Matcher[SHAPES.size()];
        for (int i = 0; i < SHAPES.size(); i++) {
            plain[i] = SHAPES.get(i).pattern().matcher(text);
        }
        int slideLen = 0;
        long filePos = 0;
        while (true) {
            int read = input.read(slide, slideLen, WINDOW);
            if (read < 0) {
                break;
            }
            slideLen += read;
            text.setLength(slideLen);
            collectPlaintext(plain, blobs, filePos, hits);
            collectBase64(text, tokens, filePos, hits);
            int keep = Math.min(slideLen, MAX_BLOB);
            int discard = slideLen - keep;
            if (discard > 0) {
                System.arraycopy(slide, discard, slide, 0, keep);
                slideLen = keep;
                filePos += discard;
            }
        }
    }

    private static void collectPlaintext(Matcher[] plain, Matcher blobs, long chunkStart, List<Hit> hits) {
        for (int i = 0; i < plain.length; i++) {
            Matcher matcher = plain[i];
            matcher.reset();
            while (matcher.find()) {
                if (insideDecodedBlob(blobs, matcher.start())) {
                    continue;
                }
                byte[] replacement = asterisks(matcher.end() - matcher.start());
                hits.add(new Hit(SHAPES.get(i).secretClass(), chunkStart + matcher.start(), replacement));
            }
        }
    }

    private static void collectBase64(CharSequence text, Matcher matcher, long chunkStart, List<Hit> hits) {
        matcher.reset();
        while (matcher.find()) {
            Hit hit = decodedHit(text, matcher, chunkStart);
            if (hit != null) {
                hits.add(hit);
            }
        }
    }

    private static Hit decodedHit(CharSequence text, Matcher matcher, long chunkStart) {
        if (!publishableToken(text, matcher)) {
            return null;
        }
        String decodedText = decodeUtf8(matcher.group());
        if (decodedText == null) {
            return null;
        }
        Redaction redaction = redactSecrets(decodedText);
        if (redaction == null) {
            return null;
        }
        byte[] original = matcher.group().getBytes(StandardCharsets.US_ASCII);
        byte[] reencoded = reencode(redaction.text(), original.length);
        return new Hit(String.join(",", redaction.classes()), chunkStart + matcher.start(), reencoded);
    }

    private static boolean publishableToken(CharSequence text, Matcher matcher) {
        int length = matcher.end() - matcher.start();
        if (!ReportBlobScan.atLeast(matcher, ReportBlobScan.PUBLISH_MINIMUM)) {
            return false;
        }
        if (length > MAX_BLOB) {
            return false;
        }
        return !imageContext(text, matcher.start());
    }

    private static String decodeUtf8(String token) {
        byte[] decoded;
        try {
            decoded = Base64.getDecoder().decode(token);
        } catch (IllegalArgumentException ignored) {
            return null;
        }
        String decodedText = new String(decoded, StandardCharsets.UTF_8);
        if (decodedText.indexOf('\uFFFD') >= 0) {
            return null;
        }
        return decodedText;
    }

    private static Redaction redactSecrets(String decodedText) {
        boolean changed = false;
        StringBuilder redacted = new StringBuilder(decodedText);
        Set<String> classes = new LinkedHashSet<>();
        for (Shape shape : SHAPES) {
            String next = replaceShape(shape, redacted, classes);
            if (next != null) {
                redacted = new StringBuilder(next);
                changed = true;
            }
        }
        if (!changed) {
            return null;
        }
        return new Redaction(redacted.toString(), classes);
    }

    private static String replaceShape(Shape shape, StringBuilder redacted, Set<String> classes) {
        Matcher secret = shape.pattern().matcher(redacted);
        StringBuffer replaced = new StringBuffer();
        boolean found = false;
        while (secret.find()) {
            found = true;
            classes.add(shape.secretClass());
            secret.appendReplacement(replaced, Matcher.quoteReplacement("*".repeat(secret.end() - secret.start())));
        }
        if (!found) {
            return null;
        }
        secret.appendTail(replaced);
        return replaced.toString();
    }

    private static byte[] reencode(String text, int originalLength) {
        byte[] reencoded = Base64.getEncoder().encode(text.getBytes(StandardCharsets.UTF_8));
        if (reencoded.length != originalLength) {
            return asterisks(originalLength);
        }
        return reencoded;
    }

    private static boolean imageContext(CharSequence text, int start) {
        int from = Math.max(0, start - 80);
        StringBuilder context = new StringBuilder(start - from);
        for (int i = from; i < start; i++) {
            context.append(Character.toLowerCase(text.charAt(i)));
        }
        String lower = context.toString();
        return lower.contains("image/") || lower.contains("data:image") || lower.contains("video/");
    }

    private static boolean insideDecodedBlob(Matcher blobs, int index) {
        blobs.reset();
        while (blobs.find()) {
            if (ReportBlobScan.atLeast(blobs, ReportBlobScan.PUBLISH_MINIMUM)
                    && blobs.start() <= index
                    && index < blobs.end()) {
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

    private record Redaction(String text, Set<String> classes) {
    }

    private record Hit(String secretClass, long offset, byte[] replacement) {
    }
}
